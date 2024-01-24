publications_summary <- function(df){
  
  n_unique_outputs <- n_distinct(df$publication_id)
  
  n_outputs_currently_affiliated <- df %>%
    filter(current_affiliation == "University of Bradford") %>%
    summarise(n = n_distinct(publication_id)) %>%
    unlist %>%
    unname
  
  df_outputs_by_type <- df %>%
    distinct(publication_id, year, type) %>%
    group_by(type) %>%
    tally() %>%
    ungroup() %>%
    mutate(percent = round(100 * n/sum(n), 1))  
  
  df_output_volume <- df %>%
    distinct(publication_id, year, type) %>%
    group_by(year, type) %>%
    tally() %>%
    ungroup() %>%
    complete(year, type, fill = list(n = 0)) %>%
    mutate(type = str_to_title(type))
  
  df_output_growth <- df_output_volume %>%
    group_by(year) %>%
    summarise(total = sum(n)) %>%
    ungroup() %>%
    mutate(cumulative_outputs = cumsum(total),
           pct_change = round(percent_change(lag(total), total), 1),
           fill = sign(pct_change))
  
  p_increase_outputs <- round(percent_change(df_output_growth$total[which.min(df_output_growth$year)],
                                             df_output_growth$total[which.max(df_output_growth$year)]), 1)
  
  output_list <- list("n_unique_outputs" = n_unique_outputs,
                      "n_outputs_currently_affiliated" = n_outputs_currently_affiliated,
                      "df_outputs_by_type" = df_outputs_by_type,
                      "df_output_volume" = df_output_volume,
                      "df_output_growth" = df_output_growth,
                      "p_increase_outputs" = p_increase_outputs)
  
}

author_positions_summary <- function(df, df_publications) {
  
  df_articles <- df_publications %>%
    filter(type == "article") %>%
    distinct(publication_id, year) %>%
    group_by(year) %>%
    tally(n = "total") %>%
    ungroup()
  
  outputs_per_year_corresponding <- df_author_positions %>%
    distinct(corresponding, pub_id, open_access, year) %>%
    filter(corresponding == TRUE) %>%
    group_by(year) %>%
    tally(n = "total_corresponding") %>%
    ungroup() %>%
    inner_join(., df_articles, by = "year") %>% 
    adorn_totals() %>%
    mutate(p_corresponding = round(100 * total_corresponding/total, 1)) %>%
    select(-total)
  
  author_pos <- df_author_positions %>%
    filter(!is.na(AuthorCategory)) %>%
    distinct(pub_id, year, AuthorCategory) %>%
    group_by(year, AuthorCategory) %>% 
    tally() %>%
    ungroup() %>%
    pivot_wider(id_cols = year, names_from = "AuthorCategory", values_from = "n") %>%
    inner_join(., df_articles, by = "year") %>% 
    adorn_totals() %>%
    mutate(p_first = round(100 * first_author/total, 1),
           p_last = round(100 * last_author/total, 1)) %>%
    relocate(total, .after = year) %>%
    relocate(p_first,  .after = first_author)
  
  inner_join(author_pos, outputs_per_year_corresponding, by = "year") %>% 
    tibble () %>%
    mutate(across(where(is.double), .fns = function(x) {format(x, nsmall = 1)}))
  
}

chart_output_affiliated <- function(df, discrete_pal, min_year, max_year){
  
  df %>%
    filter(current_affiliation == "University of Bradford") %>%
    distinct(publication_id, year) %>%
    group_by(year) %>%
    tally() %>%
    ungroup() %>%
    ggplot(aes(x = year, y = n, text = paste0("Outputs: ", n))) +
    geom_bar(stat = "identity", fill = discrete_pal[9]) +
    scale_x_continuous(name = "Year",
                       breaks = seq(min_year, max_year, 1), 
                       labels = seq(min_year, max_year, 1)) +
    scale_y_continuous(name = "Outputs") +
    theme_light() +
    theme(axis.line.x = element_line(colour = "gray"),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          text = element_text(size = 10))
}

chart_output_volume <- function(df, discrete_pal, min_year, max_year){
  
  max_value <- df_output_volume %>%
    group_by(year) %>%
    summarise(total = sum(n)) %>%
    ungroup() %>%
    select(total) %>%
    max() %>%
    plyr::round_any(., 50, f = ceiling)
    
  
  ggplot(data = df, 
         aes(x = year, y = n, fill = type,
             text = paste0("Type: ", type, "<br>",
                           "Outputs: ", n))) +
    geom_bar(stat = "identity", position = "stack") +
    scale_x_continuous(name = "Year", 
                       breaks = seq(min_year, max_year, 1), 
                       labels = seq(min_year, max_year, 1)) +
    scale_y_continuous(name = "Outputs",
                       breaks = seq(0, max_value, 50),
                       labels = seq(0, max_value, 50)) +
    scale_fill_manual(name = "Output types", values = rev(discrete_pal),
                      guide = guide_legend(title.position = "top")) +
    my_theme() +
    theme(legend.position = "right")
  
}

chart_percentage_change <- function(df, discrete_pal, min_year, max_year){
  
  df %>%
    filter(!is.na(pct_change)) %>%
    ggplot(aes(x = year, y = pct_change, fill = factor(fill),
               text = paste0("Change: ", pct_change, "%")
    )) +
    geom_bar(stat = "identity") +
    scale_x_continuous(name = "Year",
                       breaks = seq(min_year, max_year, 1)) +
    scale_y_continuous(name = "Annual change in outputs",
                       breaks = seq(-100, 100, 10),
                       labels = function(x) scales::percent(x/100)) +
    scale_fill_manual(name = NULL, values = discrete_pal[c(4, 9)]) +
    my_theme() +
    theme(legend.position = "none")
  
}

chart_total_pubs <- function(df, discrete_pal, min_year, max_year){
  
  df %>%
    ggplot(aes(x = year, y = total)) +
    geom_bar(stat = "identity", fill = discrete_pal[9]) +
    geom_text(aes(x = year, y = total, label = total), size = 3.5, vjust = -0.5) +
    scale_x_continuous(name = "Year",
                       breaks = seq(min_year, max_year, 1)) +
    scale_y_continuous(name = "Outputs") +
    my_theme() +
    theme(legend.position = "none")
  
}

chart_cumulative_growth <- function(df, discrete_pal, min_year, max_year){
  
  ggplot(data = df, 
         aes(x = year, y = cumulative_outputs,
         )) +
    geom_area(fill = discrete_pal[9], alpha = 0.6) +
    geom_line(colour = discrete_pal[9], linewidth = 1) +
    geom_point(colour = discrete_pal[9],
               aes(text = paste0("Year: ", year, "<br>",
                                 "Outputs: ", total, "<br>",
                                 "Cumulative outputs: ", cumulative_outputs))) +
    scale_x_continuous(name = "Year",
                       breaks = seq(min_year, max_year, 1), 
                       labels = seq(min_year, max_year, 1)) +
    scale_y_continuous(name = "Outputs") +
    my_theme()
  
}

open_access_summary <- function(df){
  
  open_access_data <- df %>%
    filter(type == "article") %>%
    select(publication_id, year, open_access, times_cited, field_citation_ratio) %>%
    distinct(publication_id, year, open_access, times_cited, field_citation_ratio) %>%
    mutate(open_access = str_to_title(open_access))
  
  open_access_data$open_access <- factor(open_access_data$open_access, 
                                         levels = c("Bronze",
                                                    "Gold", 
                                                    "Green", 
                                                    "Hybrid",
                                                    "Closed"))
  
  open_access_count <- open_access_data %>%
    count(open_access) %>%
    rename(count = n) %>%
    mutate(prop = round(count/sum(count), 3),
           cum_count = rev(cumsum(rev(count))),
           pos = count/2 + lead(cum_count, 1),
           pos = if_else(is.na(pos), count/2, pos),
           colour = discrete_pal[c(9,8,7,5,3)],
           l = decode_colour(colour, to = "hcl")[,"l"],
           label_colours = ifelse(l < 55, "white", "black"))
  
  p_open_access <- open_access_count %>% 
    filter(open_access != "Closed") %>% 
    summarise(p = round(100 * sum(prop), 1)) %>% 
    unlist %>% 
    unname
  
  p_closed_open_access <- ceiling(round(100 * open_access_count$prop[open_access_count$open_access == "Closed"], 1))
  
  p_gold_open_access <- plyr::round_any(100 * open_access_count$prop[open_access_count$open_access == "Gold"], 10, f = floor)  
  
  output_list <- list("open_access_data" = open_access_data,
                      "open_access_count" = open_access_count,
                      "p_open_access" = p_open_access,
                      "p_closed_open_access" = p_closed_open_access,
                      "p_gold_open_access" = p_gold_open_access)
  
}

open_access_donut <- function(df, discrete_pal){
  
  ggplot(data = df, aes(x = 2, y = count, fill = open_access)) +
    geom_col(colour = "black", linewidth = 0.5) +
    geom_label_repel(aes(y = pos,
                         label = glue("{open_access} ({scales::percent(prop)})"), 
                         fill = open_access),
                     size = 3.5,
                     nudge_x = 1,
                     colour = df$label_colours,
                     show.legend = FALSE) +
    scale_x_discrete(name = "Open access type") +
    scale_y_continuous(name = "Frequency") +
    scale_fill_manual(name = NULL, values = df$colour) +
    coord_polar(theta = "y", start = 0, direction = -1) +
    xlim(0.1, 3) +
    theme_void() +
    theme(legend.position = "none")
  
}

open_access_trend <- function(df, discrete_pal){
  
  oa_dat <- df %>%
    group_by(year, open_access) %>%
    tally() %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(p = round(100 * n/sum(n), 1)) %>%
    ungroup()
  
  outputs_per_year <- oa_dat %>%
    group_by(year) %>%
    summarise(total = sum(n)) %>%
    ungroup()
  
  ggplot() +
    geom_text(data = outputs_per_year, aes(x = year, y = 105, label = total), size = 3.5) +
    geom_bar(data = oa_dat, aes(x = year, y = p, fill = open_access,
                                text = paste0(open_access, ": ", p, "%",
                                              "\nOutputs: ", n)), 
             stat = "identity", position = "stack") +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name = NULL,
                       breaks = seq(0, 100, 20), 
                       minor_breaks = seq(10, 90, 10),
                       labels = scales::percent_format(scale = 1)) +
    scale_fill_manual(name = "OA status", 
                      guide = (guide_legend(title.position = "top")),
                      values = discrete_pal[c(9,8,7,5,3)]) +
    theme_minimal() +
    theme(axis.text.x = element_text(vjust = 1),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "bottom", 
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
}

corresponding_oa_status <- function(df){
  
  df %>%
    distinct(corresponding, pub_id, open_access, year) %>%
    filter(corresponding == TRUE) %>%
    group_by(year, open_access) %>%
    tally() %>%
    ungroup() %>%
    mutate(open_access = str_to_title(open_access)) %>%
    group_by(year) %>%
    mutate(p = round(100 * n/sum(n), 1)) %>%
    ungroup()
  
}

corresponding_oa_proportions <- function(df) {
  
  df_all <- df %>%
    group_by(year) %>%
    summarise(total_corr_n = sum(n)) %>%
    ungroup()
  
  df_oa <- df %>%
    filter(open_access != "Closed") %>%
    group_by(year) %>%
    summarise(total_oa_n = sum(n)) %>%
    ungroup()
  
  inner_join(df_all, df_oa, by = "year") %>%
    adorn_totals(., where = "row") %>%
    mutate(oa_corr_p = round(100 * total_oa_n/total_corr_n, 1)) %>%
    rename(Year = year,
           `Open access` = total_oa_n,
           `Journal articles` = total_corr_n,
           `Percent` = oa_corr_p)
  
}

open_access_trend_corresponding <- function(df, discrete_pal){
  
  corresponding_totals <- df %>%
    group_by(year) %>%
    summarise(total_corresponding_oa = sum(n)) %>%
    ungroup()
  
  ggplot() +
    geom_text(data = corresponding_totals, 
              aes(x = year, y = 105, label = total_corresponding_oa), size = 3.5) +
    geom_bar(data = df, 
             aes(x = year, y = p, fill = open_access,
                 text = paste0(open_access, ": ", p, "%",
                               "\nOutputs: ", n)), 
             stat = "identity", position = "stack") +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name = NULL,
                       breaks = seq(0, 100, 20), 
                       minor_breaks = seq(10, 90, 10),
                       labels = scales::percent_format(scale = 1)) +
    scale_fill_manual(name = "OA status", 
                      guide = (guide_legend(title.position = "top")),
                      values = discrete_pal[c(9,8,7,5,3)]) +
    theme_minimal() +
    theme(axis.text.x = element_text(vjust = 1),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "bottom", 
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
}


where_published_summary <- function(df, discrete_pal){
  
  n_journals <- df %>%
    filter(type == "article") %>%
    filter(!is.na(source_title)) %>%
    summarise(n = n_distinct(source_title)) %>%
    unname %>%
    unlist
  
  df_journals_count <- df %>%
    filter(type == "article") %>%
    filter(!is.na(source_title)) %>%
    distinct(publication_id, source_title) %>%
    group_by(source_title) %>%
    tally(name = "count") %>%
    ungroup() %>%
    arrange(desc(count)) %>%
    relocate(count) %>% 
    pivot_wider(names_from = count, values_from = source_title, values_fn = list) %>%
    t() %>% data.frame %>% 
    rename(Journal = 1) %>%
    rownames_to_column(var = "Outputs") %>%
    rowwise() %>%
    mutate(Journal = toString(Journal)) %>%
    ungroup()
  
  df_publishers_count <- df %>%
    filter(type == "article") %>%
    filter(!is.na(publisher)) %>%
    distinct(publication_id, .keep_all = TRUE) %>%
    group_by(publisher) %>%
    tally(n = "count") %>%
    ungroup() %>%
    arrange(desc(count)) %>%
    pivot_wider(names_from = count, values_from = publisher, values_fn = list) %>%
    t() %>% data.frame %>%
    rename(Publisher = 1) %>%
    rownames_to_column(var = "Outputs") %>%
    rowwise() %>%
    mutate(Publisher = toString(Publisher)) %>%
    ungroup()
  
  output_list <- list("n_journals" = n_journals,
                      "df_journals_count" = df_journals_count,
                      "df_publishers_count" = df_publishers_count)
  
}

# Research areas ----------
uncategorised_outputs <- function(df, category = "category"){

  df %>%
    filter(is.na(!!as.symbol(category))) %>%
    summarise(n = n_distinct(publication_id)) %>%
    unlist %>%
    unname

}

# categroised ouptuts
# n unique categories

for_2020_summary <- function(df){
  
  outputs_with_no_for <- df %>%
    filter(is.na(for_2020)) %>%
    summarise(n = n_distinct(publication_id)) %>%
    unlist %>%
    unname
  
  #outputs_with_no_for_test <- uncategorised_outputs(df, for_2020)

  n_unique_for <- df %>%
    filter(!is.na(for_2020)) %>%
    filter(level == "level_1") %>%
    summarise(n = n_distinct(for_2020)) %>%
    unlist %>%
    unname
  
  for_level_1_summary <- df %>%
    filter(!is.na(for_2020)) %>%
    filter(level == "level_1") %>%
    group_by(for_2020) %>%
    tally(name = "Frequency") %>%
    ungroup() %>%
    mutate(Percent = round(100 * Frequency/sum(Frequency), 1)) %>%
    arrange(desc(Percent)) %>%
    rename(`Level 1 Field of Research` = for_2020) #%>%
    #filter(Percent > 1)
  
  for_2020_level_1_per_publication <- df %>%
    filter(level == "level_1") %>%
    group_by(publication_id) %>%
    tally(name = "for_2020") %>%
    ungroup() %>%
    group_by(for_2020) %>%
    tally(name = "count") %>%
    ungroup() %>%
    mutate(Percent = round(100 * count/(outputs_with_no_for + sum(count)), 1))
  
  n_outputs_with_single_level_1_for <- for_2020_level_1_per_publication$count[for_2020_level_1_per_publication$for_2020 == 1]
  p_outputs_with_single_level_1_for <- for_2020_level_1_per_publication$Percent[for_2020_level_1_per_publication$for_2020 == 1]
  n_outputs_with_multi_level_1_for <- sum(for_2020_level_1_per_publication$count) - n_outputs_with_single_level_1_for
  p_outputs_with_multi_level_1_for <- 100 - p_outputs_with_single_level_1_for
  
  output_list <- list("outputs_with_no_for" = outputs_with_no_for,
                      "n_unique_for" = n_unique_for,
                      "for_level_1_summary" = for_level_1_summary,
                      "for_2020_level_1_per_publication" = for_2020_level_1_per_publication,
                      "n_outputs_with_single_level_1_for" = n_outputs_with_single_level_1_for,
                      "p_outputs_with_single_level_1_for" = p_outputs_with_single_level_1_for,
                      "n_outputs_with_multi_level_1_for" = n_outputs_with_multi_level_1_for,
                      "p_outputs_with_multi_level_1_for" = p_outputs_with_multi_level_1_for)
  
}

for_2020_treemap <- function(df, palette = discrete_pal){
  
  library(treemapify)
  
  df <- df %>%
    select(-type) %>%
    filter(!is.na(for_2020)) #%>%
    #mutate(level = ifelse(nchar(for_2020_code) == 2, "level_1", "level_2"))
  
  publication_ids <- df %>% distinct(publication_id)
  
  df_for_2020_out <- data.frame()
  
  for (i in seq_along(publication_ids$publication_id)){
    
    df_publication <- df %>%
      filter(publication_id == publication_ids$publication_id[i])
    
    df_for_2020_l1 <- df_publication %>% 
      filter(level == "level_1") %>% 
      select(-level) %>%
      rename(level_1_for = for_2020) %>%
      rename(level_1_code = for_2020_code)
    
    df_for_2020_l2 <- df_publication %>% 
      filter(level == "level_2") %>% 
      select(-level) %>%
      rename(level_2_for = for_2020) %>%
      rename(level_2_code = for_2020_code)
    
    df_collect <- data.frame()
    
    if(length(df_for_2020_l2$publication_id > 0)){ 
      
      for (j in seq_along(unique(df_for_2020_l1$level_1_code))){
        
        ref <- df_for_2020_l1$level_1_code[j]
        
        df_hold <- df_for_2020_l2 %>% 
          filter(as.numeric(str_extract(df_for_2020_l2$level_2_code, "^\\d{2}")) == ref) %>%
          select(-publication_id)
        
        n <- dim(df_hold)[1]
        
        df_ready <- rep(df_for_2020_l1[j,], n)
        
        df_ready <- cbind.data.frame(df_ready[1:3], df_hold)
        
        df_collect <- rbind(df_collect, df_ready)
        
      }
    } else {
      
      df_hold <- df_publication %>%
        select(-level) %>%
        mutate(level_2_for = NA,
               level_2_code = NA) %>%
        rename(level_1_for = for_2020) %>%
        rename(level_1_code = for_2020_code)
      
      df_collect <- rbind(df_collect, df_hold)
    }
    
    df_for_2020_out <- rbind(df_for_2020_out, df_collect)
    
  }
  
  df_for_2020_summary <- df_for_2020_out %>%
    group_by(level_1_for, level_2_for) %>%
    tally() %>% 
    ungroup() %>%
    rename(level_2_count = n)
  
  for_level_1_totals <- df_for_2020_summary %>%
    group_by(level_1_for) %>%
    summarise(n = sum(level_2_count))
  
  df_for_2020_summary <- df_for_2020_summary %>%
    mutate(level_1_total = VLOOKUP(level_1_for, for_level_1_totals, level_1_for, n)) %>%
    relocate(level_1_total, .after = level_1_for) %>%
    mutate(tot = sum(level_2_count)) %>%
    group_by(level_1_for) %>%
    mutate(level_2_cum = cumsum(level_2_count)) %>%
    ungroup()
  
  lower_bound <- unique(select(df_for_2020_summary, level_1_for, tot, level_1_total)) %>%
    mutate(bottom = tot - cumsum(level_1_total))
  
  df_for_2020_summary <- inner_join(df_for_2020_summary, 
                                    select(lower_bound, level_1_for, bottom),
                                    by = "level_1_for")
  
  df_for_2020_summary <- mutate(df_for_2020_summary, pos = bottom + level_2_cum - level_2_count/2)
  
  df_for_2020_summary <- df_for_2020_summary %>%
    mutate(level_2_for = replace_na(level_2_for, "None"),
           wrap_lbl = str_wrap(level_2_for, width = 20))
  
  my_palette <- colorRampPalette(palette[4:9])(n_distinct(df_for_2020_summary$level_1_for))
  
  ggplot(data = df_for_2020_summary,
         aes(area = level_2_count, fill = level_1_for, 
             label = paste0(wrap_lbl, " (", level_2_count, ")"), 
             subgroup = level_1_for)) +
    geom_treemap(colour = "white", size = 1.5) +
    geom_treemap_subgroup_text(colour = "grey40", place = "topleft", grow = TRUE) +
    geom_treemap_text(place = "center",
                      colour = "white",
                      size = 12) +
    geom_treemap_subgroup_border(colour = "grey25", size = 3.5) +
    scale_fill_manual(values = my_palette) +
    theme(legend.position = "none")
  
}

uoa_summary <- function(df){
  
  outputs_with_no_uoa <- df %>%
    filter(is.na(uoa)) %>%
    summarise(n = n_distinct(publication_id)) %>%
    unlist %>%
    unname
  
  #outputs_with_no_uoa_test <- uncategorised_outputs(df, uoa)

  n_units_of_assessment <- df %>%
    filter(!is.na(uoa)) %>%
    summarise(n = n_distinct(uoa)) %>%
    unlist %>%
    unname
  
  uoas_per_publication <- df %>%
    group_by(publication_id) %>%
    tally(name = "UoAs") %>%
    ungroup() %>%
    group_by(UoAs) %>%
    tally() %>%
    ungroup()
  
  np_one_uoa <- uoas_per_publication %>%
    mutate(percent = round(100 * (n/sum(n)), 1)) %>%
    filter(UoAs == 1)
  
  uoa_summary <- df %>%
    filter(!is.na(uoa)) %>%
    group_by(uoa) %>%
    tally(name = "Frequency") %>%
    ungroup() %>%
    mutate(Percent = round(100 * Frequency/sum(Frequency), 1)) %>%
    rename(`Unit of Assessment` = uoa)
  
  output_list = list("outputs_with_no_uoa" = outputs_with_no_uoa,
                     "n_units_of_assessment" = n_units_of_assessment,
                     "uoas_per_publication" = uoas_per_publication,
                     "np_one_uoa" = np_one_uoa,
                     "uoa_summary" = uoa_summary)
                     #,
                      #"outputs_with_no_uoa_test" = outputs_with_no_uoa_test)
  
}

mesh_summary <- function(df){
  
  outputs_with_at_least_one_mesh_term <- df %>%
    filter(!is.na(mesh_terms)) %>%
    summarise(n = n_distinct(publication_id)) %>%
    unlist %>%
    unname
  
  outputs_with_no_mesh_term <- df %>%
    filter(is.na(mesh_terms)) %>%
    summarise(n = n_distinct(publication_id)) %>%
    unlist %>%
    unname
  
  #outputs_with_no_mesh_term_test <- uncategorised_outputs(df, mesh_terms)

  n_unique_mesh_terms <- df %>%
    filter(!is.na(mesh_terms)) %>%
    summarise(n = n_distinct(mesh_terms)) %>%
    unlist %>%
    unname
  
  top_k_mesh_terms <- df %>%
    filter(!is.na(mesh_terms)) %>%
    group_by(mesh_terms) %>%
    tally(name = "Frequency") %>%
    ungroup() %>%
    mutate(Percent = round(100 * Frequency/sum(Frequency), 1)) %>%
    arrange(desc(Frequency)) %>%
    filter(Percent >= 1) %>%
    rename(`MeSH Term` = mesh_terms)
  
  k <- length(top_k_mesh_terms$Frequency)
  
  output_list = list("k" = k,
                     "outputs_with_at_least_one_mesh_term" = outputs_with_at_least_one_mesh_term,
                     "outputs_with_no_mesh_term" = outputs_with_no_mesh_term,
                     "n_unique_mesh_terms" = n_unique_mesh_terms,
                     "top_k_mesh_terms" = top_k_mesh_terms)#,
                      #"outputs_with_no_mesh_term_test" = outputs_with_no_mesh_term_test)
  
}

rcdc_summary <- function(df, p = 1){
  
  outputs_with_at_least_one_rcdc <- df %>%
    filter(!is.na(rcdc)) %>%
    summarise(n = n_distinct(publication_id)) %>%
    unlist %>%
    unname
  
  outputs_with_no_rcdc <- df %>%
    filter(is.na(rcdc)) %>%
    summarise(n = n_distinct(publication_id)) %>%
    unlist %>%
    unname

  #outputs_with_no_rcdc_test <- uncategorised_outputs(df, rcdc)
  
  n_unique_rcdc <- df %>%
    filter(!is.na(rcdc)) %>%
    summarise(n = n_distinct(rcdc)) %>%
    unlist %>%
    unname
  
  top_k_rcdc <- df %>%
    filter(!is.na(rcdc)) %>%
    group_by(rcdc) %>%
    tally(name = "Frequency") %>%
    ungroup() %>%
    mutate(Percent = round(100 * Frequency/sum(Frequency), 1)) %>%
    arrange(desc(Frequency)) %>%
    filter(Percent >= p) %>%
    rename(`Research, Condition, and Disease Category` = rcdc)
  
  output_list = list("p" = p, 
                     "outputs_with_at_least_one_rcdc" = outputs_with_at_least_one_rcdc,
                     "outputs_with_no_rcdc" = outputs_with_no_rcdc,
                     "n_unique_rcdc" = n_unique_rcdc,
                     "top_k_rcdc" = top_k_rcdc)#,
                      #"outputs_with_no_rcdc_test" = outputs_with_no_rcdc_test)
  
}

hrcs_hc_summary <- function(df, p = 1){
  
  outputs_with_at_least_one_hrcs_hc <- df %>%
    filter(!is.na(hrcs_hc)) %>%
    summarise(n = n_distinct(publication_id)) %>%
    unlist %>%
    unname
  
  outputs_with_no_hrcs_hc <- df %>%
    filter(is.na(hrcs_hc)) %>%
    summarise(n = n_distinct(publication_id)) %>%
    unlist %>%
    unname
  
  n_unique_hrcs_hc <- df %>%
    filter(!is.na(hrcs_hc)) %>%
    summarise(n = n_distinct(hrcs_hc)) %>%
    unlist %>%
    unname
  
  top_k_hrcs_hc <- df %>%
    filter(!is.na(hrcs_hc)) %>%
    group_by(hrcs_hc) %>%
    tally(name = "Frequency") %>%
    ungroup() %>%
    mutate(Percent = round(100 * Frequency/sum(Frequency), 1)) %>%
    arrange(desc(Frequency)) %>%
    filter(Percent >= p) %>%
    rename(`HRCS Health Category` = hrcs_hc)
  
  output_list = list("p" = p, 
                     "outputs_with_at_least_one_hrcs_hc" = outputs_with_at_least_one_hrcs_hc,
                     "outputs_with_no_hrcs_hc" = outputs_with_no_hrcs_hc,
                     "n_unique_hrcs_hc" = n_unique_hrcs_hc,
                     "top_k_hrcs_hc" = top_k_hrcs_hc)
  
}

hrcs_rac_summary <- function(df, p = 1){
  
  outputs_with_at_least_one_hrcs_rac <- df %>%
    filter(!is.na(hrcs_rac)) %>%
    summarise(n = n_distinct(publication_id)) %>%
    unlist %>%
    unname
  
  outputs_with_no_hrcs_rac <- df %>%
    filter(is.na(hrcs_rac)) %>%
    summarise(n = n_distinct(publication_id)) %>%
    unlist %>%
    unname
  
  n_unique_hrcs_rac <- df %>%
    filter(!is.na(hrcs_rac)) %>%
    summarise(n = n_distinct(hrcs_rac)) %>%
    unlist %>%
    unname
  
  top_k_hrcs_rac <- df %>%
    filter(!is.na(hrcs_rac)) %>%
    group_by(hrcs_rac) %>%
    tally(name = "Frequency") %>%
    ungroup() %>%
    mutate(Percent = round(100 * Frequency/sum(Frequency), 1)) %>%
    arrange(desc(Frequency)) %>%
    filter(Percent >= p) %>%
    rename(`HRCS Research Activity Classification` = hrcs_rac)
  
  output_list = list("p" = p, 
                     "outputs_with_at_least_one_hrcs_rac" = outputs_with_at_least_one_hrcs_rac,
                     "outputs_with_no_hrcs_rac" = outputs_with_no_hrcs_rac,
                     "n_unique_hrcs_rac" = n_unique_hrcs_rac,
                     "top_k_hrcs_rac" = top_k_hrcs_rac)
  
}

icrp_cso_summary <- function(df, p = 1){
  
  outputs_with_at_least_one_icrp_cso <- df %>%
    filter(!is.na(icrp_cso)) %>%
    summarise(n = n_distinct(publication_id)) %>%
    unlist %>%
    unname
  
  outputs_with_no_icrp_cso <- df %>%
    filter(is.na(icrp_cso)) %>%
    summarise(n = n_distinct(publication_id)) %>%
    unlist %>%
    unname
  
  n_unique_icrp_cso <- df %>%
    filter(!is.na(icrp_cso)) %>%
    summarise(n = n_distinct(icrp_cso)) %>%
    unlist %>%
    unname
  
  top_k_icrp_cso <- df %>%
    filter(!is.na(icrp_cso)) %>%
    group_by(icrp_cso) %>%
    tally(name = "Frequency") %>%
    ungroup() %>%
    mutate(Percent = round(100 * Frequency/sum(Frequency), 1)) %>%
    arrange(desc(Frequency)) %>%
    filter(Percent >= p) %>%
    rename(`ICRP Common Scientific Outline category` = icrp_cso)
  
  output_list = list("p" = p, 
                     "outputs_with_at_least_one_icrp_cso" = outputs_with_at_least_one_icrp_cso,
                     "outputs_with_no_icrp_cso" = outputs_with_no_icrp_cso,
                     "n_unique_icrp_cso" = n_unique_icrp_cso,
                     "top_k_icrp_cso" = top_k_icrp_cso)
  
}

sdg_summary <- function(df){
  
  outputs_with_at_least_one_sdg <- df %>%
    filter(!is.na(sdg)) %>%
    summarise(n = n_distinct(publication_id)) %>%
    unlist %>%
    unname
  
  outputs_with_no_sdg <- df %>%
    filter(is.na(sdg)) %>%
    summarise(n = n_distinct(publication_id)) %>%
    unlist %>%
    unname
  
  #outputs_with_no_sdg_test <- uncategorised_outputs(df, sdg)

  p_with_at_least_one_sdg <- round(100 * outputs_with_at_least_one_sdg/(outputs_with_at_least_one_sdg + outputs_with_no_sdg), 1)
  
  n_unique_sdg <- df %>%
    filter(!is.na(sdg)) %>%
    summarise(n = n_distinct(sdg)) %>%
    unlist %>%
    unname
  
  sdgs_per_publication <- df %>%
    filter(!is.na(sdg)) %>%
    group_by(publication_id) %>%
    tally(name = "sdgs") %>%
    ungroup() %>%
    group_by(sdgs) %>%
    tally() %>%
    ungroup()
  
  n_with_multi_sdgs <- sdgs_per_publication %>%
    filter(sdgs != 1) %>%
    summarise(n = sum(n)) %>%
    unlist %>%
    unname
  
  sdg_summary <- df %>%
    filter(!is.na(sdg)) %>%
    group_by(sdg) %>%
    tally(name = "Frequency") %>%
    ungroup() %>%
    mutate(Percent = round(100 * Frequency/sum(Frequency), 1)) %>%
    arrange(desc(Frequency)) %>%
    rename(`Sustainable Development Goal` = sdg)
  
  output_list <- list("outputs_with_at_least_one_sdg" = outputs_with_at_least_one_sdg,
                      "outputs_with_no_sdg" = outputs_with_no_sdg,
                      "p_with_at_least_one_sdg" = p_with_at_least_one_sdg,
                      "n_unique_sdg" = n_unique_sdg,
                      "sdgs_per_publication" = sdgs_per_publication,
                      "sdgs_per_publication" = sdgs_per_publication,
                      "n_with_multi_sdgs" = n_with_multi_sdgs,
                      "sdg_summary" = sdg_summary)#,
                      #"outputs_with_no_sdg_test" = outputs_with_no_sdg_test)
  
}
