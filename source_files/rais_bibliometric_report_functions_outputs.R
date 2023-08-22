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

chart_output_affiliated <- function(df, discrete_pal, min_year, max_year){
  
  df %>%
    filter(current_affiliation == "University of Bradford") %>%
    distinct(publication_id, year) %>%
    group_by(year) %>%
    tally() %>%
    ungroup() %>%
    ggplot(aes(x = year, y = n)) +
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
  
  ggplot(data = df, aes(x = year, y = n, fill = type)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_x_continuous(name = "Year", 
                       breaks = seq(min_year, max_year, 1), 
                       labels = seq(min_year, max_year, 1)) +
    scale_y_continuous(name = "Outputs",
                       breaks = seq(0, 100, 20),
                       labels = seq(0, 100, 20)) +
    scale_fill_manual(name = NULL, values = rev(discrete_pal),
                      guide = guide_legend(title.position = "top")) +
    my_theme() +
    theme(legend.position = "bottom")
  
}

chart_percentage_change <- function(df, discrete_pal, min_year, max_year){
  
  df %>%
    filter(!is.na(pct_change)) %>%
    ggplot(aes(x = year, y = pct_change, fill = factor(fill))) +
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

chart_cumulative_growth <- function(df, discrete_pal, min_year, max_year){
  
  ggplot(data = df, aes(x = year, y = cumulative_outputs)) +
    geom_area(fill = discrete_pal[9], alpha = 0.6) +
    geom_line(colour = discrete_pal[9], linewidth = 1) +
    geom_point(colour = discrete_pal[9]) +
    scale_x_continuous(name = "Year",
                       breaks = seq(min_year, max_year, 1), 
                       labels = seq(min_year, max_year, 1)) +
    scale_y_continuous(name = "Outputs") +
    my_theme()
  
}

open_access_summary <- function(df){
  
  open_access_data <- df %>%
    filter(type == "article") %>%
    select(publication_id, open_access, times_cited, field_citation_ratio) %>%
    distinct(publication_id, open_access, times_cited, field_citation_ratio) %>%
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
           pos = if_else(is.na(pos), count/2, pos))
  
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
                     colour = c(rep("white", 4), "black"),
                     show.legend = FALSE) +
    scale_x_discrete(name = "Open access type") +
    scale_y_continuous(name = "Frequency") +
    scale_fill_manual(name = NULL, values = discrete_pal[c(9,8,7,5,3)]) +
    coord_polar(theta = "y", start = 0, direction = -1) +
    xlim(0.1, 3) +
    theme_void() +
    theme(legend.position = "none")
  
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
    filter(count > 1) %>%
    pivot_wider(names_from = count, values_from = source_title, values_fn = list) %>%
    t() %>% data.frame %>% 
    rename(Journal = 1) %>%
    rownames_to_column(var = "Outputs") %>%
    rowwise() %>%
    mutate(Journal = toString(Journal)) %>%
    ungroup()
  
  publishers_plot <- df %>%
    filter(type == "article") %>%
    filter(!is.na(publisher)) %>%
    distinct(publication_id, .keep_all = TRUE) %>%
    group_by(publisher) %>%
    tally() %>%
    ungroup() %>%
    arrange(n) %>%
    mutate(publisher = factor(publisher, levels = publisher)) %>%
    ggplot() +
    geom_col(aes(x = publisher, y = n), fill = discrete_pal[9]) +
    geom_text(aes(x = publisher, y = n, label = n), hjust = -0.5, size = 3) +
    scale_x_discrete(name = NULL,
                     labels = function(x) str_wrap(x, width = 40)) +
    scale_y_continuous(name = NULL, expand = c(0, 0.1), limits = c(0, 100)) +
    coord_flip() +
    my_theme() +
    theme(axis.line.x = element_blank(),
          axis.line.y = element_line(colour = "gray"),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank())
  
  output_list <- list("n_journals" = n_journals,
                      "df_journals_count" = df_journals_count,
                      "publishers_plot" = publishers_plot)
  
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
    rename(`Level 1 Field of Research` = for_2020) %>%
    filter(Percent > 1)
  
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
