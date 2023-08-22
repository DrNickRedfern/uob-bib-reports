impact_summary <- function(df){
  
  n_distinct_outputs <- dim(df)[1]
  
  total_citations <- sum(df$times_cited)
  
  n_cited_outputs <- df %>%
    summarise(n = sum(times_cited > 0)) %>%
    unname %>%
    unlist
  
  n_uncited_outputs <- n_distinct_outputs - n_cited_outputs
  p_uncited_outputs <- round(100 * (n_uncited_outputs/n_distinct_outputs), 0)
  p_cited_outputs <- round(100 * (n_cited_outputs/n_distinct_outputs), 1)
  
  i_10_index_by_year <- df %>%
    group_by(year) %>%
    summarise(n = sum(times_cited >= 10)) %>%
    ungroup() %>%
    rename(Year = year)
  
  i_10_index_overall <- i_10_index_by_year %>% 
    filter(!is.na(n)) %>% 
    select(n) %>% 
    sum
  
  p_i_10_index_overall <- round(100 * (i_10_index_overall/n_distinct_outputs), 1)
  
  output_list = list("n_distinct_outputs" = n_distinct_outputs,
                     "total_citations" = total_citations,
                     "n_cited_outputs" = n_cited_outputs,
                     "n_uncited_outputs" = n_uncited_outputs,
                     "p_uncited_outputs" = p_uncited_outputs,
                     "p_cited_outputs" = p_cited_outputs,
                     "i_10_index_by_year" = i_10_index_by_year,
                     "i_10_index_overall" = i_10_index_overall,
                     "p_i_10_index_overall" = p_i_10_index_overall)
  
}

fcr_summary <- function(df){
  
  df <- df %>% 
    filter(!is.na(field_citation_ratio)) %>% 
    mutate(class = case_when((times_cited == 0) ~ "uncited",
                             (field_citation_ratio == 1) ~ "average",
                             (field_citation_ratio < 1) ~ "below_average",
                             TRUE ~ "above_average"))
  
  fcr_class_summary <- df %>%
    select(class) %>%
    group_by(class) %>%
    summarise(n = n()) %>%
    ungroup()
  
  n_outputs_with_fcr <- length(df$publication_id)
  
  n_uncited <- fcr_class_summary$n[fcr_class_summary$class == "uncited"]
  p_uncited <- round(100 * (n_uncited/sum(fcr_class_summary$n)), 1)
  
  n_below_average <- fcr_class_summary$n[fcr_class_summary$class == "below_average"]
  p_below_average <- round(100 * (n_below_average/sum(fcr_class_summary$n)), 1)
  
  n_above_average <- fcr_class_summary$n[fcr_class_summary$class == "above_average"]
  p_above_average <- round(100 * (n_above_average/sum(fcr_class_summary$n)), 1)
  
  range_fcr <- range(df$field_citation_ratio)
  median_fcr <- round(median(df$field_citation_ratio), 2)
  
  fcr_overall_geomean <- df %>%
    mutate(fcr_plus_one = field_citation_ratio + 1,
           fcr_geomean = exp(mean(log(fcr_plus_one))) - 1) %>%
    select(fcr_geomean) %>%
    round(2) %>%
    unique() %>%
    unlist %>% 
    unname
  
  output_list = list("fcr_class_summary" = fcr_class_summary,
                     "n_outputs_with_fcr" = n_outputs_with_fcr,
                     "n_uncited" = n_uncited,
                     "p_uncited" = p_uncited,
                     "n_below_average" = n_below_average,
                     "p_below_average" = p_below_average,
                     "n_above_average" = n_above_average,
                     "p_above_average" = p_above_average,
                     "range_fcr" = range_fcr,
                     "median_fcr" = median_fcr,
                     "fcr_overall_geomean" = fcr_overall_geomean)
  
}

citations_summary <- function(df, i_10_index_by_year){
  
  df <- df %>%
    group_by(year) %>%
    summarise(Outputs = n(),
              `Cited outputs` = sum(times_cited > 0),
              `Total citations` = sum(times_cited),
              `Mean citations` = round(mean(times_cited), 1),
              `Median citations` = round(median(times_cited), 1)) %>%
    ungroup() %>%
    rename(Year = year)
  
  inner_join(df, i_10_index_by_year, by = "Year") %>%
    rename(`i-10` = n)
  
}

fcr_box_plot <- function(df, discrete_pal){
  
  df %>% 
    filter(!is.na(field_citation_ratio)) %>%
    ggplot(aes(x = " ", y = field_citation_ratio)) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    geom_boxplot(outlier.shape = NA, coef = 0) +
    geom_jitter(aes(colour = open_access), width = 0.25) +
    scale_x_discrete(name = NULL) +
    scale_y_continuous(name = "Field Citation Ratio",
                       trans = scales::pseudo_log_trans(base = 10),
                       breaks = c(0, 1, 10, 100, 1000),
                       minor_breaks = c(seq(2, 9, 1), seq(20, 90, 10), seq(200, 900, 100)),
                       labels = c("Uncited", 1, 10, 100, 1000)) +
    scale_colour_manual(name = "Open access",
                        values = discrete_pal[c(9,8,7,5,3)],
                        guide = (guide_legend(title.position = "top"))) +
    coord_flip() +
    theme_light() +
    theme(axis.line.x = element_line(colour = "gray"),
          axis.text.y = element_text(angle = 90),
          axis.ticks.y = element_blank(),
          legend.position = "bottom",
          legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 10),
          panel.border = element_blank(),
          panel.grid.major.y = element_blank(),
          text = element_text(size = 10))
  
}

open_access_citations <- function(df){
  
  oa_citations <- df %>%
    filter(!is.na(times_cited)) %>%
    group_by(open_access) %>%
    summarise(`Total outputs` = n(), 
              `Cited outputs` = sum(times_cited > 0),
              `Total citations` = sum(times_cited),
              `Mean citations` = round(mean(times_cited), 1),
              `Median citations` = round(median(times_cited), 1)) %>%
    ungroup()
  
  oa_fcr_geomean <- df %>%
    filter(!is.na(field_citation_ratio)) %>%
    mutate(fcr_plus_one = field_citation_ratio + 1) %>%
    group_by(open_access) %>%
    mutate(`FCR geomean` = exp(mean(log(fcr_plus_one))) - 1) %>%
    distinct(`FCR geomean`) %>%
    mutate(`FCR geomean` = round(`FCR geomean`, 1)) %>%
    ungroup()
  
  open_access_citations <- inner_join(oa_citations, oa_fcr_geomean, 
                                      by = "open_access") %>%
    rename(`Open Access` = open_access)
  
}

citing_journals <- function(df, k = 25){
  
  df %>%
    filter(type == "article") %>%
    group_by(journal_title_raw) %>%
    tally(n = "count") %>%
    ungroup() %>%
    top_n(., k, count) %>%
    arrange(desc(count)) %>%
    relocate(count) %>% 
    pivot_wider(names_from = count, values_from = journal_title_raw, values_fn = list) %>%
    t() %>% data.frame %>% 
    rename(Journal = 1) %>%
    rownames_to_column(var = "Citing publications") %>%
    rowwise() %>%
    mutate(Journal = toString(Journal)) %>%
    ungroup()
  
}

citations_by_publisher <- function(df, k = 20, discrete_pal){
  
  df <- df %>%
    filter(!is.na(publisher)) %>%
    group_by(publisher) %>%
    tally(n = "citing_publications") %>%
    arrange(desc(citing_publications)) %>%
    top_n(k, citing_publications)
  
  df$publisher = factor(df$publisher, levels = df$publisher[order(df$citing_publications)])
  
  ggplot(data = df,
         aes(x = publisher, y = citing_publications)) +
    geom_bar(stat = "identity", fill = discrete_pal[9]) +
    geom_text(aes(label = citing_publications), hjust = -0.5, size = 3) +
    scale_x_discrete(name = NULL,
                     labels = function(x) str_wrap(x, width = 40)) +
    scale_y_continuous(name = NULL, expand = expansion(mult = c(0, 0.1))) +
    coord_flip() +
    my_theme() +
    theme(axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          panel.grid.major.y = element_blank())
  
}

citers_summary <- function(df){
  
  n_unique_citing_countries <- n_distinct(df$country)
  n_unique_citing_organisations <- n_distinct(df$name)
  
  n_nas_countries <- df %>%
    filter(is.na(country)) %>%
    tally() %>%
    unlist %>%
    unname
  
  df_citing_countries <- df %>%
    filter(!is.na(country)) %>%
    group_by(country) %>%
    tally(n = "frequency") %>%
    ungroup() %>%
    arrange(desc(frequency)) %>%
    rename(region = country) %>%
    mutate(region = recode(str_trim(region), "United States" = "USA",
                           "United Kingdom" = "UK",
                           "Korea (Rep.)" = "South Korea",
                           "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
                           "Congo (Rep.)" = "Republic of Congo"))
  
  outputs_list = list("n_unique_citing_countries" = n_unique_citing_countries,
                      "n_unique_citing_organisations" = n_unique_citing_organisations,
                      "n_nas_countries" = n_nas_countries,
                      "df_citing_countries" = df_citing_countries)
  
}

citing_orgs <- function(df, k = 10, discrete_pal){
  
  df <- df %>%
    filter(!is.na(name)) %>%
    group_by(name) %>%
    tally(n = "frequency") %>%
    ungroup() %>%
    arrange(desc(frequency)) %>%
    mutate(rank = rank(-frequency, ties.method = "min")) %>%
    relocate(rank) %>%
    filter(rank <= k)
  
  df$name <- factor(df$name, levels = df$name[order(df$frequency)])
  
  ggplot(data = df, aes(x = name, y = frequency)) +
    geom_bar(stat = "identity", fill = discrete_pal[9]) +
    geom_text(aes(label = frequency), hjust = -0.5, size = 3) +
    scale_x_discrete(name = NULL,
                     labels = function(x) str_wrap(x, width = 40)) +
    scale_y_continuous(name = NULL,
                       expand = expansion(mult = c(0, 0.1))
    ) +
    coord_flip() +
    my_theme() +
    theme(axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.y = element_blank())
  
}

citing_org_types <- function(df, discrete_pal, 
                             label_colours = "black"){
  
  df_org_types_colours <- org_colours(discrete_pal)
  
  df <- df %>%
    filter(!is.na(types)) %>%
    group_by(types) %>%
    tally(n = "count") %>%
    ungroup() %>%
    arrange(count) %>%
    mutate(prop = round(count/sum(count), 3),
           cum_count = rev(cumsum(rev(count))),
           pos = count/2 + lead(cum_count, 1),
           pos = if_else(is.na(pos), count/2, pos),
           colour = VLOOKUP(types, df_org_types_colours, org_type, colour))
  
  df$types <- factor(df$types, levels = df$types[order(df$count)])
  
  df$colour <- factor(df$colour, levels = df$colour[order(df$count)])
  
  ggplot(data = df, aes(x = 2, y = count, fill = colour)) +
    geom_col(colour = "black", linewidth = 0.5) +
    geom_label_repel(aes(y = pos,
                         label = glue("{types} ({scales::percent(prop)})"), 
                         fill = colour),
                     size = 3.5,
                     nudge_x = 1,
                     colour = label_colours,
                     show.legend = FALSE) +
    scale_fill_identity(name = NULL,
                        labels = df$types,
                        breaks = df$colour) +
    coord_polar(theta = "y", start = 0, direction = -1) +
    xlim(0.1, 3) +
    theme_void() +
    theme(legend.position = "none")
  
}
