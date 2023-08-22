# Researchers ----------
researchers_summary <- function(df){
  
  n_researchers_in_results <-  n_distinct(df$full_name)
  
  # Remember this is from all researchers and not just researchers with publications
  n_currently_affiliated_researchers <- df %>%
    filter(current_research_org.name == "University of Bradford") %>%
    summarise(n = n_distinct(full_name)) %>%
    unlist %>%
    unname
  
  n_researchers_without_orcid <- df %>%
    distinct(full_name, orcid_id) %>%
    summarise(n = length(orcid_id) - sum(is.na(orcid_id))) %>%
    unlist %>%
    unname
  
  n_researchers_with_orcid <- n_researchers_in_results - n_researchers_without_orcid
  
  p_researchers_with_orcid <- round(100 * (n_researchers_with_orcid/n_researchers_in_results), 0)
  
  output_list <- list("n_researchers_in_results" = n_researchers_in_results, 
                      "n_currently_affiliated_researchers" = n_currently_affiliated_researchers, 
                      "n_researchers_without_orcid" = n_researchers_without_orcid,
                      "n_researchers_with_orcid" = n_researchers_with_orcid, 
                      "p_researchers_with_orcid" = p_researchers_with_orcid)
  
}

researchers_outputs <- function(df){
  
  df %>%
    group_by(full_name) %>%
    count(year) %>%
    pivot_wider(names_from = year, values_from = n) %>%
    replace(is.na(.), 0) %>%
    mutate(Total = rowSums(across(where(is.numeric)))) %>%
    rename(Researcher = full_name) %>%
    select(order(colnames(.))) %>%
    relocate(Researcher) %>%
    ungroup() %>%
    replace(is.na(.), "NA") %>% 
    mutate(last_name = str_split_i(Researcher, " ", -1)) %>%
    arrange(last_name) %>%
    select(-last_name)
  
}

academic_age_summary <- function(df){
  
  mean_academic_age <- df %>%
    summarise(mean = mean(academic_age)) %>%
    round(1) %>%
    unlist %>%
    unname
  
  median_academic_age <- df %>%
    summarise(median = median(academic_age)) %>%
    round(1) %>%
    unlist %>%
    unname
  
  academic_age_bins <- c(0, 1, seq(5, 50, 5))
  academic_age_tags <- c("Below 1", "1 to 5", "6 to 10", "11 to 15", "16 to 20", "21 to 25", "26 to 30", "31 to 35", "36 to 40", "41 or above")
  
  df_academic_age <- df %>%
    select(full_name, researcher_id, academic_age, total_publications) %>%
    mutate(tag = case_when(
      academic_age < 1 ~ academic_age_tags[1],
      academic_age >= 1 & academic_age < 6 ~ academic_age_tags[2],
      academic_age >= 6 & academic_age < 11 ~ academic_age_tags[3],
      academic_age >= 11 & academic_age < 16 ~ academic_age_tags[4],
      academic_age >= 16 & academic_age < 21 ~ academic_age_tags[5],
      academic_age >= 21 & academic_age < 26 ~ academic_age_tags[6],
      academic_age >= 26 & academic_age < 31 ~ academic_age_tags[7],
      academic_age >= 31 & academic_age < 36 ~ academic_age_tags[8],
      academic_age >= 36 & academic_age < 41 ~ academic_age_tags[9],
      academic_age >= 41 ~ academic_age_tags[10]
    )) %>%
    filter(!is.na(total_publications))
  
  df_academic_age$tag <- factor(df_academic_age$tag,
                                levels = academic_age_tags,
                                ordered = FALSE)
  
  p_younger_than_eleven <- df_academic_age %>%
    distinct(full_name, academic_age) %>%
    summarise(n = sum(academic_age < 11)) %>%
    mutate(n = 100 * (n/length(df_academic_age$academic_age))) %>%
    round(1) %>%
    unlist %>%
    unname
  
  output_list <- list("mean_academic_age" = mean_academic_age,
                      "median_academic_age" = median_academic_age,
                      "p_younger_than_eleven" = p_younger_than_eleven,
                      "academic_age" = df_academic_age)
  
}

academic_age_plot <- function(df, discrete_pal){
  
  ggplot(data = df) +
    geom_bar(aes(x = tag), fill = discrete_pal[9]) +
    stat_count(geom = "text", aes(x = tag, label = after_stat(count)), hjust = -0.5, size = 3) +
    scale_x_discrete(name = "Academic age", drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    my_theme() +
    coord_flip() +
    theme(axis.line.x = element_blank(),
          axis.line.y = element_line(colour = "gray"),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank())
  
}


