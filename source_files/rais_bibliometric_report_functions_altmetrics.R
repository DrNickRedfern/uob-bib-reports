altmetrics_summary <- function(df){
  
  n_outputs_with_altmetric_score <- n_distinct(df$doi)
  
  altmetric_scores_summary <- df %>%
    select(Altmetric) %>% 
    summarise(min = fivenum(Altmetric)[1],
              Q1 = fivenum(Altmetric)[2],
              median = fivenum(Altmetric)[3],
              Q3 = fivenum(Altmetric)[4],
              max = fivenum(Altmetric)[5])
  
  output_list = list("n_outputs_with_altmetric_score" = n_outputs_with_altmetric_score,
                     "altmetric_scores_summary" = altmetric_scores_summary)
  
}

altmetrics_plot <- function(df, discrete_pal){
  
  breaks <- 10^(-10:10)
  minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each = 9))
  
  altmetric_bar <- df %>% 
    select(Altmetric) %>%
    ggplot() +
    geom_bar(aes(x = Altmetric), stat = "count", fill = discrete_pal[9]) +
    scale_x_continuous(name = "Altmetric score", 
                       breaks = breaks,
                       minor_breaks = minor_breaks,
                       trans = "log10") +
    scale_y_continuous(name = "Outputs") +
    theme_light() +
    theme(axis.line.x = element_line(colour = "gray"),
          axis.ticks = element_blank(),
          panel.border = element_blank(), 
          panel.grid.minor.y = element_blank(),
          text = element_text(size = 10))
  
  altmetric_box <- df %>% 
    select(Altmetric) %>%
    ggplot(aes(x = " ", y = Altmetric)) +
    geom_boxplot(outlier.shape = NA, coef = 0) +
    geom_jitter(width = 0.2, colour = discrete_pal[9]) +
    scale_x_discrete(name = NULL) +
    scale_y_continuous(name = NULL, 
                       breaks = breaks,
                       minor_breaks = minor_breaks,
                       trans = "log10") +
    coord_flip() +
    theme_light() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.grid.major.y = element_blank())
  
  ggpubr::ggarrange(altmetric_box, altmetric_bar, 
                    nrow = 2, align = "v", heights = c(1,2))
  
}

altmetric_types <- function(df, unique_outputs = n_unique_outputs){
  
  df_altmetric_types <- df %>%
    select(-c(altmetric_id:current_affiliation)) %>%
    select(-c(rank, Altmetric)) %>%
    pivot_longer(!doi, names_to = "altmetric", values_to = "count") 
  
  outputs_per_altmetric <- df_altmetric_types %>%
    filter(!is.na(count)) %>%
    group_by(altmetric) %>%
    tally(name = "outputs") %>%
    ungroup() %>%
    mutate(percent = round(100 * outputs/unique_outputs, 1))
  
  mentions_per_altmetric <- df_altmetric_types %>%
    group_by(altmetric) %>%
    summarise(mentions = sum(count, na.rm = TRUE)) %>%
    ungroup()
  
  altmetric_summary <- inner_join(outputs_per_altmetric, mentions_per_altmetric, by = "altmetric")
  
  n_outputs_in_news <- ifelse("News" %in% altmetric_summary$altmetric,
                              altmetric_summary$outputs[altmetric_summary$altmetric == "News"], 0)
  
  n_mentions_in_news <- ifelse("News" %in% altmetric_summary$altmetric,
                               altmetric_summary$mentions[altmetric_summary$altmetric == "News"], 0)
  
  n_outputs_in_policy_docs <- ifelse("Policy documents" %in% altmetric_summary$altmetric,
                                     altmetric_summary$outputs[altmetric_summary$altmetric == "Policy documents"], 0)
  n_mentions_in_policy_docs <- ifelse("Policy documents" %in% altmetric_summary$altmetric,
                                      altmetric_summary$mentions[altmetric_summary$altmetric == "Policy documents"], 0)
  
  output_list = list("n_outputs_in_news" = n_outputs_in_news,
                     "n_mentions_in_news" = n_mentions_in_news,
                     "n_outputs_in_policy_docs" = n_outputs_in_policy_docs,
                     "n_mentions_in_policy_docs" = n_mentions_in_policy_docs,
                     "altmetric_summary" = altmetric_summary)
  
}

altmetrics_context <- function(df, discrete_pal){
  
  # TODO: replace with case_when
  df_altmetric_context <- df %>%
    select(doi, contains("context")) %>%
    filter(!is.na(context_all_count)) %>%
    select(doi, contains("pct")) %>%
    pivot_longer(!doi, names_to = "context", values_to = "percentile")
  
  df_altmetric_context <- df_altmetric_context %>%
    mutate(clean_name = ifelse(context == unique(df_altmetric_context$context)[1], "All research outputs",
                               ifelse(context == unique(df_altmetric_context$context)[3], 
                                      "Outputs of similar age",
                                      ifelse(context == unique(df_altmetric_context$context)[2],
                                             "All outputs from source",
                                             "Outputs of similar age from source")))) 
  
  df_altmetric_context$clean_name <- factor(df_altmetric_context$clean_name,
                                            levels = c(unique(df_altmetric_context$clean_name)[1],
                                                       unique(df_altmetric_context$clean_name)[3],
                                                       unique(df_altmetric_context$clean_name)[2],
                                                       unique(df_altmetric_context$clean_name)[4]))
  
  df_altmetric_context <- df_altmetric_context %>%
    filter(str_detect(context, "journal"))
  
  outputs_with_altmetric_context <- n_distinct(df_altmetric_context$doi)
  
  altmetric_context_data_summary <- df_altmetric_context %>%
    group_by(context) %>%
    summarise(min = fivenum(percentile)[1],
              Q1 = fivenum(percentile)[2],
              median = fivenum(percentile)[3],
              Q3 = fivenum(percentile)[4],
              max = fivenum(percentile)[5])
  
  altmetric_context_distribution <- ggplot(data = df_altmetric_context, 
                                           aes(x = clean_name, y = percentile, fill = clean_name)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = 0.2) +
    scale_x_discrete(name = NULL, 
                     limits = rev(levels(df_altmetric_context$clean_name)[3:4])) +
    scale_y_continuous(name = "Percentile") +
    scale_fill_manual(name = NULL, values = discrete_pal[c(3, 6)]) +
    coord_flip() +
    my_theme() +
    theme(legend.position = "none")
  
  output_list <- list("outputs_with_altmetric_context" = outputs_with_altmetric_context,
                      "altmetric_context_data_summary" = altmetric_context_data_summary,
                      "altmetric_context_distribution" = altmetric_context_distribution)
  
}
