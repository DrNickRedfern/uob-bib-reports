collaboration_summary <- function(df){
  
  df_types_of_collaboration <- df %>%
    group_by(collaboration) %>%
    distinct(publication_id) %>%
    tally(name = "frequency") %>% 
    complete(collaboration, fill = list(frequency = 0)) %>%
    ungroup() %>%
    mutate(percent = round(100 * frequency/sum(frequency), 1)) %>%
    rename_with(str_to_title) %>%
    adorn_totals(., where = "row")
  
  p_collaboration <- df_types_of_collaboration %>%
    filter(Collaboration != "Single authored") %>%
    summarise(n = sum(Percent)) %>% 
    unlist() %>% 
    unname()
  
  output_list <- list("df_types_of_collaboration" = df_types_of_collaboration,
                      "p_collaboration" = p_collaboration)
  
}

countries_collab_summary <- function(df){
  
  collaboration_countries <- df %>%
    filter(organisation_name != "University of Bradford") %>%
    select(publication_id, country_name, year) %>%
    rename(region = country_name) %>%
    mutate(region = recode(str_trim(region), "United States" = "USA",
                           "United Kingdom" = "UK",
                           "Korea (Rep.)" = "South Korea",
                           "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
                           "Congo (Rep.)" = "Republic of Congo"))
  
  df_collaborating_countries <- collaboration_countries %>%
    group_by(region) %>%
    summarise(frequency = n_distinct(publication_id)) %>%
    ungroup() %>%
    arrange(desc(frequency)) %>%
    mutate(rank = rank(-frequency, ties.method = "min")) %>%
    relocate(rank) 
  
  n_collaborating_countries <- dim(df_collaborating_countries)[1]
  
  max_country <- df_collaborating_countries$region[which.max(df_collaborating_countries$frequency)]
  
  output_list <- list("collaboration_countries" = collaboration_countries,
                      "df_collaborating_countries" = df_collaborating_countries,
                      "n_collaborating_countries" = n_collaborating_countries,
                      "max_country" = max_country)
  
}

collaborating_orgs_summary <- function(df){
  
  unique_organisations <- df %>%
    filter(organisation_name != "University of Bradford") %>%
    select(organisation_name, organisation_type) %>%
    distinct(organisation_name, organisation_type)
  
  n_unique_collaborators <- dim(unique_organisations)[1]
  
  n_publications_with_non_educational_collaborators <- df %>%
    filter(organisation_type != "Education") %>%
    summarise(count = n_distinct(publication_id)) %>%
    unlist %>%
    unname
  
  p_publications_with_non_educational_collaborators <- df %>%
    filter(organisation_type != "Education") %>%
    summarise(count = n_distinct(publication_id)) %>%
    mutate(percent = 100 * count/n_distinct(df$publication_id)) %>%
    select(percent) %>%
    round(1) %>%
    unlist %>%
    unname
  
  n_non_educational_collaborators <- df %>%
    filter(organisation_type != "Education") %>%
    summarise(count = n_distinct(organisation_name)) %>%
    unlist %>%
    unname
  
  df_types_of_collaborators <- research_organisations %>%
    filter(organisation_name != "University of Bradford") %>%
    select(organisation_name, organisation_type) %>%
    group_by(organisation_type) %>%
    distinct(organisation_name, organisation_type) %>%
    tally(n = "Organisations") %>%
    ungroup() %>%
    mutate(Percent = round(100 * Organisations/sum(Organisations), 1))
  
  
  outputs_in_sample_by_type_of_collaborator <- research_organisations %>%
    filter(organisation_name != "University of Bradford") %>%
    group_by(organisation_type) %>%
    distinct(publication_id, organisation_type) %>%
    tally(n = "Outputs") %>% 
    ungroup()
  
  outputs_in_sample_by_type_of_collaborator <- inner_join(df_types_of_collaborators, outputs_in_sample_by_type_of_collaborator, by = "organisation_type") %>%
    rename(`Organisation type` = organisation_type)
  
  output_list <- list("unique_organisations" = unique_organisations,
                      "n_unique_collaborators" = n_unique_collaborators,
                      "n_publications_with_non_educational_collaborators" = n_publications_with_non_educational_collaborators,
                      "p_publications_with_non_educational_collaborators" = p_publications_with_non_educational_collaborators,
                      "n_non_educational_collaborators" = n_non_educational_collaborators,
                      "outputs_in_sample_by_type_of_collaborator" = outputs_in_sample_by_type_of_collaborator,
                      "df_types_of_collaborators" = df_types_of_collaborators)
  
}

collab_summary_plots <- function(df, discrete_pal){
  
  collab_type_plot <- df %>%
    group_by(year, collaboration) %>%
    tally() %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(prop = n/sum(n)) %>%
    ggplot(aes(x = year, y = prop, fill = fct_rev(collaboration))) +
    geom_bar(aes(text = paste0(collaboration, ": ", round(100 * prop, 1), "%")),
             stat="identity") +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name = "Outputs", 
                       breaks = seq(0, 1, 0.2), 
                       labels = scales::percent) +
    scale_fill_manual(name = "Type of research collaboration",
                      values = discrete_pal[c(9, 6, 3, 1)],
                      guide = (guide_legend(title.position = "top", reverse = TRUE))) +
    theme_light() +
    theme(axis.line.x = element_line(colour = "gray"),
          legend.position = "bottom",
          legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 10),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          text = element_text(size = 10))
  
  collab_type_cum <- df %>%
    filter(!is.na(year)) %>%  # This will need to be removed later 
    distinct(publication_id, year, collaboration) %>%
    group_by(year, collaboration) %>%
    tally() %>%
    ungroup() %>%
    complete(year, collaboration, fill = list(n = 0)) %>%
    group_by(collaboration) %>%
    filter(sum(n) > 0) %>%
    mutate(cumulative_sum = cumsum(n)) %>%
    ggplot(aes(x = year, y = cumulative_sum, fill = fct_rev(collaboration))) +
    geom_bar(aes(text = paste0(collaboration, ": ", cumulative_sum)),
                 stat = "identity") + 
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name = "Outputs") + 
    scale_fill_manual(name = "Type of research collaboration",
                      values = discrete_pal[c(9, 6, 3, 1)],
                      guide = (guide_legend(title.position = "top", reverse = TRUE))) +
    theme_light() +
    theme(axis.line.x = element_line(colour = "gray"),
          legend.position = "bottom",
          legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 10),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          text = element_text(size = 10))
  
  output_list <- list("collab_type_plot" = collab_type_plot,
                      "collab_type_cum" = collab_type_cum)
  
}

collab_countries_plots <- function(df, discrete_pal, k = 15){
  
  collab_countries_barplot <- df %>%
    filter(rank <= k) %>%
    ggplot(aes(x = reorder(region, -frequency), y = frequency)) +
    geom_col(fill = discrete_pal[9]) +
    geom_text(aes(label = frequency), hjust = -0.5, size = 3) +
    scale_x_discrete(name = NULL, limits = rev) +
    scale_y_continuous(name = NULL, expand = expansion(mult = c(0, 0.1))) +
    coord_flip() +
    theme_light() +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          text = element_text(size = 10),
          panel.border = element_blank(),
          panel.grid = element_blank())
  
  output_list <- list("collab_countries_barplot" = collab_countries_barplot)
  
}

collab_orgs_plots <- function(df, unique_organisations, discrete_pal, k = 15){
  
  df_org_types_colours <- org_colours(discrete_pal)
  
  top_k_orgs <- df %>%
    filter(organisation_name != "University of Bradford") %>%
    group_by(organisation_name) %>%
    tally(name = "count") %>%
    ungroup() %>%
    arrange(count) %>%
    top_n(k, count) %>%
    mutate(organisation_name = factor(organisation_name, levels = unique(organisation_name)),
           organisation_type = VLOOKUP(organisation_name, unique_organisations, organisation_name, organisation_type),
           colour = VLOOKUP(organisation_type, df_org_types_colours, org_type, colour))
  
  top_k_orgs_plot <- ggplot(data = top_k_orgs, 
                            aes(x = organisation_name, y = count, fill = colour)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = count), hjust = -0.5, size = 3) +
    scale_x_discrete(name = NULL,
                     labels = function(x) stringr::str_wrap(x, width = 32)) +
    scale_y_continuous(name = NULL, 
                       expand = expansion(mult = c(0, 0.1))) +
    scale_fill_identity(name = "Type of research organisation",
                        labels = df_org_types_colours$org_type,
                        breaks = df_org_types_colours$colour,
                        guide = (guide_legend(title.position = "top"))) +
    coord_flip() +
    theme_light() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "bottom",
          legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 10),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          text = element_text(size = 10))
  
  collab_org_types_plot <- df %>%
    filter(organisation_name != "University of Bradford") %>%
    group_by(organisation_type) %>%
    summarise(count = n_distinct(organisation_name)) %>%
    mutate(colour = VLOOKUP(organisation_type, df_org_types_colours, org_type, colour)) %>%
    ggplot(aes(x = organisation_type, y = count, fill = colour)) +
    geom_col() +
    geom_text(aes(label = count), hjust = -0.5, size = 3) +
    scale_x_discrete(name = NULL, limits = rev) +
    scale_y_continuous(name = NULL, expand = expansion(mult = c(0, 0.1))) +
    scale_fill_identity() +
    coord_flip() +
    theme_light() +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          text = element_text(size = 10),
          panel.border = element_blank(),
          panel.grid = element_blank())
  
  outputs_by_org_type_plot <- df %>%
    filter(organisation_name != "University of Bradford") %>%
    distinct(publication_id, year, organisation_type) %>%
    group_by(year, organisation_type) %>%
    tally() %>%
    ungroup() %>%
    ggplot(aes(x = year, y = n, fill = fct_rev(organisation_type),
               text = paste0("Type: ", organisation_type,
                             "\nOutputs: ", n))) +
    geom_bar(stat = "identity") +
    scale_x_continuous(name = NULL) +
    scale_y_continuous(name = "Outputs",
                       # limits = c(0, 300),
                       # breaks = seq(0, 300, 50)
                       ) +
    scale_fill_manual(name = "Organisation type",
                      values = rev(discrete_pal),
                      guide = (guide_legend(title.position = "top", 
                                            title.hjust = 0, reverse = TRUE))) +
    my_theme() +
    theme(axis.line.x = element_line(colour = "gray"),
          legend.position = "bottom",
          legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 10),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          text = element_text(size = 10))
  
  cum_outputs_by_org_type_plot <- df %>%
    filter(organisation_name != "University of Bradford") %>%
    distinct(publication_id, year, organisation_type) %>%
    group_by(year, organisation_type) %>%
    tally() %>%
    ungroup() %>%
    complete(year, organisation_type, fill = list(n = 0)) %>%
    group_by(organisation_type) %>%
    mutate(cumulative_sum = cumsum(n)) %>%
    ggplot(aes(x = year, y = cumulative_sum, fill = fct_rev(organisation_type),
               text = paste0("Type: ", organisation_type, 
                             "\nCumualtive outputs: ", cumulative_sum))) +
    geom_bar(stat = "identity") +
    scale_x_continuous(name = NULL) +
    scale_y_continuous(name = "Collaborations") +
    scale_fill_manual(name = "Organisation type",
                      values = rev(discrete_pal),
                      guide = (guide_legend(title.position = "top", 
                                            title.hjust = 0, reverse = TRUE))) +
    my_theme() +
    theme(axis.line.x = element_line(colour = "gray"),
          legend.position = "bottom",
          legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 10),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          text = element_text(size = 10))
  
  output_list <- list("top_k_orgs_plot" = top_k_orgs_plot,
                      "collab_org_types_plot" = collab_org_types_plot,
                      "outputs_by_org_type_plot" = outputs_by_org_type_plot,
                      "cum_outputs_by_org_type_plot" = cum_outputs_by_org_type_plot)
  
}

educational_collab_summary <- function(df){
  
  n_educational_collaborators <- df %>%
    filter(organisation_name != "University of Bradford") %>%
    filter(organisation_type == "Education") %>%
    summarise(count = n_distinct(organisation_name)) %>%
    unlist %>%
    unname
  
  n_uk_educational_collaborators <- df %>%
    filter(organisation_name != "University of Bradford") %>%
    filter(country_name == "United Kingdom") %>%
    filter(organisation_type == "Education") %>%
    summarise(count = n_distinct(organisation_name)) %>%
    unlist %>%
    unname
  
  n_int_educational_collaborators <- df %>%
    filter(country_name != "United Kingdom") %>%
    filter(organisation_type == "Education") %>%
    summarise(count = n_distinct(organisation_name)) %>%
    unlist %>%
    unname
  
  uk_educational_collab <- df %>%
    filter(organisation_name != "University of Bradford") %>%
    filter(organisation_type == "Education") %>%
    filter(country_name == "United Kingdom") %>%
    group_by(organisation_name) %>%
    tally(name = "count") %>%
    ungroup() %>%
    arrange(desc(count)) %>%
    relocate(count) %>% 
    pivot_wider(names_from = count, values_from = organisation_name, values_fn = list) %>%
    t() %>% data.frame %>% 
    rename(Organisations = 1) %>%
    rownames_to_column(var = "Outputs") %>%
    rowwise() %>%
    mutate(Organisations = toString(Organisations)) %>%
    ungroup()
  
  int_educational_collab <- df %>%
    filter(organisation_type == "Education") %>%
    filter(country_name != "United Kingdom") %>%
    group_by(organisation_name) %>%
    tally(name = "count") %>%
    ungroup() %>%
    arrange(desc(count)) %>%
    relocate(count) %>% 
    # filter(count > 2) %>%
    pivot_wider(names_from = count, values_from = organisation_name, values_fn = list) %>%
    t() %>% data.frame %>% 
    rename(Organisations = 1) %>%
    rownames_to_column(var = "Outputs") %>%
    rowwise() %>%
    mutate(Organisations = toString(Organisations)) %>%
    ungroup()
  
  output_list = list("n_educational_collaborators" = n_educational_collaborators,
                     "n_uk_educational_collaborators" = n_uk_educational_collaborators,
                     "n_int_educational_collaborators" = n_int_educational_collaborators,
                     "uk_educational_collab" = uk_educational_collab,
                     "int_educational_collab" = int_educational_collab)
  
}

non_ed_collab_summary <- function(df, discrete_pal, unique_organisations){
  
  df_org_types_colours <- org_colours(discrete_pal)
  
  n_uk_non_educational_collaborators <- df %>%
    filter(country_name == "United Kingdom") %>%
    filter(organisation_type != "Education") %>%
    summarise(count = n_distinct(organisation_name)) %>%
    unlist %>%
    unname
  
  n_int_non_educational_collaborators <- df %>%
    filter(country_name != "United Kingdom") %>%
    filter(organisation_type != "Education") %>%
    summarise(count = n_distinct(organisation_name)) %>%
    unlist %>%
    unname
  
  # df_int_non_edu_collab <- df %>%
  #   filter(organisation_type != "Education") %>%
  #   filter(country_name != "United Kingdom") %>%
  #   group_by(organisation_name) %>%
  #   tally(name = "count") %>%
  #   ungroup() %>%
  #   arrange(count) %>%
  #   mutate(organisation_name = factor(organisation_name, levels = unique(organisation_name)),
  #          organisation_type = VLOOKUP(organisation_name, unique_organisations, organisation_name, organisation_type),
  #          colour = VLOOKUP(organisation_type, df_org_types_colours, org_type, colour))
  # 
  # df_uk_orgs <- df %>%
  #   filter(organisation_type != "Education") %>%
  #   filter(country_name == "United Kingdom") %>%
  #   group_by(organisation_name) %>%
  #   tally(name = "count") %>%
  #   ungroup() %>%
  #   arrange(count) %>%
  #   mutate(organisation_name = factor(organisation_name, levels = unique(organisation_name)),
  #          organisation_type = VLOOKUP(organisation_name, unique_organisations, organisation_name, organisation_type),
  #          colour = VLOOKUP(organisation_type, df_org_types_colours, org_type, colour)) 
  
  df_uk_orgs <- research_organisations %>%
    filter(organisation_type != "Education") %>%
    filter(country_name == "United Kingdom") %>%
    group_by(organisation_type, organisation_name) %>%
    tally(name = "count") %>%
    ungroup() %>%
    arrange(desc(count)) %>%
    relocate(count) %>% 
    pivot_wider(names_from = count, values_from = organisation_name, values_fn = list) 
  
  df_uk_non_ed_orgs <- data.frame()
  
  for (i in seq_along(df_uk_orgs$organisation_type)) {
    
    df_temp <- df_uk_orgs %>%
      filter(organisation_type == organisation_type[i]) %>%
      t() %>% 
      data.frame %>% 
      rename(Organisations = 1) %>%
      rownames_to_column(var = "Outputs") %>%
      rowwise() %>%
      mutate(Organisations = toString(Organisations)) %>%
      ungroup() %>%
      filter(Organisations != "") %>%
      mutate(organisation_type = rep(df_uk_orgs$organisation_type[i], length(Organisations))) %>%
      relocate(organisation_type) %>%
      slice(-1)
    
    df_uk_non_ed_orgs <- rbind.data.frame(df_uk_non_ed_orgs, df_temp)
    
  }
  
  df_int_orgs <- research_organisations %>%
    filter(organisation_type != "Education") %>%
    filter(country_name != "United Kingdom") %>%
    group_by(organisation_type, organisation_name) %>%
    tally(name = "count") %>%
    ungroup() %>%
    arrange(desc(count)) %>%
    relocate(count) %>% 
    pivot_wider(names_from = count, values_from = organisation_name, values_fn = list)
  
  df_int_non_ed_orgs <- data.frame()
  
  for (i in seq_along(df_int_orgs$organisation_type)) {
    
    df_temp <- df_int_orgs %>%
      filter(organisation_type == organisation_type[i]) %>%
      t() %>% 
      data.frame %>% 
      rename(Organisations = 1) %>%
      rownames_to_column(var = "Outputs") %>%
      rowwise() %>%
      mutate(Organisations = toString(Organisations)) %>%
      ungroup() %>%
      filter(Organisations != "") %>%
      mutate(organisation_type = rep(df_int_orgs$organisation_type[i], length(Organisations))) %>%
      relocate(organisation_type) %>%
      slice(-1)
    
    df_int_non_ed_orgs <- rbind.data.frame(df_int_non_ed_orgs, df_temp)
    
  }
  
  output_list = list("n_uk_non_educational_collaborators" = n_uk_non_educational_collaborators,
                     "n_int_non_educational_collaborators" = n_int_non_educational_collaborators,
                     # "df_int_non_edu_collab" = df_int_non_edu_collab,
                     # "df_uk_orgs" = df_uk_orgs
                     "df_int_non_ed_orgs" = df_int_non_ed_orgs,
                     "df_uk_non_ed_orgs" = df_uk_non_ed_orgs)
  
}

non_ed_collab_outputs <- function(df, df_output_growth){
  
  df %>%
    filter(organisation_type != "Education") %>%
    distinct(year, publication_id) %>%
    group_by(year) %>%
    tally(n = "non_ed_collab") %>%
    ungroup() %>%
    inner_join(., df_output_growth, by = "year") %>%
    select(year, non_ed_collab, total) %>%
    adorn_totals(., where = "row") %>%
    mutate(p = non_ed_collab/total,
           Percent = paste0(round(100 * p, 1), "%")) %>%
    rename(Year = year, 
           `Outputs with at least one non-educational collaborator` = non_ed_collab, 
           `Total outputs` = total)
  
}

non_ed_outputs_plot <- function(df, discrete_pal){
  
  df %>%
    filter(Year != "Total") %>%
    mutate(Year = year(as.Date(as.character(Year), format = "%Y"))) %>% 
    ggplot(aes(x = Year, y = p)) +
    geom_bar(stat = "identity", fill = discrete_pal[9]) +
    scale_x_continuous(name = NULL) +
    scale_y_continuous(name = NULL, labels = scales::percent) +
    theme_minimal() +
    theme(text = element_text(size = 14))
  
}

non_ed_orgs_plot <- function(df, discrete_pal){
  
  df_org_types_colours <- org_colours(discrete_pal)
  
  ggplot(data = df, aes(x = organisation_name, y = count, fill = colour)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = count), hjust = -0.5, size = 3) +
    scale_x_discrete(name = NULL) + 
    scale_y_continuous(name = NULL, 
                       expand = expansion(mult = c(0, 0.1))) +
    scale_fill_identity(name = "Type of research organisation",
                        labels = df_org_types_colours$org_type,
                        breaks = df_org_types_colours$colour,
                        guide = (guide_legend(title.position = "top", 
                                              nrow = 2, byrow = FALSE))) +
    coord_flip() +
    theme_light() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "bottom",
          legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 10),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          text = element_text(size = 10))
  
}
