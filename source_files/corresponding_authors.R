pacman::p_load(fontawesome, glue, ggpubr, ggrepel, here, janitor, plotly, tidyquant, tidyverse)

faculty <- read_csv(here("data", "faculty.csv"), show_col_types = FALSE) %>%
  mutate(full_name = paste0(first_name, " ", family_name) %>%
           str_to_title())

faculty_name_ref <- faculty %>%
  distinct(unique_id, full_name, level_3_long_desc)

researchers <- read_csv(here("data", glue("{params$project_name}_researchers.csv")), 
                        show_col_types = FALSE) %>%
  mutate(unique_id = VLOOKUP(researcher_id, faculty, researcher_id, unique_id))

df_publications <- read_csv(here("data", glue("{params$project_name}_publications_details.csv")), 
                            show_col_types = FALSE) %>%
  filter(type != "preprint") %>%
  mutate(current_affiliation = VLOOKUP(researcher_id, researchers, researcher_id, current_research_org.name),
         unique_id = VLOOKUP(researcher_id, faculty, researcher_id, unique_id),
         full_name = VLOOKUP(unique_id, faculty_name_ref, unique_id, full_name),
         school = VLOOKUP(unique_id, faculty_name_ref, unique_id, level_3_long_desc))

df_author_positions <- read_csv(here("data", 
                                     glue("{params$project_name}_author_positions.csv")),
                                show_col_types = FALSE) %>%
  mutate(unique_id = VLOOKUP(researcher_id, researchers, researcher_id, unique_id)) %>%
  filter(!is.na(unique_id))

# Corresponding authors ------------
# Will need checking against open access report to check the results are consistent
n_total_corresponding <- df_author_positions %>%
  distinct(pub_id, corresponding) %>%
  summarise(n = sum(corresponding)) %>%
  unname %>%
  unlist

n_total_corresponding

outputs_per_year_corresponding <- df_author_positions %>%
  filter(corresponding == TRUE) %>%
  group_by(year) %>%
  tally(n = "total_corresponding") %>%
  ungroup()

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
  relocate(total, .after = total_corresponding) %>%
  rename(Year = year,
         `UoB corresponding author` = total_corresponding,
         `Journal articles` = total,
         `%` = p_corresponding)

outputs_per_year_corresponding

corresponding_oa_status <- df_author_positions %>%
  distinct(corresponding, pub_id, open_access, year) %>%
  filter(corresponding == TRUE) %>%
  group_by(year, open_access) %>%
  tally() %>%
  ungroup() %>%
  mutate(open_access = str_to_title(open_access)) %>%
  group_by(year) %>%
  mutate(p = round(100 * n/sum(n), 1)) %>%
  ungroup()

corresponding_oa_status$open_access <- factor(corresponding_oa_status$open_access, 
                                              levels = c("Bronze",
                                                         "Gold", 
                                                         "Green", 
                                                         "Hybrid",
                                                         "Closed"))


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


corresponding_oa_status

n_total_corresponding

corresponding_oa_status %>%
  group_by(open_access) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  mutate(p = round(100 * n/sum(n), 1)) %>%
  adorn_totals()

total_oa_corresponding <- corresponding_oa_status %>%
  filter(open_access != "Closed") %>%
  group_by(year) %>%
  summarise(total_oa_n = sum(n),
            total_oa_p = sum(p)) %>%
  ungroup()

total_oa_corresponding %>%
  adorn_totals(., where = "row")


outputs_per_year_corresponding

oa_corresponding <- inner_join(outputs_per_year_corresponding, total_oa_corresponding, 
                               by = "year")

oa_corresponding

outputs_per_year_corresponding <- df_author_positions %>%
  distinct(corresponding, pub_id, open_access, year) %>%
  filter(corresponding == TRUE) %>%
  group_by(year) %>%
  tally(n = "total_corresponding") %>%
  ungroup() %>%
  inner_join(., df_articles, by = "year") %>% 
  #adorn_totals() %>%
  mutate(p_corresponding = round(100 * total_corresponding/total, 1)) %>%
  select(-total)

outputs_per_year_corresponding

corresponding_totals <- corresponding_oa_status %>%
  group_by(year) %>%
  summarise(total_corresponding_oa = sum(n)) %>%
  ungroup()

corresponding_totals

oa_status_corr_plot <- ggplot() +
  geom_text(data = corresponding_totals, 
            aes(x = year, y = 105, label = total_corresponding_oa), size = 3.5) +
  geom_bar(data = corresponding_oa_status, 
           aes(x = year, y = p, fill = open_access,
               text = paste0(open_access, ": ", p, "%",
                             "\nOutputs: ", n)), 
           stat = "identity", position = "stack") +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = NULL,
                     breaks = seq(0, 100, 20), 
                     minor_breaks = seq(10, 90, 10),
                     labels = scales::percent_format(scale = 1)) +
  scale_fill_viridis_d(name = "OA status", 
                       guide = (guide_legend(title.position = "top")),
                       option = "viridis") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom", 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

oa_status_corr_plot

oa_status_trend_corresponding <- function(df){
  
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
    scale_fill_viridis_d(name = "OA status", 
                         guide = (guide_legend(title.position = "top")),
                         option = "viridis") +
    theme_minimal() +
    theme(axis.text.x = element_text(vjust = 1),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "bottom", 
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
}


oa_status_corr_plot(corresponding_oa)

ggplotly(oa_status_corr_plot, tooltip = "text") %>%
  layout(legend = list(orientation = "h", x = 0.1))

# Authorship positions ------------
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
  relocate(p_first,  .after = first_author) %>%
  rename(Year = year,
         `First author` = first_author,
         `Last author` = last_author,
         `Journal articles` = total)

author_pos

n_first_author <- author_pos$n[which(author_pos$AuthorCategory == "first_author")]
n_first_author

n_last_author <- author_pos$n[which(author_pos$AuthorCategory == "last_author")]
n_last_author

