pacman::p_load(GGally, network)
source(here("source_files", "df_to_matrix.R"))

pre_format_network_data <- function(edge_data, staff_list, target_faculty){
  
  edge_data %>%
    mutate(researcher_x_faculty = VLOOKUP(researcher_id_x, staff_list, researcher_id_x, faculty),
           researcher_y_faculty = VLOOKUP(researcher_id_y, staff_list, researcher_id_x, faculty),
           researcher_x_unique_id = VLOOKUP(researcher_id_x, staff_list, researcher_id_x, unique_id),
           researcher_y_unique_id = VLOOKUP(researcher_id_y, staff_list, researcher_id_x, unique_id)) %>%
    filter(!is.na(researcher_x_faculty) & !is.na(researcher_y_faculty)) %>%
    mutate(researcher_x = VLOOKUP(researcher_id_x, staff_list, researcher_id_x, full_name),
           researcher_y = VLOOKUP(researcher_id_y, staff_list, researcher_id_x, full_name)) %>%
    select(-c(researcher_id_x, researcher_id_y)) %>%
    group_by(across(c(-pub_id))) %>%
    summarise(pub_id = sum(pub_id)) %>%
    ungroup() %>%
    distinct() %>%
    filter(str_detect(.$researcher_x_faculty, target_faculty) | 
             str_detect(.$researcher_y_faculty, target_faculty))
  
}

total_connections <- function(df){
  
  df %>%
    group_by(researcher_x) %>% 
    summarise(connections = sum(pub_id),
              connectors = length(unique(researcher_y))) %>% 
    ungroup()
  
}

internal_collab_summary <- function(df) {
  
  collab_summary <- df %>%
    group_by(researcher_x_faculty, researcher_y_faculty) %>%
    summarise(collaborative_pairs  = n(), 
              total_collaborations = sum(pub_id),
              min = min(pub_id),
              median = median(pub_id),
              mean = round(mean(pub_id), 1),
              max = max(pub_id)) %>%
    ungroup() %>%
    filter(researcher_x_faculty == target_faculty)
  
  collab_staff_count <- df %>%
    group_by(researcher_x_faculty) %>%
    summarise(collaborating_staff = n_distinct(researcher_x)) %>%
    ungroup()
  
  collab_summary <- left_join(collab_summary, collab_staff_count, 
                              by = c("researcher_y_faculty" = "researcher_x_faculty")) %>%
    relocate(collaborating_staff, .after = "researcher_y_faculty") %>%
    select(-researcher_x_faculty) %>%
    rename(collaborating_faculty = researcher_y_faculty) %>%
    mutate(connections_per_staff_member = round(total_collaborations/collaborating_staff, 1)) %>%
    rename_all(str_to_title) %>%
    rename_all(~ gsub("_", " ", .))
  
}

format_matrix <- function(df) {
  
  my_matrix <- data.frame2matrix(df, "researcher_x", "researcher_y", "pub_id")
  
  ind <- which(upper.tri(my_matrix, diag = FALSE), arr.ind = TRUE)
  matrix_out <- cbind(ind, my_matrix[ind])
  
  matrix_names <- dimnames(my_matrix)
  data.frame(row = matrix_names[[1]][ind[, 1]],
             col = matrix_names[[2]][ind[, 2]],
             val = my_matrix[ind]) %>%
    filter(!is.na(val))
  
}

format_network <- function(network_data, df_from_matrix) {
  
  set.seed(2)
  network_plot <- ggnet2(df_from_matrix, label = TRUE, label.size = 2.5)
  
  dat_points <- network_plot$data %>%
    select(label, x, y) %>%
    rename(coord_x = x, coord_y = y)
  
  dat_points <- network_data %>%
    mutate(coord_x = VLOOKUP(researcher_x, dat_points, label, coord_x),
           coord_y = VLOOKUP(researcher_x, dat_points, label, coord_y)) %>%
    distinct(researcher_x, researcher_x_faculty, coord_x, coord_y)
  
  dat_lines <- network_plot$layers[[1]]$data
  
  dat_lines <- dat_lines %>%
    mutate(researcher_x = VLOOKUP(X1, dat_points, coord_x, researcher_x),
           researcher_x_faculty = VLOOKUP(X1, dat_points, coord_x, researcher_x_faculty),
           researcher_y = VLOOKUP(X2, dat_points, coord_x, researcher_x),
           researcher_y_faculty = VLOOKUP(X2, dat_points, coord_x, researcher_x_faculty)) %>% 
    left_join(., df_from_matrix, by = c("researcher_x" = "row", "researcher_y" = "col")) %>%
    mutate(m_x = 0.5 * (X1 + X2),
           m_y = 0.5 * (Y1 + Y2))
  
  all_connections <- total_connections(network_data)
  
  dat_points <- inner_join(dat_points, all_connections, by = "researcher_x")
  
  output_list = list("dat_points" = dat_points,
                     "dat_lines" = dat_lines)
  
}

internal_network_plot <- function(network_plotting_data){
  
  faculty_colours <- c("Faculty of Engineering and Digital Technologies" = "#014636",
                       "Faculty of Health Studies" = "#081D58",
                       "Faculty of Management, Law and Social Sciences" = "#800026",
                       "Faculty of Life Sciences" = "#74A9CF")
  
  dat_lines <- network_plotting_data$dat_lines
  dat_points <- network_plotting_data$dat_points
  
  ggplot() +
    geom_segment(data = dat_lines, aes(x = X1, y = Y1, xend = X2, yend = Y2, 
                                       linewidth = val/2),
                 colour = "gray80") +
    geom_point(data = dat_lines, aes(x = m_x, y = m_y,
                                     text = paste0(researcher_x, " -- ", researcher_y, "<br>",
                                                   "Collaborations: ", val)),
               alpha = 0) +
    geom_point(data = dat_points, aes(x = coord_x, y = coord_y, 
                                      colour = researcher_x_faculty,
                                      shape = researcher_x_faculty,
                                      size = connections,
                                      text = paste0(researcher_x, "<br>",
                                                    researcher_x_faculty, "<br>",
                                                    "Collaborators: ", connectors, "<br>",
                                                    "Total collaborations: ", connections)),
               alpha = 0.8) +
    geom_text(data = dat_points, 
              aes(x = coord_x, y = coord_y + 0.0175, label = researcher_x), 
              size = 2.5) +
    scale_colour_manual(name = "Faculty", values = faculty_colours) +
    scale_shape_manual(name = "Faculty", values = c(15, 16, 17, 18)) +
    guides(colour = guide_legend(nrow = 2,byrow = TRUE)) +
    scale_linewidth(guide = "none") +
    scale_size(name = "Total connections", guide ="none") +
    theme_void() +
    theme(axis.line = element_blank(),
          legend.position = "bottom")
  
}

