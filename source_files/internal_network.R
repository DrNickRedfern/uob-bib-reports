pacman::p_load(GGally, network)
source(here("source_files", "df_to_matrix.R"))

#' Preformat network data for internal collaboration analysis
#'
#' This function takes edge data, a staff list, and a target faculty, and preprocesses the data for
#' internal collaboration analysis. It performs the following steps:
#'
#' 1. Adds faculty and unique ID information for each researcher based on the staff list.
#' 2. Filters out any rows with missing faculty information.
#' 3. Adds full name information for each researcher based on the staff list.
#' 4. Aggregates the data by the non-pub_id columns, summing the pub_id values.
#' 5. Deduplicates the data.
#' 6. Filters the data to only include rows where either the researcher_x_faculty or researcher_y_faculty
#'    matches the target faculty.
#'
#' @param edge_data A data frame containing the edge data (e.g., researcher IDs and publication counts).
#' @param staff_list A data frame containing the staff list (e.g., researcher IDs, faculty, and full names).
#' @param target_faculty The faculty to focus the analysis on.
#' @return A preprocessed data frame ready for internal collaboration analysis.
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

#' Calculate total connections and connectors for each researcher
#'
#' This function takes a data frame of network data and calculates the total number of connections and unique connectors for each researcher.
#'
#' @param df A data frame containing the network data, with columns for `researcher_x`, `researcher_y`, and `pub_id`.
#' @return A data frame with columns for `researcher_x`, `connections`, and `connectors`.
total_connections <- function(df){
  
  df %>%
    group_by(researcher_x) %>% 
    summarise(connections = sum(pub_id),
              connectors = length(unique(researcher_y))) %>% 
    ungroup()
  
}

#' Summarize internal collaboration data
#'
#' This function takes a data frame of internal collaboration data and summarizes it by faculty. It calculates the number of collaborative pairs, total collaborations, minimum, median, mean, and maximum collaborations, as well as the number of collaborating staff members and the average number of connections per staff member.
#'
#' @param df A data frame containing the internal collaboration data, with columns for `researcher_x_faculty`, `researcher_y_faculty`, and `pub_id`.
#' @return A data frame with summarized collaboration statistics, including `Collaborative Pairs`, `Total Collaborations`, `Min`, `Median`, `Mean`, `Max`, `Collaborating Faculty`, `Collaborating Staff`, and `Connections Per Staff Member`.
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

#' Format a data frame into a matrix representation of a network
#'
#' This function takes a data frame containing network data and converts it into a matrix representation. 
#' The matrix has rows and columns representing the researchers, and the values in the matrix represent the number of collaborations between each pair of researchers.
#' The function filters out any rows where the number of collaborations is NA.
#'
#' @param df A data frame containing network data, with columns for `researcher_x`, `researcher_y`, and `pub_id`.
#' @return A data frame with columns `row`, `col`, and `val`, representing the matrix elements.
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

#' Format network data for visualization
#'
#' This function takes network data and a data frame from a matrix, and formats the data for plotting a network visualization using ggnet2.
#' It calculates the coordinates for the nodes and edges, and adds additional metadata to the data frames.
#'
#' @param network_data A data frame containing the network data, including researcher names, faculties, and collaboration counts.
#' @param df_from_matrix A data frame containing the matrix representation of the network.
#' @return A list containing two data frames: `dat_points` for the node data, and `dat_lines` for the edge data.
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

#' Plot an internal network visualization
#'
#' This function takes a list of network plotting data and generates a ggplot2 visualization of the internal network.
#' The visualization includes nodes representing researchers, colored by their faculty, and edges representing collaborations between researchers.
#' The size of the nodes is proportional to the total number of collaborations for each researcher.
#'
#' @param network_plotting_data A list containing the `dat_points` and `dat_lines` data frames required to plot the network.
#' @return A ggplot2 object representing the internal network visualization.
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