subject_network <- function(df, palette){
  
  df <- df %>%
    rename(for_2020_name = for_2020) %>%
    mutate(for_2020 = paste0(for_2020_code, " ", for_2020_name)) %>%
    filter(!is.na(for_2020_name)) %>%
    filter(level == "level_1")
  
  for_2020_freq <- df %>%
    group_by(for_2020) %>%
    summarise(n = n_distinct(publication_id)) %>%
    ungroup() %>%
    rename()
  
  df <- df %>%
    select(publication_id, for_2020) %>%
    group_by(publication_id) %>%
    mutate(cat = paste0("cat_", row_number())) %>%
    ungroup() %>%
    pivot_wider(id_cols = c("publication_id"), names_from = "cat", values_from = "for_2020")
  
  my_matrix <- crossprod(table(cbind(df[1], stack(df[-1]))[-3]))
  
  ind <- which(upper.tri(my_matrix, diag = FALSE), arr.ind = TRUE)
  matrix_names <- dimnames(my_matrix)
  df_from_matrix <- data.frame(row = matrix_names[[1]][ind[, 1]],
                               col = matrix_names[[2]][ind[, 2]],
                               val = my_matrix[ind]) %>%
    filter(!is.na(val))
  
  set.seed(1234)
  network_plot <- ggnet2(my_matrix, label = TRUE, label.size = 2.5)
  
  dat_points <- network_plot$data %>%
    select(label, x, y) %>%
    rename(for_2020 = label,
           coord_x = x, 
           coord_y = y) %>%
    inner_join(., for_2020_freq, by = "for_2020")
  
  dat_lines <- network_plot$layers[[1]]$data %>%
    mutate(for_2020_x = VLOOKUP(X1, dat_points, coord_x, for_2020),
           for_2020_y = VLOOKUP(X2, dat_points, coord_x, for_2020)) %>%
    left_join(., df_from_matrix, by = c("for_2020_x" = "row", "for_2020_y" = "col")) %>%
    mutate(m_x = 0.5 * (X1 + X2),
           m_y = 0.5 * (Y1 + Y2)) %>%
    filter(!is.na(val))
  
  palette <- continuous_pal(length(dat_points$for_2020))
  
  output <- ggplot() +
    geom_segment(data = dat_lines, aes(x = X1, y = Y1, xend = X2, yend = Y2, 
                                       linewidth = val),
                 colour = "gray80") +
    geom_point(data = dat_lines, aes(x = m_x, y = m_y,
                                     text = paste0(for_2020_x, " -- ", for_2020_y, "<br>",
                                                   "Outputs: ", val)),
               alpha = 0) +
    geom_point(data = dat_points, aes(x = coord_x, y = coord_y, 
                                      fill = for_2020,
                                      size = n,
                                      text = paste0(for_2020, "<br>",
                                                    "Total outputs: ", n)),
               alpha = 0.8,
               shape = 21, 
               colour = "black") +
    geom_text(data = dat_points, 
              aes(x = coord_x, y = coord_y + 0.0175, label = for_2020), 
              size = 2.5) +
    scale_fill_manual(values = palette) +
    theme_void() +
    theme(axis.line = element_blank(),
          legend.position = "none")
  
  ggplotly(output, tooltip = "text")
  
}


