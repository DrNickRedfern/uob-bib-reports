# Percent change function
percent_change <- function(first, last){100 * ((last - first)/first)}

# Key stats plot
key_stats_plot <- function(df, discrete_pal){
  
  scale_lower_limit = 9 - (length(df$x) - 1)
  
  ggplot(data = df, 
         aes(x, y, height = h, width = w, label = text)) +
    geom_tile(aes(fill = colour)) +
    # text for values
    geom_text(colour = "white", fontface = "bold", size = 20,
              aes(label = values, x = x - 2.9, y = y - 1.5), hjust = 0, vjust = 0) +
    # text for tile headings
    geom_text(colour = "white", fontface = "bold", size = 6, lineheight = 0.9,
              aes(label = text, x = x - 2.9, y = y + 1.4), hjust = 0) +
    coord_fixed() +
    scale_fill_manual(name = NULL, values = discrete_pal[c(scale_lower_limit:9)]) +
    theme(plot.margin = unit(c(-0.30, 0 , 0, 0), "null")) +
    theme_void() +
    guides(fill = "none")
  
}

# Map functions
world_map_data <- function(){
  
  pacman::p_load(maps)
  world <- map_data("world")
  world %>% filter(region != "Antarctica")
  
}

# TODO set additional parameters according to different uses of the map
world_map_plot <- function(df, palette, scale_name = "Scale"){
  
  ggplot() + 
    geom_polygon(data = df, aes(long, lat, group = group, fill = frequency,
                                text = paste0(region, "<br>",
                                              scale_name, ": ", frequency)), 
                 colour = "gray30", linewidth = 0.1) +
    scale_fill_distiller(palette = palette,
                         na.value = "lightgray",
                         trans = "log10",
                         name = scale_name,
                         breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000),
                         labels = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000),
                         limits = c(1, max(df$frequency)),
                         guide = guide_colourbar(barwidth = 15,
                                                 barheight = 1,
                                                 title.position = "top")) +
    coord_fixed(1.3) +
    theme(axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 10),
          text = element_text(size = 10))

}

# Organisation colours
#' Get a data frame of organization type colors
#'
#' This function returns a data frame with organization types and their corresponding colors from a discrete color palette.
#'
#' @param discrete_pal A discrete color palette to use for the organization types.
#' @return A data frame with columns 'org_type' and 'colour'.
org_colours <- function(discrete_pal) {
  data.frame(
    org_type = c("Archive", "Company", "Education", "Facility", "Government", "Healthcare", "Nonprofit", "Other"),
    colour = discrete_pal[2:9]
  )
}