my_theme <- function(){ 
  
  theme_light() %+replace%    #replace elements we want to change
    
    theme(
      
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(colour = "gray"),
      #text = element_text(size = 12),  # changed to 12 for html, but it's 10 for pdf
      legend.key.size = unit(0.5, "cm"),
      #legend.text = element_text(size = 9),
      #legend.title = element_text(size = 10),
      panel.border = element_blank()
      
    )
}