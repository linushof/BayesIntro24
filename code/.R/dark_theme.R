library(tidyverse)

dark_theme <- function(base_size = 20, base_family = "mono") {
  theme_void() +
    theme(
      panel.background = element_rect(fill = "black", color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y =  element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.line = element_line(color = "white"),
      axis.text = element_text(color = "white"),
      axis.title = element_text(color = "white"),
      legend.title = element_text(color = "white"),
      legend.text = element_text(color = "white"),
      plot.title = element_text(color = "white"),
      plot.subtitle = element_text(color = "white"),
      plot.caption = element_text(color = "white"),
      strip.text = element_text(color = "white")
    )
}