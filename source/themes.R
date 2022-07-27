#source('./source/libs.R')
source('./source/palettes.R')
library(extrafont)
library(grid) 
#font_import()  #run first time
#loadfonts(device = "win")

font <- "Helvetica" #Set font type for all fonts

theme_generic <- theme_bw() +
  theme(plot.title = element_text(family = font, size = 28, face = "bold", color = "#222222"), 
        plot.subtitle = element_text(family = font, size = 22, margin = margin(9, 0, 9, 0)), 
        plot.caption = element_blank(),
        axis.ticks.length = unit(-0.2, "cm"),  
        axis.title.x = element_text(margin = unit(c(0.3, 0, 0, 0), "cm"),
                                    family = font, size = 14, color = "#222222"), 
        axis.text.x = element_text(margin = unit(c(0.5, 0, 0, 0), "cm"), 
                                   family = font, size = 12, color = "#222222"),
        axis.title.y = element_text(margin = unit(c(0, 0.3, 0, 0), "cm"), 
                                    family = font, size = 14, color = "#222222"), 
        axis.text.y = element_text(margin = unit(c(0, 0.5, 0, 0), "cm"), 
                                   family = font, size = 12, color = "#222222"),
        legend.title = element_text(family = font, size = 12, color = "#222222"),  
        legend.text = element_text(family = font, size = 12, color = "#222222"),
        legend.key = element_blank(),
        legend.position = c("bottom"), # position the legend in the upper left 
        legend.direction = "horizontal")

theme_small <- theme_bw() +
  theme(plot.title = element_text(family = font, size = 14, face = "bold", color = "#222222"), 
        plot.subtitle = element_text(family = font, size = 10, margin = margin(9, 0, 9, 0)), 
        plot.caption = element_blank(),
        axis.ticks.length = unit(-0.2, "cm"),  
        axis.title.x = element_text(margin = unit(c(0.3, 0, 0, 0), "cm"),
                                    family = font, size = 8, color = "#222222"), 
        axis.text.x = element_text(margin = unit(c(0.5, 0, 0, 0), "cm"), 
                                   family = font, size = 8, color = "#222222"),
        axis.title.y = element_text(margin = unit(c(0, 0.3, 0, 0), "cm"), 
                                    family = font, size = 8, color = "#222222"), 
        axis.text.y = element_text(margin = unit(c(0, 0.5, 0, 0), "cm"), 
                                   family = font, size = 8, color = "#222222"),
        legend.title = element_text(family = font, size = 8, color = "#222222"),  
        legend.text = element_text(family = font, size = 8, color = "#222222"),
        legend.key = element_blank())


theme_very_small <- theme_bw() +
  theme(plot.title = element_text(family = font, size = 10, face = "bold", color = "#222222"), 
        plot.subtitle = element_text(family = font, size = 8, margin = margin(9, 0, 9, 0)), 
        plot.caption = element_blank(),
        axis.ticks.length = unit(-0.1, "cm"),  
        axis.title.x = element_text(margin = unit(c(0.3, 0, 0, 0), "cm"),
                                    family = font, size = 8, color = "#222222"), 
        axis.text.x = element_text(margin = unit(c(0.5, 0, 0, 0), "cm"), 
                                   family = font, size = 8, color = "#222222"),
        axis.title.y = element_text(margin = unit(c(0, 0.3, 0, 0), "cm"), 
                                    family = font, size = 8, color = "#222222"), 
        axis.text.y = element_text(margin = unit(c(0, 0.5, 0, 0), "cm"), 
                                   family = font, size = 8, color = "#222222"),
        legend.title = element_text(family = font, size = 8, color = "#222222"),  
        legend.text = element_text(family = font, size = 8, color = "#222222"),
        legend.key = element_blank())

theme_harvard <- theme_minimal() + # start with a minimal theme and add what we need
  theme(text = element_text(color = "gray20"),
        legend.position = c("top"), # position the legend in the upper left 
        legend.direction = "horizontal",
        legend.justification = 0.1, # anchor point for legend.position.
        legend.text = element_text(size = 11, color = "gray10"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(vjust = -1), # move title away from axis
        axis.title.y = element_text(vjust = 2), # move away for axis
        axis.ticks.y = element_blank(), # element_blank() is how we remove elements
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank())

theme_bbc <- 
  theme(plot.title = element_text(family = font, size = 28, face = "bold", color = "#222222"), 
        plot.subtitle = element_text(family = font, size = 22, margin = margin(9, 0, 9, 0)), 
        plot.caption = element_blank(),
        legend.position = "top", 
        legend.text.align = 0, 
        legend.background = element_blank(),
        legend.title = element_blank(), 
        legend.key = element_blank(),
        legend.text = element_text(family = font, size = 18, color = "#222222"), 
        axis.title = element_blank(),
        axis.text = element_text(family = font, size = 18, color = "#222222"), 
        axis.text.x = element_text(margin = margin(5, b = 10)), 
        axis.ticks = element_blank(), 
        axis.line = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#cbcbcb"),
        panel.grid.major.x = element_blank(), 
        panel.background = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 22, hjust = 0))
