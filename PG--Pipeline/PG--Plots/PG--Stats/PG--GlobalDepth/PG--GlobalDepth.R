### The BEGINNING ~~~~~
##
# ~ Creates FPG--GlobalCoverage | By George PACHECO


# Cleans the environment ~ 
rm(list=ls())


# Sets working directory ~
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Loads required packages ~
pacman::p_load(scales, extrafont, tidyverse)


# Imports extra fonts ~
loadfonts(device = "win", quiet = TRUE)


# Loads data ~
Data <- read.table("PG--GoodSamples_IntersectedWithMerged.mean")
colnames(Data) <- c("Loci", "Coverage")
DataUp <- data.frame(Coverage = Data$Coverage, Type = "")


# Creates the plot ~
GlobalCoverage <-
 ggplot(DataUp, aes(x = Coverage, fill = Type, colour = Type)) +
  geom_density(alpha = .15, adjust = .75, size = .3) +
  scale_x_continuous("Global Depth (X)",
                     breaks = c(5000, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 50000, 55000, 60000, 65000, 70000),
                     labels = c("5K", "10K", "15K","20K", "25K", "30K", "35K", "40K", "45K", "50K", "55K", "60K", "65K","70K"),
                     expand = c(0,0),
                     limits = c(0, 71000)) +
  scale_y_continuous("Density",
                     breaks = c(0.00001, 0.00002, 0.00003, 0.00004), 
                     expand = c(0,0),
                     labels = c("1e-05", "2e-05", "3e-05", "4e-05"), 
                     limits = c(0, 0.000145)) +
  theme(panel.background = element_rect(fill = '#ffffff'),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 9, color = "#000000"),
        axis.ticks = element_line(size = .3, color = "#000000"),
        axis.line = element_line(colour = "#000000", size = .3),
        axis.title.x = element_text(size = 15, face = "bold", color = "#000000", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 15, face = "bold", color = "#000000", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        legend.position = "none")

  
# Saves plot ~
ggsave(GlobalCoverage, file = "PG--GlobalCoverage.pdf", width = 12, height = 8, device = cairo_pdf, dpi = 600)


#
##
### The END ~~~~~