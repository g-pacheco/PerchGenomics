### The BEGINNING ~~~~~
##
# ~ Plots PG--Sites-ScaffoldsRegression | By George PACHECO


# Cleans the environment ~ 
rm(list=ls())


# Sets working directory ~
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Loads required packages ~
pacman::p_load(tidyverse, extrafont)


# Imports extra fonts ~
loadfonts(device = "win", quiet = TRUE)


# Loads data ~
Data <- read.table("PG--GoodSamples_SITES.ScaffoldInfo_OnlyWithSites.txt")
colnames(Data) <- c("Scaffold", "ScaffoldLength", "NumberOfSites")


# Performs regression ~
Regression <- lm(formula = Data$ScaffoldLength ~ Data$NumberOfSites, data = Data)
summary(Regression)


# Creates plot ~
Plot <-
 ggplot(Data,aes(ScaffoldLength, NumberOfSites)) +
  stat_smooth(method = "lm") +
  geom_point(alpha = 0.7, color = "#000000", size = 1) +
  annotate("text", label = "Multiple R-squared: 0.7642", x = 27000000, y = 100000, size = 5, colour = "#FF0000") +
  annotate("text", label = "P-value: 2.386e-08", x = 27000000, y = 97000, size = 5, colour = "#FF0000") +
  scale_x_continuous("Scaffold Length",
                     breaks = c(25000000, 30000000, 35000000, 40000000, 45000000),
                     labels = c("25Mb", "30Mb", "35Mb", "40Mb", "45Mb"),
                     limits = c(21400000, 48900000),
                     expand = c(0,0)) +
  scale_y_continuous("# of Sites",
                     breaks = c(25000, 50000, 75000, 100000, 125000, 150000),
                     labels = c("25K", "50K", "75K", "100K", "125K", "150K"),
                     #limits = c(0, 153000),
                     expand = c(0,0)) +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#ffffff"),
        axis.title.x = element_text(size = 15, color = "#000000", face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 15, color = "#000000", face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.text = element_text(size = 10, color = "#000000", face = "bold"),
        axis.ticks = element_line(size = .3, color = "#000000"),
        axis.line = element_line(colour = "#000000", size = .3, color = "#000000"))


# Saves plot ~
ggsave(Plot, file = "PG--Sites-ScaffoldsRegression.pdf", device = cairo_pdf, width = 12, height = 8, dpi = 600)


#
##
### The END ~~~~~