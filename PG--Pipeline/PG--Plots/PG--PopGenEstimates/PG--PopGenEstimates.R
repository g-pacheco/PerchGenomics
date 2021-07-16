### The BEGINNING ~~~~
##
# ~ Plots PG--PopGenEstimates | Initial code by Marie-Christine RUFENER & George PACHECO.


# Cleans the environment ~ 
rm(list=ls())


# Sets working directory ~
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Loads required packages ~
pacman::p_load(scales, extrafont, dplyr, grid, lubridate, cowplot, egg, tidyverse, stringr, reshape)


# Imports extra fonts ~
loadfonts(device = "win", quiet = TRUE)


# Load helper functions (to be used along the script)
source("utilities.R")


# Loads the data ~
PopGen <- read.table("AfterChrGenome_PGP--GoodSamples_DoSaf-WithWrapper-DoThetas-NoWrapper_ForGenPop.PopGenSummary.txt", sep = "\t", header = FALSE); head(PopGen)
Hets <- read.table("AfterChrGenome_PGP--GoodSamples_RefGen_OutGroup_Sites--Article--Ultra.Heterozygosity.txt", sep = "\t", header = FALSE); head(Hets)


# Adds column names ~
colnames(PopGen) <- c("Population", "NSites", "Nucleotide_Diversity", "Watson_Theta", "Tajima_D")
colnames(Hets) <- c("Sample_ID", "Population", "Het")


# Removes unwanted populations ~
UnwantedPops <- c("Pflavescens", "Pfluviatilis")
Hets <- filter(Hets, !Population %in% UnwantedPops)


# Converts DF from wide into long (necessary for ggplot) ~
PopGenUp <- gather(PopGen, Estimate, Value, "Nucleotide_Diversity", "Watson_Theta", "Tajima_D")


# Adds data ID column to each DF ~
PopGenUp$ID <- factor(paste("PopGen"))
Hets$ID <- factor(paste("Hets"))


# Binds the 2 DFs ~
fulldf <- mybind(PopGenUp, Hets)


# Includes label for empty factor level (related to PHS) ~
idx <- which(fulldf$ID == "Hets")
fulldf[idx,"Estimate"] <- rep("PHS", length(idx))
fulldf$Estimate <- factor(fulldf$Estimate) #Set to factor for plotting


# Reorders factor levels ~
fulldf$Estimate <-
  factor(fulldf$Estimate, ordered = T, levels = c("PHS",
                                                  "Nucleotide_Diversity",
                                                  "Tajima_D",
                                                  "Watson_Theta"))


# Corrects the facet labels ~
ylabel <- c("Nucleotide_Diversity" = "Nucelotide Diversity",
            "Tajima_D"= "Tajima's D",
            "Watson_Theta" = "Watson's Theta",
            "PHS"= "Heterozygosity")


# Reorders populations ~
fulldf$Population <- factor(fulldf$Population, ordered = T,
                            levels = c("TAN-F", "RAN-B", "FAR-F", "SJA-F" ,"SON-F", "TYB-F",
                                       "POL-BF", "ROS-B", "KET-B", "NAK-B", "KAR-B", "ISH-B"))


# Creates the panel ~
PopGennEstimates <- 
 ggplot() +
  geom_boxplot(data = subset(fulldf, ID == "Hets"),
               aes(x = Population, y = Het, fill = Population), show.legend = FALSE, outlier.colour = "black", outlier.shape = 21, outlier.size = 1.85, width = .3, lwd = .3) +
  geom_point(data = subset(fulldf, Estimate == "Nucleotide_Diversity"),
             aes(x = Population, y = Value, fill = Population), colour = "black", shape = 21, size = 3.5, alpha = .9) +
  facet_grid(Estimate ~. , scales = "free", labeller = labeller(Estimate = ylabel)) +
  #scale_fill_manual(values = c("#44AA99", "#F0E442", "#E69F00", "#56B4E9")) +
  #scale_colour_manual(values = c("#44AA99", "#F0E442", "#E69F00", "#56B4E9")) +
  theme(panel.background = element_rect(fill = "#ffffff"),
        panel.grid.major.x = element_line(color = "#ededed", linetype = "dashed", size = .00005),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "#000000", size = .3),
        axis.title = element_blank(),
        axis.text.x = element_text(colour="#000000", size = 16, face = "bold", family = "Helvetica", angle = 90, vjust = .5, hjust = 1),
        axis.text.y = element_text(color="#000000", size = 16, family = "Helvetica"),
        axis.ticks.x = element_line(color="#000000", size = .3),
        axis.ticks.y = element_line(color="#000000", size = .3),
        strip.background.y = element_rect(colour = "#000000", fill = "#d6d6d6", size = 0.3),
        strip.text = element_text(colour = "#000000", size = 12, face = "bold", family = "Georgia"),
        legend.position = "right",
        legend.key = element_rect(fill = NA),
        legend.background =element_blank()) +
  guides(fill = guide_legend(title = "Populations", title.theme = element_text(size = 16, face = "bold", family = "Helvetica"),
                             label.theme = element_text(size = 14, family = "Helvetica"),
                             override.aes = list(size = 5, alpha = .9)), colour = "none")


# Saves the panel ~
ggsave(PopGennEstimates, file = "PG--PopGenEstimates.pdf", device = cairo_pdf, width = 10, height = 5, scale = 1.75, dpi = 600)


#
##
### The END ~~~~