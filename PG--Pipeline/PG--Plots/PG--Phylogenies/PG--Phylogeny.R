### The BEGINNING ~~~~~
##
# ~ Plots FPG--PhyloData_II | By George PACHECO


# Cleans the environment ~ 
rm(list=ls())


# Sets working directory ~
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Loads required packages ~
pacman::p_load(ggtree, tidyverse, ggrepel, extrafont, treeio, ape, ggtreeExtra, ggnewscale, ggstar, reshape2)


# Load helper function ~
source("utilities.R")


# Imports extra fonts ~
loadfonts(device = "win", quiet = TRUE)


# Reads datasets ~
Data <- read.tree(file = "FPG--GoodSamples_NoSrisoriaNoCpalumbus.ngsDist.raxml.bestTree")


# Reads annotations ~
Data_annot <- read.table("FPG--GoodSamples_FPG--GoodSamples_NoSrisoriaNoCpalumbus.annot", sep = "\t", header = FALSE, stringsAsFactors = FALSE)
colnames(Data_annot) <- c("label", "Population")


# Expands the data by adding BioStatus ~
Data_annot$BioStatus <- ifelse(Data_annot$Population %in% c("Torshavn","Ejde","Sumba","LjosAir","Kunoy","Nolsoy", "Crete", "Sardinia","Vernelle","WadiHidan","PigeonIsland","Trincomalee"), "Remote_Localities_Within_Natural_Range",
                        ifelse(Data_annot$Population %in% c("Guimaraes","Lisbon","Barcelona","Berlin","Cambridge",
                                                              "Colombo","Copenhagen","London","Prague","Jihlava","Abadeh",
                                                              "Isfahan","Lahijan","Nowshahr","Tehran","TelAviv"), "Urban_Localities_Within_Natural_Range",
                        ifelse(Data_annot$Population %in% c("SaltLakeCity","Denver", "FeralVA", "FeralUT", "TlaxcalaDeXicohtencatl",
                                                              "MexicoCity","Monterrey","SanCristobalDeLasCasas","Santiago",
                                                              "Salvador","Tatui","Johannesburg","Nairobi","Perth"), "Localities_Outside_Natural_Range",
                        ifelse(Data_annot$Population %in% c("TelAvivColony","Wattala", "Wellawatte"), "Captives", NA))))


# Expands the data by adding Groups ~
Data_annot$Groups <- # Remote Localities Within Natural Range
                        ifelse(Data_annot$Population %in% c("PigeonIsland", "Trincomalee"), "Group_A",
                        ifelse(Data_annot$Population %in% c("Abadeh", "Tehran", "Crete", "Sardinia", "Vernelle", "Torshavn", "Ejde", "Sumba", "LjosAir", "Kunoy", "Nolsoy"), "Group_B",
                        ifelse(Data_annot$Population %in% c("TelAviv", "TelAvivColony", "WadiHidan"), "Group_C",
                        ifelse(Data_annot$Population %in% c("Nairobi", "Colombo", "Lahijan", "Nowshahr", "Wellawatte", "Isfahan"), "Group_D",
                        ifelse(Data_annot$Population %in% c("Guimaraes", "Barcelona", "Lisbon", "Salvador", "Tatui","Denver" , "Santiago", "TlaxcalaDeXicohtencatl", "MexicoCity", "Monterrey", "SanCristobalDeLasCasas"), "Group_E",
                        ifelse(Data_annot$Population %in% c("Jihlava", "Prague", "Berlin", "SaltLakeCity", "Johannesburg", "London", "Cambridge", "Perth", "Copenhagen"), "Group_F",
                        ifelse(Data_annot$Population %in% c("Wattala"), "Not_Grouped", NA)))))))


# Melts the annotation dataset ~
Data_annot <- melt(Data_annot)


# Defines the shapes to be used for each Group ~
Shapes <- as.vector(c(# Group A
                      29, 
                      # Group B
                      11,
                      # Group C
                      13,
                      # Group D
                      7,
                      # Group E
                      14,
                      # Group F
                      28,
                      # Not Grouped
                      9))


# Roots the phylogeny ~
Data_rooted <- root(Data, node = 521)

# 481

# Selects clades to highlight ~
groups <- list(group1 = c("Crupestris_01-WGS", "Crupestris_01-GBS"))
Data_rooted <- groupOTU(Data_rooted, groups)

groups <- list(group1 = c("Crupestris_01-WGS", "Crupestris_01-GBS"))
Data <- groupOTU(Data, groups)


# Reorders BioStatus ~
Data_annot$BioStatus <- factor(Data_annot$BioStatus, ordered = T,
                        levels = c("Remote_Localities_Within_Natural_Range",
                                   "Urban_Localities_Within_Natural_Range",
                                   "Localities_Outside_Natural_Range",
                                   "Captives"))


# Reorders Groups ~
Data_annot$Groups <- factor(Data_annot$Groups, ordered = T,
                     levels = c("Group_A",
                                "Group_B",
                                "Group_C",
                                "Group_D",
                                "Group_E",
                                "Group_F",
                                "Not_Grouped"))

# Corrects groups ~
#levels(Data2_annot$Groups <- sub("Torshavn", "Tórshavn", fulldf$Population))
#levels(Data2_annot$Groups <- sub("WadiHidan", "Wadi Hidan", fulldf$Population))
#levels(Data2_annot$Groups <- sub("Tatui", "Tatuí", fulldf$Population))
#levels(Data2_annot$Groups <- sub("PigeonIsland", "Pigeon Island", fulldf$Population))
#levels(Data2_annot$Groups <- sub("Guimaraes", "Guimarães", fulldf$Population))


# Creates base phylogeny ~
basePhylo2 <-
  ggtree(Data_rooted, layout = "fan", aes(colour = group), size = .125) +
  scale_colour_manual(labels = c("Columba livia", "Columba rupestris"), values = c("#000000", "#fb8072"))


# Merges annotation to base phylogeny ~
basePhylo_annot <- basePhylo2 %<+% Data_annot


# Creates final phylogeny ~
Plot <-
 basePhylo_annot +
  #geom_fruit(geom = geom_tile, mapping = aes(fill = BioStatus), alpha = .9, colour = NA, offset = .05, width = 0.004, show.legend = FALSE) +
  geom_tiplab(align = TRUE, linesize = .02, size = 1.5, show.legend = FALSE) +
  geom_point2(aes(label = label, subset = !is.na(as.numeric(label)) & as.numeric(label) > 70), shape = 21, size = 1.25, fill = "#155211", colour = "#155211", alpha = .9, stroke = .07) +
  geom_star(mapping = aes(fill = BioStatus, starshape = Groups), size = 1.25, starstroke = .07) +
  scale_fill_manual(values = c("#44AA99", "#F0E442", "#E69F00", "#56B4E9"), labels = gsub("_", " ", levels(Data_annot$BioStatus)), na.translate = FALSE) +
  scale_starshape_manual(values = Shapes, labels = gsub("_", " ", levels(Data_annot$Groups)), na.translate = FALSE) +
  #geom_treescale(x = 12, y = 12, label = "Scale", fontsize = 4, offset.label = 4, family = "Helvetica") +
  #geom_text(aes(label = node), size = 1, hjust = -.3) +
  theme(panel.spacing = margin(t = 0, b = 0, r = 0, l = 0),
        plot.margin = margin(t = 0, b = 0, r = 0, l = 0),
        legend.position = c(.11, .875),
        legend.spacing.y = unit(.25, "cm"),
        legend.key.height = unit(.35, "cm"),
        legend.margin = margin(t = 0, b = 0, r = 0, l = 0),
        legend.box.margin = margin(t = 5, b = -20, r = 0, l = 0)) +
  guides(colour = guide_legend(title = "Species", title.theme = element_text(size = 11, face = "bold", family = "Helvetica"),
                               label.theme = element_text(size = 8, family = "Helvetica", face = "italic"), override.aes = list(size = .8, starshape = NA), order = 1),
         fill = guide_legend(title = "Biological Status", title.theme = element_text(size = 11, face = "bold", family = "Helvetica"),
                             label.theme = element_text(size = 8, family = "Helvetica"),
                             override.aes = list(starshape = 21, size = 2.85, alpha = .9, starstroke = .0015), order = 2),
         starshape = guide_legend(title = "Groups", title.theme = element_text(size = 11, face = "bold", family = "Helvetica"),
                                  label.theme = element_text(size = 8, family = "Helvetica"),
                                  override.aes = list(starshape = Shapes, size = 2.85, starstroke = .15), order = 3))


# Saves plot ~
ggsave(Plot, file = "FPG--PhyloData_II.pdf", device = cairo_pdf, width = 12, height = 12, dpi = 600)


# Tel Aviv Colony ~
collapsePhylo2_1 <-
 Plot %>% collapse(node = 588, 'max', fill = "#56B4E9", alpha = .7)

# Faroe Islands ~
collapsePhylo2_2 <-
 collapsePhylo2_1 %>% collapse(node = 526, 'max', fill = "#44AA99", alpha = .7)

# Barcelona ~
collapsePhylo2_3 <-
 collapsePhylo2_2 %>% collapse(node = 900, 'max', fill = "#F0E442", alpha = .7)

# Salvador ~
collapsePhylo2_4 <-
 collapsePhylo2_3 %>% collapse(node = 886, 'max', fill = "#E69F00", alpha = .7)

# Tatuí ~
collapsePhylo2_5 <-
 collapsePhylo2_4 %>% collapse(node = 867, 'max', fill = "#E69F00", alpha = .7)

# Santiago ~
collapsePhylo2_6 <-
 collapsePhylo2_5 %>% collapse(node = 851, 'max', fill = "#E69F00", alpha = .7)

# Monterrey ~
collapsePhylo2_7 <-
  collapsePhylo2_6 %>% collapse(node = 837, 'max', fill = "#E69F00", alpha = .7)

# San Cristóbal de las Casas ~
collapsePhylo2_8 <-
  collapsePhylo2_7 %>% collapse(node = 824, 'max', fill = "#E69F00", alpha = .7)

# Nairobi ~
collapsePhylo2_9 <-
  collapsePhylo2_8 %>% collapse(node = 639, 'max', fill = "#E69F00", alpha = .7)

# Colombo ~
collapsePhylo2_10 <-
  collapsePhylo2_9 %>% collapse(node = 629, 'max', fill = "#F0E442", alpha = .7)

# Colombo ~
collapsePhylo2_10 <-
  collapsePhylo2_9 %>% collapse(node = 629, 'max', fill = "#F0E442", alpha = .7)

# Pigeon Island & Trincomalee ~
collapsePhylo2_11 <-
  collapsePhylo2_10 %>% collapse(node = 480, 'max', fill = "#44AA99", alpha = .7)


finalPhylo2 <- 
 collapsePhylo2_11 +
  
  geom_text2(aes(subset = (node == 526)), label = "Faroe Islands", fontface = "bold", family = "Helvetica", cex = 2.25, vjust = -1.6, hjust = -3.5, angle = 70, show.legend = FALSE) +
  geom_star(aes(subset = (node == 526)), starshape = 13, size = 1.25, fill = "#44AA99", alpha = .9, starstroke = .07, show.legend = FALSE) +
  
  geom_text2(aes(subset = (node == 588)), label = "Tel Aviv Colony", fontface = "bold", family = "Helvetica", cex = 2.25, vjust = 0, hjust = -2, angle = 95, show.legend = FALSE) +
  geom_star(aes(subset = (node == 588)), starshape = 13, size = 1.25, fill = "#56B4E9", alpha = .9, starstroke = .07, show.legend = FALSE) +
  
  geom_text2(aes(subset = (node == 639)), label = "Nairobi", fontface = "bold", family = "Helvetica", cex = 2.25, vjust = 0, hjust = -6.5, angle = 120, show.legend = FALSE) +
  geom_star(aes(subset = (node == 639)), starshape = 7, size = 1.25, fill = "#E69F00", alpha = .9, starstroke = .07, show.legend = FALSE) +
  
  geom_text2(aes(subset = (node == 629)), label = "Colombo", fontface = "bold", family = "Helvetica", cex = 2.25, vjust = -.85, hjust = -5, angle = 130, show.legend = FALSE) +
  geom_star(aes(subset = (node == 629)), starshape = 7, size = 1.25, fill = "#F0E442", alpha = .9, starstroke = .07, show.legend = FALSE) +
  
  geom_text2(aes(subset = (node == 900)), label = "Barcelona", fontface = "bold", family = "Helvetica", cex = 2.25, vjust = -3, hjust = 5.75, angle = -2.5, show.legend = FALSE) +
  geom_star(aes(subset = (node == 900)), starshape = 14, size = 1.25, fill = "#F0E442", starstroke = .07, show.legend = FALSE) +
  
  geom_text2(aes(subset = (node == 886)), label = "Salvador", fontface = "bold", family = "Helvetica", cex = 2.25, vjust = 15, hjust = 6, angle = 12, show.legend = FALSE) +
  geom_star(aes(subset = (node == 886)), starshape = 13, size = 1.25, fill = "#56B4E9", starstroke = .07, show.legend = FALSE) +
  
  geom_text2(aes(subset = (node == 867)), label = "Tatuí", fontface = "bold", family = "Helvetica", cex = 2.25, vjust = 0, hjust = 0, angle = 0, show.legend = FALSE) +
  geom_star(aes(subset = (node == 867)), starshape = 13, size = 1.25, fill = "#56B4E9", starstroke = .07, show.legend = FALSE) +
  
  geom_text2(aes(subset = (node == 851)), label = "Santiago", fontface = "bold", family = "Helvetica", cex = 2.25, vjust = 0, hjust = 0, angle = 0, show.legend = FALSE) +
  geom_star(aes(subset = (node == 851)), starshape = 13, size = 1.25, fill = "#56B4E9", starstroke = .07, show.legend = FALSE) +
  
  geom_text2(aes(subset = (node == 824)), label = "San Cristóbal de las Casas", fontface = "bold", family = "Helvetica", cex = 2.25, vjust = 0, hjust = 0, angle = 0, show.legend = FALSE) +
  geom_star(aes(subset = (node == 824)), starshape = 13, size = 1.25, fill = "#56B4E9", starstroke = .07, show.legend = FALSE) +
  
  geom_text2(aes(subset = (node == 480)), label = "Pigeon Island & Trincomalee", fontface = "bold", family = "Helvetica", cex = 2.25, vjust = -8, hjust = -1.75, angle = 0, show.legend = FALSE) +
  geom_star(aes(subset = (node == 480)), starshape = 29, size = 1.25, fill = "#44AA99", alpha = .9, starstroke = .07, show.legend = FALSE)

 
ggsave(finalPhylo2, file = "FPG--PhyloData_II_Collapsed.pdf", device = cairo_pdf, width = 12, height = 18, dpi = 600)


#
##
### The END ~~~~~

geom_text(aes(label = node), size = 1, hjust = -.3) +