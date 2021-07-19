### The BEGINNING ~~~~~
##
# ~ Plots PG--MDS | First written by Filipe G. VIEIRA with later modifications by George PACHECO.


# Cleans the environment ~ 
rm(list=ls())


# Sets working directory ~
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Loads required packages ~
pacman::p_load(optparse, tidyverse, plyr, RColorBrewer, extrafont, ggforce, ggstar)


# Imports extra fonts ~
loadfonts(device = "win", quiet = TRUE)


# Loads list of options ~
option_list <- list(make_option(c('-i','--in_file'), action = 'store', type = 'character', default = "stdin", help = 'Input file'),
                    make_option(c('-a','--annot'), action = 'store', type = 'character', default = NULL, help = 'File with indiv annotations'),
                    make_option(c('--id_column'), action = 'store', type = 'numeric', default = 1, help = 'Column to use as ID'),
                    make_option(c('-L','--in_maj_labels'), action = 'store', type = 'character', default = NULL, help = 'Column from annotation file to use as MAJOR label'),
                    make_option(c('-l','--in_min_labels'), action = 'store', type = 'character', default = NULL, help = 'Column from annotation file to use as MINOR label'),
                    make_option(c('--no_header'), action = 'store_true', type = 'logical', default = FALSE, help = 'Input file has no header'),
                    make_option(c('--var_excl'), action = 'store', type = 'character', default = NULL, help = 'Variables to exclude from analysis'))

  
# Defines parameters ~
opt <- parse_args(OptionParser(option_list = option_list))
opt$in_file = "PG--GoodSamples_NoOutgroup_SNPs.mds"
opt$annot = "PG--GoodSamples_NoOutgroup_SNPs.annot"
opt$id_column = 1
opt$in_maj_labels = "Population"


# Reads data ~
data <- read.table(opt$in_file, row.names = 1, sep = "\t", header = !opt$no_header, stringsAsFactors = FALSE, check.names = FALSE)
n <- ncol(data)


# Reads annotation file ~
if(!is.null(opt$annot)){
annot <- read.table(opt$annot, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
colnames(annot) <- c("Sample_ID", "Population")
data <- merge(data, annot, by.x = 0, by.y = opt$id_column)


# Gets rownames back into place ~
rownames(data) <- data[,1]; data <- data[,-1]
data[colnames(annot)[opt$id_column]] <- rownames(data)}


# Excludes variables ~
if( !is.null(opt$var_excl)){
opt$var_excl <- unlist(strsplit(opt$var_excl, ","))
data <- data[!(rownames(data) %in% opt$var_excl),]}


# Gets Major labels mean location ~
colors <- NULL
if(!is.null(opt$in_maj_labels)){


# Merge Major labels ~
in_maj_labels <- unlist(strsplit(opt$in_maj_labels, ",", fixed = TRUE))
tmp_data <- data[,in_maj_labels[1]]
data[in_maj_labels[1]] <- NULL
if(length(in_maj_labels) > 1){
   for (cnt in 2:length(in_maj_labels)){
    tmp_data <- paste(tmp_data, data[,in_maj_labels[cnt]], sep="/")
    data[in_maj_labels[cnt]] <- NULL}
    opt$in_maj_labels <- "MERGE"}


# Make sure Major label column is after data ~
data <- data.frame(data, tmp_data)
colnames(data)[ncol(data)] <- opt$in_maj_labels


# Converts to factor, in case there is a Major label with just numbers~
data[,opt$in_maj_labels] <- factor(data[,opt$in_maj_labels])


# If label was in input file, decreases number of data columns ~
if(is.null(opt$annot) || !opt$in_maj_labels %in% colnames(annot))
n = n - 1


# Gets mean value for Major label ~
data_mean <- ddply(data, opt$in_maj_labels, function(x){colMeans(x[, 1:n], na.rm = TRUE)})
colors <- as.character(opt$in_maj_labels)}


# Expands the data by adding BioStatus ~
data$BioStatus <- ifelse(data$Population %in% c("Torshavn","Ejde","Sumba","LjosAir","Kunoy","Nolsoy", "Crete", "Sardinia","Vernelle","WadiHidan","PigeonIsland","Trincomalee"), "Remote_Localities_Within_Natural_Range",
                  ifelse(data$Population %in% c("Guimaraes","Lisbon","Barcelona","Berlin","Cambridge",
                                               "Colombo","Copenhagen","London","Prague","Jihlava","Abadeh",
                                               "Isfahan","Lahijan","Nowshahr","Tehran","TelAviv"), "Urban_Localities_Within_Natural_Range",
                  ifelse(data$Population %in% c("SaltLakeCity","Denver", "FeralVA", "FeralUT", "TlaxcalaDeXicohtencatl",
                                               "MexicoCity","Monterrey","SanCristobalDeLasCasas","Santiago",
                                               "Salvador","Tatui","Johannesburg","Nairobi","Perth"), "Localities_Outside_Natural_Range",
                  ifelse(data$Population %in% c("TelAvivColony","Wattala", "Wellawatte"), "Captives", NA))))


# Expands the data by adding Groups ~
data$Groups <- # Remote Localities Within Natural Range
                 ifelse(data$Population %in% c("PigeonIsland", "Trincomalee"), "Group A",
                 ifelse(data$Population %in% c("Abadeh", "Tehran", "Crete", "Sardinia", "Vernelle", "Torshavn", "Ejde", "Sumba", "LjosAir", "Kunoy", "Nolsoy"), "Group B",
                 ifelse(data$Population %in% c("TelAviv", "TelAvivColony", "WadiHidan"), "Group C",
                 ifelse(data$Population %in% c("Nairobi", "Colombo", "Lahijan", "Nowshahr", "Wellawatte", "Isfahan"), "Group D",
                 ifelse(data$Population %in% c("Guimaraes", "Barcelona", "Lisbon", "Salvador", "Tatui","Denver" , "Santiago", "TlaxcalaDeXicohtencatl", "MexicoCity", "Monterrey", "SanCristobalDeLasCasas"), "Group E",
                 ifelse(data$Population %in% c("Jihlava", "Prague", "Berlin", "SaltLakeCity", "Johannesburg", "London", "Cambridge", "Perth", "Copenhagen"), "Group F", "Not Grouped"))))))
                                                                          

# Creates alternative Population column ~
data$Population_cbind <- data$Population


# Combines all populations from the Faroe Islands ~
levels(data$Population_cbind <- sub("Torshavn", "FaroeIslands", data$Population_cbind))
levels(data$Population_cbind <- sub("Ejde", "FaroeIslands", data$Population_cbind))
levels(data$Population_cbind <- sub("Sumba", "FaroeIslands", data$Population_cbind))
levels(data$Population_cbind <- sub("LjosAir", "FaroeIslands", data$Population_cbind))
levels(data$Population_cbind <- sub("Kunoy", "FaroeIslands", data$Population_cbind))
levels(data$Population_cbind <- sub("Nolsoy", "FaroeIslands", data$Population_cbind))
                          
                          
## Reorders BioStatus ~
data$BioStatus <- factor(data$BioStatus, ordered = T,
                           levels = c("Remote_Localities_Within_Natural_Range",
                                      "Urban_Localities_Within_Natural_Range",
                                      "Localities_Outside_Natural_Range",
                                      "Captives"))


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


# Dimensions ~
# 1: D1_3.18814763506453
# 2: D2_1.93889781570542
# 3: D3_1.47369056639638


# Creates MDS plots ~
MDS_12 <-
ggplot(data, aes_string(x = "D1_3.18814763506453", y = "D2_1.93889781570542")) +
  geom_star(aes(starshape = Groups, fill = BioStatus), size = 2.8, alpha = .9, starstroke = .15) +
  scale_fill_manual(values = c("#44AA99", "#F0E442", "#E69F00", "#56B4E9"), labels = gsub("_", " ", levels(data$BioStatus))) +
  scale_starshape_manual(values = Shapes) +
  geom_mark_ellipse(aes(color = BioStatus, group = Population_cbind, filter = Population_cbind == "FaroeIslands", label = "Faroe Islands"),
                    label.buffer = unit(8, 'mm'), con.colour = "black", con.type = "elbow", label.fill = NA, show.legend = FALSE) +
  geom_mark_ellipse(aes(color = BioStatus, group = Population, filter = Population == "PigeonIsland", label = "Pigeon Island"),
                    label.buffer = unit(40, 'mm'), con.colour = "black", con.type = "elbow", label.fill = NA, show.legend = FALSE) +
  geom_mark_ellipse(aes(color = BioStatus, group = Population, filter = Population == "Trincomalee", label = "Trincomalee"),
                    label.buffer = unit(16, 'mm'), con.colour = "black", con.type = "elbow", label.fill = NA, show.legend = FALSE) +
  scale_x_continuous("Dimension 1 (3.19%)",
                     breaks = c(-0.075, -0.05, -0.025, 0, 0.025),
                     labels = c("-0.075", "-0.05", "-0.025", "0", "0.025"),
                     expand = c(0,0),
                     limits = c(-0.073, 0.03)) +
  scale_y_continuous("Dimension 2 (1.94%)",
                     breaks = c(-0.05, -0.025, 0, 0.025, 0.05), 
                     expand = c(0,0),
                     labels = c("-0.05", "-0.025", "0", "0.025", "0.05"), 
                     limits = c(-0.0525, 0.0525)) +
  theme(panel.background = element_rect(fill = "#ffffff"),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "right",
        legend.title = element_text(color = "#000000", size = 13),
        legend.text = element_text(size = 11),
        axis.title.x = element_text(size = 18, face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 18, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.text = element_text(color = "#000000", size = 13),
        axis.ticks = element_line(color = "#000000", size = 0.3),
        axis.line = element_line(colour = "#000000", size = 0.3)) +
  guides(fill = guide_legend(title = "Biological Status", title.theme = element_text(size = 16, face = "bold", family = "Helvetica"),
                             label.theme = element_text(size = 14, family = "Helvetica"),
                             override.aes = list(starshape = 21, size = 5, alpha = .9, starstroke = .15), order = 1),
         starshape = guide_legend(title = "Groups", title.theme = element_text(size = 16, face = "bold", family = "Helvetica"),
                              label.theme = element_text(size = 14, family = "Helvetica"),
                              override.aes = list(starshape = Shapes, size = 5, alpha = .9, starstroke = .15), order = 2),
         colour = "none")


# Creates & Saves the final MDS Panel ~
ggsave(MDS_12, file = "PG--MDS_12.pdf", device = cairo_pdf, scale = 1.5, width = 12, height = 8, dpi = 600)


MDS_13 <-
  ggplot(data, aes_string(x = "D1_3.18814763506453", y = "D3_1.47369056639638")) +
  geom_star(aes(starshape = Groups, fill = BioStatus), size = 2.8, alpha = .9, starstroke = .15) +
  scale_fill_manual(values = c("#44AA99", "#F0E442", "#E69F00", "#56B4E9"), labels = gsub("_", " ", levels(data$BioStatus))) +
  scale_starshape_manual(values = Shapes) +
  geom_mark_ellipse(aes(color = BioStatus, group = Population_cbind, filter = Population_cbind == "FaroeIslands", label = "Faroe Islands"),
                    label.buffer = unit(8, 'mm'), con.colour = "black", con.type = "elbow", label.fill = NA, show.legend = FALSE) +
  geom_mark_ellipse(aes(color = BioStatus, group = Population, filter = Population == "PigeonIsland", label = "Pigeon Island"),
                    label.buffer = unit(22, 'mm'), con.cap = unit(5, "mm"), con.colour = "black", con.type = "elbow", label.fill = NA, show.legend = FALSE) +
  geom_mark_ellipse(aes(color = BioStatus, group = Population, filter = Population == "Trincomalee", label = "Trincomalee"),
                    label.buffer = unit(14, 'mm'), con.cap = unit(8.5, "mm"), con.colour = "black", con.type = "elbow", label.fill = NA, show.legend = FALSE) +
  scale_x_continuous("Dimension 1 (3.19%)",
                     breaks = c(-0.05, -0.025, 0, 0.025),
                     labels = c("-0.05", "-0.025", "0", "0.025"),
                     expand = c(0,0),
                     limits = c(-0.073, 0.03)) +
  scale_y_continuous("Dimension 3 (1.47%)",
                     breaks = c(-0.05, -0.025, 0, 0.025, 0.05), 
                     expand = c(0,0),
                     labels = c("-0.05", "-0.025", "0", "0.025", "0.05"), 
                     limits = c(-0.0525, 0.0525)) +
  theme(panel.background = element_rect(fill = "#ffffff"),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "right",
        legend.title = element_text(color = "#000000", size = 13),
        legend.text = element_text(size = 11),
        axis.title.x = element_text(size = 18, face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 18, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.text = element_text(color = "#000000", size = 13),
        axis.ticks = element_line(color = "#000000", size = 0.3),
        axis.line = element_line(colour = "#000000", size = 0.3)) +
  guides(fill = guide_legend(title = "Biological Status", title.theme = element_text(size = 16, face = "bold", family = "Helvetica"),
                             label.theme = element_text(size = 14, family = "Helvetica"),
                             override.aes = list(starshape = 21, size = 5, alpha = .9, starstroke = .15), order = 1),
         starshape = guide_legend(title = "Groups", title.theme = element_text(size = 16, face = "bold", family = "Helvetica"),
                                  label.theme = element_text(size = 14, family = "Helvetica"),
                                  override.aes = list(starshape = Shapes, size = 5, alpha = .9, starstroke = .15), order = 2),
         colour = "none")


# Creates & Saves the final MDS Panel ~
ggsave(MDS_13, file = "PG--MDS_13.pdf", device = cairo_pdf, scale = 1.5, width = 12, height = 8, dpi = 600)


MDS_23 <-
  ggplot(data, aes_string(x = "D2_1.93889781570542", y = "D3_1.47369056639638")) +
  geom_star(aes(starshape = Groups, fill = BioStatus), size = 2.8, alpha = .9, starstroke = .15) +
  scale_fill_manual(values = c("#44AA99", "#F0E442", "#E69F00", "#56B4E9"), labels = gsub("_", " ", levels(data$BioStatus))) +
  scale_starshape_manual(values = Shapes) +
  geom_mark_ellipse(aes(color = BioStatus, group = Population_cbind, filter = Population_cbind == "FaroeIslands", label = "Faroe Islands"),
                    label.buffer = unit(8, 'mm'), con.colour = "black", con.type = "elbow", label.fill = NA, show.legend = FALSE) +
  geom_mark_ellipse(aes(color = BioStatus, group = Population, filter = Population == "PigeonIsland", label = "Pigeon Island"),
                    label.buffer = unit(20, 'mm'), con.cap = unit(8.5, "mm"), con.colour = "black", con.type = "elbow", label.fill = NA, show.legend = FALSE) +
  geom_mark_ellipse(aes(color = BioStatus, group = Population, filter = Population == "Trincomalee", label = "Trincomalee"),
                    label.buffer = unit(16, 'mm'), con.cap = unit(11.5, "mm"), con.colour = "black", con.type = "elbow", label.fill = NA, show.legend = FALSE) +
  scale_x_continuous("Dimension 2 (1.94%)",
                     breaks = c(-0.05, -0.025, 0, 0.025, 0.05),
                     labels = c("-0.05", "-0.025", "0", "0.025", "0.05"),
                     expand = c(0,0),
                     limits = c(-0.045, 0.045)) +
  scale_y_continuous("Dimension 3 (1.47%)",
                     breaks = c(-0.050, -0.025, 0, 0.025, 0.050), 
                     expand = c(0,0),
                     labels = c("-0.05", "-0.025", "0", "0.025", "0.05"), 
                     limits = c(-0.0525, 0.0525)) +
  theme(panel.background = element_rect(fill = "#ffffff"),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "right",
        legend.title = element_text(color = "#000000", size = 13),
        legend.text = element_text(size = 11),
        axis.title.x = element_text(size = 18, face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 18, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.text = element_text(color = "#000000", size = 13),
        axis.ticks = element_line(color = "#000000", size = 0.3),
        axis.line = element_line(colour = "#000000", size = 0.3)) +
  guides(fill = guide_legend(title = "Biological Status", title.theme = element_text(size = 16, face = "bold", family = "Helvetica"),
                             label.theme = element_text(size = 14, family = "Helvetica"),
                             override.aes = list(starshape = 21, size = 5, alpha = .9, starstroke = .15), order = 1),
         starshape = guide_legend(title = "Groups", title.theme = element_text(size = 16, face = "bold", family = "Helvetica"),
                                  label.theme = element_text(size = 14, family = "Helvetica"),
                                  override.aes = list(starshape = Shapes, size = 5, alpha = .9, starstroke = .15), order = 2),
         colour = "none")


# Creates & Saves the final MDS Panel ~
ggsave(MDS_23, file = "PG--MDS_23.pdf", device = cairo_pdf, width = 12, height = 8, scale = 1.5, dpi = 600)


#
##
### The END ~~~~~
