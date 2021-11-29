### The BEGINNING ~~~~~
##
# ~ Plots PG--Fst


# Cleans the environment ~ 
rm(list=ls())


# Sets working directory ~
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Loads required packages ~
pacman::p_load(pheatmap)


# Loads Fst table ~
FstTable <- read.table("PG--Fst.tsv", sep = "\t", header = FALSE, stringsAsFactors = FALSE)


# Adds column names ~
colnames(FstTable) <- c("Pop1", "Pop2", "LociNumber", "Unweighted", "Weighted")


# Removes certain populations ~
FstTable <- subset(FstTable, Pop1 != "SouthEast-B" & Pop2 != "SouthEast-B")


# Melts data ~
pops = union(FstTable$Pop1, FstTable$Pop2)
n = length(pops)


# Creates Fst-Sites matrix ~
FstSites <- matrix(0, nrow = n, ncol = n, dimnames = list(pops, pops))
for (i in 1:nrow(FstTable)) {
  FstSites[FstTable[i, "Pop1"], FstTable[i, "Pop2"]] = FstTable[i, "LociNumber"]
  FstSites[FstTable[i, "Pop2"], FstTable[i, "Pop1"]] = FstTable[i, "Weighted"]}


# Writes Fst-Sites matrix ~
write.table(FstSites, "PG--Fst-Sites.txt", sep = "\t", row.names = TRUE, col.names = TRUE)


# Creates Fst-Fst matrix ~
Fst <- matrix(0, nrow = n, ncol = n, dimnames = list(pops, pops))
for (i in 1:nrow(FstTable)) {
  Fst[FstTable[i, "Pop1"], FstTable[i, "Pop2"]] = FstTable[i, "Weighted"]
  Fst[FstTable[i, "Pop2"], FstTable[i, "Pop1"]] = FstTable[i, "Weighted"]}


# Writes Fst-Fst matrix ~
write.table(Fst, "PG--Fst.txt", sep = "\t", row.names = TRUE, col.names = TRUE)


# Reorganises the data ~
PopOrder <- c("TAN-F", "RAN-B", "FAR-F", "SJA-F", "SON-F", "TYB-F", "POL-BF", "ROS-B", "KET-B", "NAK-B", "KAR-B", "ISH-B")


# Reorders populations ~
Fst_Ordered <- reorder_mat(mat = Fst, order = PopOrder)


# Creates & Saves the heatmap ~
pheatmap(Fst_Ordered, cluster_rows = FALSE, cluster_cols = FALSE, border_color = "black", 
         treeheight_row = 60, treeheight_col = 60, filename = "PG--Fst.pdf") 


#
##
### The END ~~~~~