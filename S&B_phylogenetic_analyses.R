####phylogenetic analyses for Ch 1 

setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/RMBL phylogeny/Smith&Brown18")

library(ape)
install.packages("geiger")
library(geiger)

#import S&B18 tree and check data 
SBtree <- read.tree(file = "ALLMB.tre")
write.tree(SBtree)
is.rooted(SBtree)
plot(SBtree)

##2021 community matrix

setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1")

matrix2021 <- read.table("2021_community_matrix.txt", sep = "\t", header = T, row.names = 1)

#calculate pd of row 1 of matrix (PBM)

matrix2021[1, matrix2021[1,]>0]

#richness of PBM 2021
richnessPBM21 = length(matrix2021[1, matrix2021[1,]>0])
#prune tree to just these species
pruned.tree <- treedata(SBtree, unlist(matrix2021[1,matrix2021[1,]>0]), warnings = F)$phy
class(pruned.tree)
pruned.tree
plot(pruned.tree)

#Faith's index for PBM2021
sum(pruned.tree$edge.length)
