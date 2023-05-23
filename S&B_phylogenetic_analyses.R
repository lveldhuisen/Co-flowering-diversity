####phylogenetic analyses for Ch 1 

setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/RMBL phylogeny/Smith&Brown18")

library(ape)
install.packages("geiger")
library(geiger)

#import S&B18 tree and check data 
SBtree <- read.tree(file = "ALLMB.tre")
write.tree(SBtree)
is.rooted(SBtree)

##2021 community matrix
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1")

matrix2021 <- read.table("2021_community_matrix.txt", sep = "\t", header = T, row.names = 1)
