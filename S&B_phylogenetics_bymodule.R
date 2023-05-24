##2021 phylogenetic metrics by module-------------------------------------------
library(ape)
library(geiger)
library(picante)


#import S&B18 tree and check data 
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/RMBL phylogeny/Smith&Brown18")
SBtree <- read.tree(file = "ALLMB.tre")

#community matrix with my data 
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1")
matrix2021.mod <- read.table("2021_community_matrix_modules.txt", sep = "\t", header = T, row.names = 1)



###2021 Faith's PD###########################
prune.sum.function <- function(x){
  tmp.tree <- treedata(SBtree, x[x>0])$phy
  sum(tmp.tree$edge.length)
}

PD21.mod <- apply(matrix2021.mod, MARGIN = 1, prune.sum.function)
print(PD21.mod)
PD21.mod

###standard effect sizes 2021################
###Standard effect sizes for 2021 using picante ###############################
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1")

pruned.tree2021 <- treedata(SBtree, unlist(matrix2021[4,matrix2021[4,]>0]), warnings = F)$phy
plot(pruned.tree2021)

#standard effect size using picante 
library(picante)
ses.pd(matrix2021.mod, pruned.tree2021, null.model = c("sample.pool"),
       runs = 999, iterations = 1000, include.root=TRUE)

###2022 Faith's PD#####################################
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1")
matrix2022.mod <- read.table("2022_community_matrix_modules.txt", sep = "\t", header = T, row.names = 1)

prune.sum.function <- function(x){
  tmp.tree <- treedata(SBtree, x[x>0])$phy
  sum(tmp.tree$edge.length)
}

PD22.mod <- apply(matrix2022.mod, MARGIN = 1, prune.sum.function)
print(PD22.mod)
PD22.mod

##MPD calculations--------------------------------------------------------------

