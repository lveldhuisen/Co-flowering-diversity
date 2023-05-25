##Phylogenetic metrics by module
library(ape)
library(geiger)
library(picante)


#import S&B18 tree and check data 
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/RMBL phylogeny/Smith&Brown18")
SBtree <- read.tree(file = "ALLMB.tre")

#community matrix with my data 
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1")
matrix2021.mod <- read.table("2021_community_matrix_modules.txt", sep = "\t", header = T, row.names = 1)


#Faith's PD-------------------------------------------------------------------
###2021#################
prune.sum.function <- function(x){
  tmp.tree <- treedata(SBtree, x[x>0])$phy
  sum(tmp.tree$edge.length)
}

PD21.mod <- apply(matrix2021.mod, MARGIN = 1, prune.sum.function)
print(PD21.mod)
PD21.mod

###SES 2021 using picante ###############################
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1")

pruned.tree2021 <- treedata(SBtree, unlist(matrix2021.mod[10,matrix2021.mod[10,]>0]), warnings = F)$phy
plot(pruned.tree2021)

#standard effect size using picante 
library(picante)
ses.pd(matrix2021.mod, pruned.tree2021, null.model = c("sample.pool"),
       runs = 5000, iterations = 5000, include.root=TRUE)

###2022#####################################
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1")
matrix2022.mod <- read.table("2022_community_matrix_modules.txt", sep = "\t", header = T, row.names = 1)

pruned.tree2022 <- treedata(SBtree, unlist(matrix2022.mod[11,matrix2022.mod[11,]>0]), warnings = F)$phy
plot(pruned.tree2022)

prune.sum.function <- function(x){
  tmp.tree <- treedata(pruned.tree2022, x[x>0])$phy
  sum(tmp.tree$edge.length)
}

PD22.mod <- apply(matrix2022.mod, MARGIN = 1, prune.sum.function)
print(PD22.mod)
PD22.mod

###SES 2022##########
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1")

ses.pd(matrix2022.mod, pruned.tree2022, null.model = c("sample.pool"),
       runs = 5000, iterations = 5000, include.root=TRUE)


#MPD--------------------------------------------------------------
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/RMBL phylogeny/Smith&Brown18")
SBtree <- read.tree(file = "ALLMB.tre")

###2021 MPD#########

pruned.tree2021 <- treedata(SBtree, unlist(matrix2021[4,matrix2021[4,]>0]), warnings = F)$phy
plot(pruned.tree2021)
dist.mat2021 <- cophenetic(pruned.tree2021)
dist.mat2021
#function for all modules at once 
new.mpd.function <- function(x){
  com.names<- names(x[x>0])
  mean(as.dist(dist.mat2021[com.names,com.names]))
}

mpd_2021 <- apply(matrix2021.mod, MARGIN = 1, new.mpd.function)
print(mpd_2021)

###SES MPD 2021#########
ses.mpd(matrix2021.mod, dist.mat2021, null.model = c("sample.pool"),
        abundance.weighted = FALSE, runs = 5000, iterations = 5000)

###2022 MPD#########

setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1")
matrix2022.mod <- read.table("2022_community_matrix_modules.txt", sep = "\t", header = T, row.names = 1)

dist.mat2022 <- cophenetic(pruned.tree2022)
dist.mat2022
#function for all modules at once
new.mpd.function <- function(x){
  com.names<- names(x[x>0])
  mean(as.dist(dist.mat2022[com.names,com.names]))
}

mpd_2022 <- apply(matrix2022.mod, MARGIN = 1, new.mpd.function)
print(mpd_2022)

###SES MPD 2022######
ses.mpd(matrix2022.mod, dist.mat2022, null.model = c("sample.pool"),
        abundance.weighted = FALSE, runs = 5000, iterations = 5000)

#MNTD------------------------------------------------------------------------
###2021###############
new.mntd.function <- function(x){
  com.names <- names(x[x>0])
  my.com.dist <- dist.mat2021[com.names,com.names]
  diag(my.com.dist) <- NA
  mean(apply(my.com.dist, MARGIN = 1, min), na.rm=TRUE)
}

mntd.2021.mod <- apply(matrix2021.mod, MARGIN = 1, new.mntd.function)
mntd.2021.mod
###SES 2021 MNTD###########
ses.mntd(matrix2021.mod, dist.mat2021, null.model = c("sample.pool"),
         abundance.weighted=FALSE, runs = 5000, iterations = 5000)

###2022#############
new.mntd.function <- function(x){
  com.names <- names(x[x>0])
  my.com.dist <- dist.mat2022[com.names,com.names]
  diag(my.com.dist) <- NA
  mean(apply(my.com.dist, MARGIN = 1, min), na.rm=TRUE)
}

mntd.2022.mod <- apply(matrix2022.mod, MARGIN = 1, new.mntd.function)
mntd.2022.mod
###SES 2022 MNTD############
ses.mntd(matrix2022.mod, dist.mat2022, null.model = c("sample.pool"),
         abundance.weighted=FALSE, runs = 5000, iterations = 5000)

