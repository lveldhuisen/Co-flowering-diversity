####phylogenetic analyses for Ch 1 

setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/RMBL phylogeny/Smith&Brown18")

library(ape)
install.packages("geiger")
library(geiger)

#import S&B18 tree and check data 
SBtree <- read.tree(file = "ALLMB.tre")
write.tree(SBtree)
is.rooted(SBtree)

##2021 community matrix for Faith's PD--------------------------------------------

setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1")

matrix2021 <- read.table("2021_community_matrix.txt", sep = "\t", header = T, row.names = 1)

#calculate pd of row 1 of matrix (PBM)

matrix2021[1, matrix2021[1,]>0]

######PBM 2021###################################################
richnessPBM21 = length(matrix2021[1, matrix2021[1,]>0])
#prune tree to just these species for PBM
pruned.treePBM21 <- treedata(SBtree, unlist(matrix2021[1,matrix2021[1,]>0]), warnings = F)$phy
class(pruned.treePBM21)
pruned.treePBM21
plot(pruned.treePBM21)

#Faith's index for PBM2021
sum(pruned.treePBM21$edge.length)

###Pfeiler 2021###############################
#prune tree
pruned.treePf21 <- treedata(SBtree, unlist(matrix2021[2,matrix2021[2,]>0]), warnings = F)$phy
class(pruned.treePf21)
pruned.treePf21
plot(pruned.treePf21)

#Faith's index for Pfeiler2021
sum(pruned.treePf21$edge.length)

###Road 2021##############################
#prune tree 
pruned.treeroad21 <- treedata(SBtree, unlist(matrix2021[3,matrix2021[3,]>0]), warnings = F)$phy
class(pruned.treeroad21)
pruned.treeroad21
plot(pruned.treeroad21)

#Faith's index for Road 2021
sum(pruned.treeroad21$edge.length)

###all sites in matrix with one code###############
prune.sum.function <- function(x){
  tmp.tree <- treedata(SBtree, x[x>0])$phy
  sum(tmp.tree$edge.length)
}

PD21 <- apply(matrix2021, MARGIN = 1, prune.sum.function)
print(PD21)

###standard effect sizes using picante ####################
pruned.tree2021 <- treedata(SBtree, unlist(matrix2021[4,matrix2021[4,]>0]), warnings = F)$phy
plot(pruned.tree2021)

library(picante)
ses.pd(matrix2021, pruned.tree2021, null.model = c("sample.pool"),
       runs = 5000, iterations = 5000, include.root=TRUE)


#2022 community data for Faith's PD--------------------------------------------
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1")

matrix2022 <- read.table("2022_community_matrix.txt", sep = "\t", header = T, row.names = 1)

###PBM 2022##########################################################
#prune tree to just these species for PBM
pruned.treePBM22 <- treedata(SBtree, unlist(matrix2022[1,matrix2022[1,]>0]), warnings = F)$phy
class(pruned.treePBM22)
pruned.treePBM22
plot(pruned.treePBM22)

#Faith's index for PBM2022
sum(pruned.treePBM22$edge.length)

###Pfeiler 2022##########################################################
#prune tree to just these species for Pfeiler
pruned.treePf22 <- treedata(SBtree, unlist(matrix2022[2,matrix2022[2,]>0]), warnings = F)$phy
class(pruned.treePf22)
pruned.treePf22
plot(pruned.treePf22)

#Faith's index for pfeiler 2022
sum(pruned.treePf22$edge.length)

###Road 2022##########################################################
#prune tree to just these species for road
pruned.treeroad22 <- treedata(SBtree, unlist(matrix2022[3,matrix2022[3,]>0]), warnings = F)$phy
class(pruned.treeroad22)
pruned.treeroad22
plot(pruned.treeroad22)

#Faith's index for pfeiler 2022
sum(pruned.treeroad22$edge.length)

###all sites in matrix with one code###############
prune.sum.function <- function(x){
  tmp.tree <- treedata(SBtree, x[x>0])$phy
  sum(tmp.tree$edge.length)
}

PD22 <- apply(matrix2022, MARGIN = 1, prune.sum.function)
print(PD22)

###Standard effect sizes for 2022 using picante ###############################
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1")

matrix2022 <- read.table("2022_community_matrix.txt", sep = "\t", header = T, row.names = 1)

pruned.tree2022 <- treedata(SBtree, unlist(matrix2022[4,matrix2022[4,]>0]), warnings = F)$phy
plot(pruned.tree2022)

#standard effect size using picante 
library(picante)

ses.pd(matrix2022, pruned.tree2022, null.model = c("sample.pool"),
       runs = 5000, iterations = 5000, include.root=TRUE)


##2021 data for MPD-------------------------------------------------------------
###Road 2021##############################################
plot.phylo(pruned.treeroad21)
dist.matR21 <- cophenetic(pruned.treeroad21)
mpd_r21<- mean(as.dist(dist.matR21))
###Pfeiler 2021##############################################
plot.phylo(pruned.treePf21)
dist.matPf21 <- cophenetic(pruned.treePf21)
mpd_Pf21<- mean(as.dist(dist.matPf21))
###PBM 2021##############################################
com.1 <- matrix2021[1, matrix2021[1,]>0]
names(com.1)
dist.mat.com.1<- dist.mat2021[names(com.1),names(com.1)]



plot.phylo(pruned.treePBM21)
dist.matPBM21 <- cophenetic(pruned.treePBM21)
mpd_PBM21<- mean(as.dist(dist.matPBM21))

###all sites together################################
#create distance matrix for all 2021 species
#need to figure out how to trim SB phylogeny but include all rows 
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1")

pruned.tree2021 <- treedata(SBtree, unlist(matrix2021[4,matrix2021[4,]>0]), warnings = F)$phy
plot(pruned.tree2021)
dist.mat2021 <- cophenetic(pruned.tree2021)
dist.mat2021

#function to do mpd for all sites 
new.mpd.function <- function(x){
  com.names<- names(x[x>0])
  mean(as.dist(dist.mat2021[com.names,com.names]))
}

mpd_2021 <- apply(matrix2021, MARGIN = 1, new.mpd.function)
print(mpd_2021)

###Standard effect size for 2021 MPD########################
ses.mpd(matrix2021, dist.mat2021, null.model = c("sample.pool"),
        abundance.weighted = FALSE, runs = 5000, iterations = 5000)

##2022 data for MPD-------------------------------------------------------------
###Road 2022##############################################
plot.phylo(pruned.treeroad22)
dist.matR22 <- cophenetic(pruned.treeroad22)
mpd_r22<- mean(as.dist(dist.matR22))
###Pfeiler 2022##############################################
plot.phylo(pruned.treePf22)
dist.matPf22 <- cophenetic(pruned.treePf22)
mpd_Pf22<- mean(as.dist(dist.matPf22))
###PBM 2022##############################################
plot.phylo(pruned.treePBM22)
dist.matPBM22 <- cophenetic(pruned.treePBM22)
mpd_PBM22<- mean(as.dist(dist.matPBM22))

##all sites together##########
new.mpd.function <- function(x){
  com.names<- names(x[x>0])
  mean(as.dist(dist.mat2022[com.names,com.names]))
}

mpd_2022 <- apply(matrix2022, MARGIN = 1, new.mpd.function)
print(mpd_2022)
##SES for mpd 2022
ses.mpd(matrix2022, dist.mat2022, null.model = c("sample.pool"),
        abundance.weighted = FALSE, runs = 5000, iterations = 5000)


##2021 MNTD---------------------------------------------------------------------
install.packages("picante")
library(picante)

dist.mat2021 <- cophenetic(pruned.tree2021)

##road 2021################
road21.sample <- matrix2021[3, matrix2021[3,]>0]

mntd.R21 <- mntd(matrix2021[3, matrix2021[3,]>0], cophenetic(pruned.tree2021), abundance.weighted = F)

###pfeiler 2021################
pfeiler21.sample <- matrix2021[2, matrix2021[2, ]>0]

mntd.pf21 <- mntd(pfeiler21.sample, cophenetic(pruned.tree2021), abundance.weighted = FALSE)

###PBM 2021##############
PBM21.sample <- matrix2021[1, matrix2021[1, ]>0]

mntd.PBM21 <- mntd(PBM21.sample, cophenetic(pruned.tree2021), abundance.weighted = FALSE)

#function for all sites##########
new.mntd.function <- function(x){
  com.names <- names(x[x>0])
  my.com.dist <- dist.mat2021[com.names,com.names]
  diag(my.com.dist) <- NA
  mean(apply(my.com.dist, MARGIN = 1, min), na.rm=TRUE)
}

mntd.2021 <- apply(matrix2021, MARGIN = 1, new.mntd.function)
mntd.2021


###SES MNTD 2021#########
ses.mntd(matrix2021, dist.mat2021, null.model = c("sample.pool"),
         abundance.weighted=FALSE, runs = 5000, iterations = 5000)

##2022 MNTD--------------------------------------------------------------------
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1")

matrix2022 <- read.table("2022_community_matrix.txt", sep = "\t", header = T, row.names = 1)

pruned.tree2022 <- treedata(SBtree, unlist(matrix2022[4,matrix2022[4,]>0]), warnings = F)$phy
plot(pruned.tree2022)


dist.mat2022 <- cophenetic(pruned.tree2022)

##road 2022################
road22.sample <- matrix2022[3, matrix2022[3,]>0]

mntd.R22 <- mntd(matrix2022[3, matrix2022[3,]>0], cophenetic(pruned.tree2022), abundance.weighted = F)

##pfeiler 2022################
pf22.sample <- matrix2022[2, matrix2022[2,]>0]

mntd.pf22 <- mntd(matrix2022[2, matrix2022[2,]>0], cophenetic(pruned.tree2022), abundance.weighted = F)

##PBM 2022################
PBM22.sample <- matrix2022[1, matrix2022[1,]>0]
mntd.PB8M22 <- mntd(matrix2022[1, matrix2022[1,]>0], cophenetic(pruned.tree2022), abundance.weighted = F)
#function all sites 2022
new.mntd.function <- function(x){
  com.names <- names(x[x>0])
  my.com.dist <- dist.mat2022[com.names,com.names]
  diag(my.com.dist) <- NA
  mean(apply(my.com.dist, MARGIN = 1, min), na.rm=TRUE)
}

mntd.2022 <- apply(matrix2022, MARGIN = 1, new.mntd.function)
mntd.2022

###SES MNTD 2022#####
ses.mntd(matrix2022, dist.mat2022, null.model = c("sample.pool"),
         abundance.weighted=FALSE, runs = 5000, iterations = 5000)


#make tables with species pairs and PD for each---------------------------------
library(metagMisc)
##2021############
###Road 2021######
class(dist.matR21)

dist.matR21[lower.tri(dist.matR21, diag = TRUE)] <- ""
Road21_PD <- as.data.frame(as.table(dist.matR21))

Road21_PD["Freq"][Road21_PD["Freq"] == ''] <- NA
na.omit(Road21_PD)
print(Road21_PD)

write.csv(Road21_PD, file="/Users/leahvedlhuisen/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona\ PhD/Research/Chapter 1/results_specieslevel/Road21_PD.csv")

###Pfeiler 2021#######

dist.matPf21[lower.tri(dist.matPf21, diag = TRUE)] <- ""
Pfeiler21_PD <- as.data.frame(as.table(dist.matPf21))

Pfeiler21_PD["Freq"][Pfeiler21_PD["Freq"] == ''] <- NA
na.omit(Pfeiler21_PD)
print(Pfeiler21_PD)

write.csv(Pfeiler21_PD, file="/Users/leahvedlhuisen/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona\ PhD/Research/Chapter 1/results_specieslevel/Pfeiler21_PD.csv")

###PBM 2021#######
dist.matPBM21[lower.tri(dist.matPBM21, diag = TRUE)] <- ""
PBM21_PD <- as.data.frame(as.table(dist.matPBM21))

PBM21_PD["Freq"][PBM21_PD["Freq"] == ''] <- NA
na.omit(PBM21_PD)
print(PBM21_PD)

write.csv(PBM21_PD, file="/Users/leahvedlhuisen/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona\ PhD/Research/Chapter 1/results_specieslevel/PBM21_PD.csv")

##2022########
###Road 2022#######
dist.matR22[lower.tri(dist.matR22, diag = TRUE)] <- ""
Road22_PD <- as.data.frame(as.table(dist.matR22))

Road22_PD["Freq"][Road22_PD["Freq"] == ''] <- NA
na.omit(Road22_PD)
print(Road22_PD)

write.csv(Road22_PD, file="/Users/leahvedlhuisen/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona\ PhD/Research/Chapter 1/results_specieslevel/Road22_PD.csv")

###Pfeiler 2022#########
dist.matPf22[lower.tri(dist.matPf22, diag = TRUE)] <- ""
Pfeiler22_PD <- as.data.frame(as.table(dist.matPf22))

Pfeiler22_PD["Freq"][Pfeiler22_PD["Freq"] == ''] <- NA
na.omit(Pfeiler22_PD)
print(Pfeiler22_PD)

write.csv(Pfeiler22_PD, file="/Users/leahvedlhuisen/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona\ PhD/Research/Chapter 1/results_specieslevel/Pfeiler22_PD.csv")

###PBM 2022########
dist.matPBM22[lower.tri(dist.matPBM22, diag = TRUE)] <- ""
PBM22_PD <- as.data.frame(as.table(dist.matPBM22))

PBM22_PD["Freq"][PBM22_PD["Freq"] == ''] <- NA
na.omit(PBM22_PD)
print(PBM22_PD)

write.csv(PBM22_PD, file="/Users/leahvedlhuisen/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona\ PhD/Research/Chapter 1/results_specieslevel/PBM22_PD.csv")

