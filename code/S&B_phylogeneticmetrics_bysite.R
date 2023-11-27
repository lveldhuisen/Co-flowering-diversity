#contains: community phylogenetic metrics group by elevational site. all necessary files are in the "files_phylogeneticmetrics_bysite" folder. 

#packages
install.packages("ape")
install.packages("geiger")
install.packages("picante")
library(ape)
library(geiger)
library(picante)

#import Smith & Brown 2018 tree and check data 
SBtree <- read.tree(file = "ALLMB.tre")
write.tree(SBtree) #test if imported correctly, can take a long time though 
is.rooted(SBtree) #check tree, should say true 

#Faith's Phylogenetic diversity----------------------------------
##2021 --------------------------------------------


#make community matrix 
matrix2021 <- read.table("2021_community_matrix.txt", sep = "\t", header = T, row.names = 1)

###PBM 2021###################################################
#prune tree to just these species for PBM
pruned.treePBM21 <- treedata(SBtree, unlist(matrix2021[1,matrix2021[1,]>0]), warnings = F)$phy
class(pruned.treePBM21)
pruned.treePBM21
plot(pruned.treePBM21)#check tree 

#Faith's PD for PBM2021
sum(pruned.treePBM21$edge.length)

###Pfeiler 2021###############################
#prune tree
pruned.treePf21 <- treedata(SBtree, unlist(matrix2021[2,matrix2021[2,]>0]), warnings = F)$phy
class(pruned.treePf21)
pruned.treePf21
plot(pruned.treePf21) #check tree

###Road 2021##############################
#prune tree 
pruned.treeroad21 <- treedata(SBtree, unlist(matrix2021[3,matrix2021[3,]>0]), warnings = F)$phy
class(pruned.treeroad21)
pruned.treeroad21
plot(pruned.treeroad21)#check tree 

###all 2021 together###############
prune.sum.function <- function(x){
  tmp.tree <- treedata(SBtree, x[x>0])$phy
  sum(tmp.tree$edge.length)
}

PD21 <- apply(matrix2021, MARGIN = 1, prune.sum.function)
print(PD21) #view absolute PD values for all 2021 sites 

###standard effect sizes using picante ####################
pruned.tree2021 <- treedata(SBtree, unlist(matrix2021[4,matrix2021[4,]>0]), warnings = F)$phy
plot(pruned.tree2021)#check tree 

ses.pd(matrix2021, pruned.tree2021, null.model = c("sample.pool"),
       runs = 5000, iterations = 5000, include.root=TRUE) #output shows standard effect size and absolute value of PD
#these values are in Table 1 in the manuscript and the "Phylogenetic Dispersion" section of Results in the manuscript


##2022 --------------------------------------------

#make community matrix 
matrix2022 <- read.table("2022_community_matrix.txt", sep = "\t", header = T, row.names = 1)

###PBM 2022##########################################################
#prune tree to just these species for PBM
pruned.treePBM22 <- treedata(SBtree, unlist(matrix2022[1,matrix2022[1,]>0]), warnings = F)$phy
class(pruned.treePBM22)
pruned.treePBM22
plot(pruned.treePBM22) #check tree

#Faith's PD for PBM2022
sum(pruned.treePBM22$edge.length)

###Pfeiler 2022##########################################################
#prune tree to just these species for Pfeiler
pruned.treePf22 <- treedata(SBtree, unlist(matrix2022[2,matrix2022[2,]>0]), warnings = F)$phy
class(pruned.treePf22)
pruned.treePf22
plot(pruned.treePf22) #check tree

#Faith's index for pfeiler 2022
sum(pruned.treePf22$edge.length)

###Road 2022##########################################################
#prune tree to just these species for road
pruned.treeroad22 <- treedata(SBtree, unlist(matrix2022[3,matrix2022[3,]>0]), warnings = F)$phy
class(pruned.treeroad22)
pruned.treeroad22
plot(pruned.treeroad22)#check tree 

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

pruned.tree2022 <- treedata(SBtree, unlist(matrix2022[4,matrix2022[4,]>0]), warnings = F)$phy
plot(pruned.tree2022) #check tree 

#standard effect size using picante 

ses.pd(matrix2022, pruned.tree2022, null.model = c("sample.pool"),
       runs = 5000, iterations = 5000, include.root=TRUE) #output includes absolute values and SES for all sites in 2022 
#these values are in Table 1 in the manuscript and the "Phylogenetic Dispersion" section of Results in the manuscript

#MPD-------------------------------------------------------------
##2021################
###Road 2021##############################################
plot.phylo(pruned.treeroad21)
dist.matR21 <- cophenetic(pruned.treeroad21) #make distance matrix from phylogeny 
mpd_r21<- mean(as.dist(dist.matR21))# save MPD value
###Pfeiler 2021##############################################
plot.phylo(pruned.treePf21)
dist.matPf21 <- cophenetic(pruned.treePf21)#make distance matrix from phylogeny
mpd_Pf21<- mean(as.dist(dist.matPf21)) # save MPD value
###PBM 2021##############################################
com.1 <- matrix2021[1, matrix2021[1,]>0]
names(com.1)
dist.mat.com.1<- dist.mat2021[names(com.1),names(com.1)]



plot.phylo(pruned.treePBM21)
dist.matPBM21 <- cophenetic(pruned.treePBM21) #make distance matrix from phylogeny
mpd_PBM21<- mean(as.dist(dist.matPBM21)) # save MPD value

###all sites together################################
#create distance matrix for all 2021 species

pruned.tree2021 <- treedata(SBtree, unlist(matrix2021[4,matrix2021[4,]>0]), warnings = F)$phy
plot(pruned.tree2021)
dist.mat2021 <- cophenetic(pruned.tree2021) #make distance matrix from phylogeny
dist.mat2021 #check distance matrix 

#function to do mpd for all sites 
new.mpd.function <- function(x){
  com.names<- names(x[x>0])
  mean(as.dist(dist.mat2021[com.names,com.names]))
}

mpd_2021 <- apply(matrix2021, MARGIN = 1, new.mpd.function) #calculate MPD values for all sites 
print(mpd_2021) #view MPD values for all sites 

###Standard effect size for 2021 MPD########################
ses.mpd(matrix2021, dist.mat2021, null.model = c("sample.pool"),
        abundance.weighted = FALSE, runs = 5000, iterations = 5000) #output shows MPD and SES for all 2021 sites 
#these values are in Table 1 in the manuscript and the "Phylogenetic Dispersion" section of Results in the manuscript

##2022-------------------------------------------------------------
###Road 2022##############################################
plot.phylo(pruned.treeroad22)
dist.matR22 <- cophenetic(pruned.treeroad22) #make distance matrix from phylogeny
mpd_r22<- mean(as.dist(dist.matR22)) #save MPD values 
###Pfeiler 2022##############################################
plot.phylo(pruned.treePf22)
dist.matPf22 <- cophenetic(pruned.treePf22) #make distance matrix from phylogeny
mpd_Pf22<- mean(as.dist(dist.matPf22)) #save MPD values
###PBM 2022##############################################
plot.phylo(pruned.treePBM22)
dist.matPBM22 <- cophenetic(pruned.treePBM22) #make distance matrix from phylogeny
mpd_PBM22<- mean(as.dist(dist.matPBM22)) #save MPD values

###all sites together##########
new.mpd.function <- function(x){
  com.names<- names(x[x>0])
  mean(as.dist(dist.mat2022[com.names,com.names]))
}

mpd_2022 <- apply(matrix2022, MARGIN = 1, new.mpd.function) #save MPD values
print(mpd_2022) #view MPD values

###SES for mpd 2022
ses.mpd(matrix2022, dist.mat2022, null.model = c("sample.pool"),
        abundance.weighted = FALSE, runs = 5000, iterations = 5000) #output shows MPD values and SES for all 2022 sites 
#these values are in Table 1 in the manuscript and the "Phylogenetic Dispersion" section of Results in the manuscript

#MNTD-----------------------------------------
##2021---------------------------------------------------------------------

dist.mat2021 <- cophenetic(pruned.tree2021) #make distance matrix 

###Road 2021################
road21.sample <- matrix2021[3, matrix2021[3,]>0] #pull data for just one site 

#save MNTD values
mntd.R21 <- mntd(matrix2021[3, matrix2021[3,]>0], cophenetic(pruned.tree2021), abundance.weighted = F)  

###Pfeiler 2021################
pfeiler21.sample <- matrix2021[2, matrix2021[2, ]>0] #pull data for just one site

#save MNTD values
mntd.pf21 <- mntd(pfeiler21.sample, cophenetic(pruned.tree2021), abundance.weighted = FALSE)

###PBM 2021##############
PBM21.sample <- matrix2021[1, matrix2021[1, ]>0] #pull data for just one site

#save MNTD values
mntd.PBM21 <- mntd(PBM21.sample, cophenetic(pruned.tree2021), abundance.weighted = FALSE)

###function for all sites##########
new.mntd.function <- function(x){
  com.names <- names(x[x>0])
  my.com.dist <- dist.mat2021[com.names,com.names]
  diag(my.com.dist) <- NA
  mean(apply(my.com.dist, MARGIN = 1, min), na.rm=TRUE)
}

mntd.2021 <- apply(matrix2021, MARGIN = 1, new.mntd.function) #MNTD for all sites 
mntd.2021 #view MNTD values 


###SES MNTD 2021#########
ses.mntd(matrix2021, dist.mat2021, null.model = c("sample.pool"),
         abundance.weighted=FALSE, runs = 5000, iterations = 5000) #output shows MNTD and SES for all 2021 sites 
#these values are in Table 1 in the manuscript and the "Phylogenetic Dispersion" section of Results in the manuscript

##2022--------------------------------------------------------------------

matrix2022 <- read.table("2022_community_matrix.txt", sep = "\t", header = T, row.names = 1) #make matrix for 2022 community data 

pruned.tree2022 <- treedata(SBtree, unlist(matrix2022[4,matrix2022[4,]>0]), warnings = F)$phy
plot(pruned.tree2022) #prune tree for 2022 

dist.mat2022 <- cophenetic(pruned.tree2022) #make phylogeny into distance matrix 

###Road 2022################
road22.sample <- matrix2022[3, matrix2022[3,]>0] #pull data for just one site

#save MNTD values 
mntd.R22 <- mntd(matrix2022[3, matrix2022[3,]>0], cophenetic(pruned.tree2022), abundance.weighted = F)

###Pfeiler 2022################
pf22.sample <- matrix2022[2, matrix2022[2,]>0] #pull data for just one site

#save MNTD values
mntd.pf22 <- mntd(matrix2022[2, matrix2022[2,]>0], cophenetic(pruned.tree2022), abundance.weighted = F)

###PBM 2022################
PBM22.sample <- matrix2022[1, matrix2022[1,]>0] #pull data for just one site
#save MNTD values
mntd.PB8M22 <- mntd(matrix2022[1, matrix2022[1,]>0], cophenetic(pruned.tree2022), abundance.weighted = F)

###function all sites 2022#######
new.mntd.function <- function(x){
  com.names <- names(x[x>0])
  my.com.dist <- dist.mat2022[com.names,com.names]
  diag(my.com.dist) <- NA
  mean(apply(my.com.dist, MARGIN = 1, min), na.rm=TRUE)
}

mntd.2022 <- apply(matrix2022, MARGIN = 1, new.mntd.function) #save MNTD values for all sites 
mntd.2022 #view MNTD values 

###SES MNTD 2022#####
ses.mntd(matrix2022, dist.mat2022, null.model = c("sample.pool"),
         abundance.weighted=FALSE, runs = 5000, iterations = 5000) #output shows MNTD and SES for all 2022 sites 
#these values are in Table 1 in the manuscript and the "Phylogenetic Dispersion" section of Results in the manuscript

#make tables with species pairs and phylogenetic distance between each---------------------------------
install.packages("metagMisc")
library(metagMisc)

##2021############
###Road 2021######
class(dist.matR21)

dist.matR21[lower.tri(dist.matR21, diag = TRUE)] <- "" #get rid of half of the matrix
Road21_PD <- as.data.frame(as.table(dist.matR21)) #turn back into dataframe

Road21_PD["Freq"][Road21_PD["Freq"] == ''] <- NA
na.omit(Road21_PD)
print(Road21_PD)

###Pfeiler 2021#######

dist.matPf21[lower.tri(dist.matPf21, diag = TRUE)] <- "" #get rid of half of the matrix
Pfeiler21_PD <- as.data.frame(as.table(dist.matPf21)) #turn back into dataframe

Pfeiler21_PD["Freq"][Pfeiler21_PD["Freq"] == ''] <- NA
na.omit(Pfeiler21_PD)
print(Pfeiler21_PD)

###PBM 2021#######
dist.matPBM21[lower.tri(dist.matPBM21, diag = TRUE)] <- "" #get rid half of matrix
PBM21_PD <- as.data.frame(as.table(dist.matPBM21)) #turn back into dataframe

PBM21_PD["Freq"][PBM21_PD["Freq"] == ''] <- NA
na.omit(PBM21_PD)
print(PBM21_PD)


###all 2021 PD together#########
dist.mat2021[lower.tri(dist.mat2021, diag = TRUE)] <- "" #get rid of half of matrix 
PD_2021 <- as.data.frame(as.table(dist.mat2021)) #turn back into dataframe 

PD_2021["Freq"][PD_2021["Freq"] == ''] <- NA
na.omit(PD_2021)
print(PD_2021)


##2022########
###Road 2022#######
dist.matR22[lower.tri(dist.matR22, diag = TRUE)] <- "" #get rid of half of matrix 
Road22_PD <- as.data.frame(as.table(dist.matR22)) #turn back into dataframe

Road22_PD["Freq"][Road22_PD["Freq"] == ''] <- NA
na.omit(Road22_PD)
print(Road22_PD)


###Pfeiler 2022#########
dist.matPf22[lower.tri(dist.matPf22, diag = TRUE)] <- "" #get rid of half of matrix 
Pfeiler22_PD <- as.data.frame(as.table(dist.matPf22)) #turn back into dataframe

Pfeiler22_PD["Freq"][Pfeiler22_PD["Freq"] == ''] <- NA
na.omit(Pfeiler22_PD)
print(Pfeiler22_PD)


###PBM 2022########
dist.matPBM22[lower.tri(dist.matPBM22, diag = TRUE)] <- "" #get rid of half of matrix 
PBM22_PD <- as.data.frame(as.table(dist.matPBM22)) #turn back into dataframe

PBM22_PD["Freq"][PBM22_PD["Freq"] == ''] <- NA
na.omit(PBM22_PD)
print(PBM22_PD)


###all 2022 PD together###########
dist.mat2022[lower.tri(dist.mat2022, diag = TRUE)] <- "" #get rid of half of matrix 
PD_2022 <- as.data.frame(as.table(dist.mat2022)) #turn back into dataframe 

PD_2022["Freq"][PD_2022["Freq"] == ''] <- NA
na.omit(PD_2022)
print(PD_2022)


# make PD dataset into files for 2021 and 2022--------------------------------------------
##2021######
distmat21all <- cophenetic(pruned.tree2021) #turn phylogeny into matrix 

PD_2021_all <- as.data.frame(as.table(distmat21all)) #turn back into dataframe 


##2022#######
distmat22all <- cophenetic(pruned.tree2022) #turn phylogeny into matrix

PD_2022_all <- as.data.frame(as.table(distmat22all))#turn back into dataframe
