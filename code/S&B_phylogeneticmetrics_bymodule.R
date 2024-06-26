##Phylogenetic metrics by module (network analysis output), all necessary files are in the "files_phylogeneticmetrics_bymodule" folder
#this code corresponds to Figure 4 and the "Phylogenetic Dispersion" section of Results in the manuscript


install.packages("ape")
install.packages("geiger")
install.packages("picante")
library(ape)
library(geiger)
library(picante)
library(tidyverse)


#import Smith and Brown 2018 tree and check data 
#file available in "files_phylogeneticetrics_bymodule" folder
SBtree <- read.tree(file = "ALLMB.tre")

#community matrix with my data 
matrix2021.mod <- read.table("2021_community_matrix_modules.txt", sep = "\t", header = T, row.names = 1)

#Faith's PD-------------------------------------------------------------------
##2021#################
prune.sum.function <- function(x){
  tmp.tree <- treedata(SBtree, x[x>0])$phy
  sum(tmp.tree$edge.length)
} #function to calculate PD from phylogeny 

PD21.mod <- apply(matrix2021.mod, MARGIN = 1, prune.sum.function) #save PD values by module
print(PD21.mod) #view PD values 
PD21.mod

###SES###############################

pruned.tree2021 <- treedata(SBtree, unlist(matrix2021.mod[10,matrix2021.mod[10,]>0]), warnings = F)$phy
plot(pruned.tree2021) #prune tree

#standard effect size using picante 
pd_mod21 <- ses.pd(matrix2021.mod, pruned.tree2021, null.model = c("sample.pool"),
       runs = 5000, iterations = 5000, include.root=TRUE) #output shows PD and SES for all modules for 2021

#format data table
pd_mod21 = subset(pd_mod21, select = -c(ntaxa,pd.obs,pd.rand.mean,pd.rand.sd,pd.obs.rank,runs) ) #remove unnecessary columns

names(pd_mod21)[names(pd_mod21) == "pd.obs.z"] <- "SES"
names(pd_mod21)[names(pd_mod21) == "pd.obs.p"] <- "P_values" #rename columns to match other datasets 

pd_mod21<- pd_mod21[-c(10),]

pd_mod21$Type <- c("PD") #add column for metric type 
pd_mod21$Site <- c("Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Middle elevation (3165 m)", "Middle elevation (3165 m)","Middle elevation (3165 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)") #add column for site name 
pd_mod21$Year <- c("2021")
pd_mod21$Module <- c("Beginning","Middle","End","Beginning","Middle","End","Beginning","Middle","End")

##2022#####################################
matrix2022.mod <- read.table("2022_community_matrix_modules.txt", sep = "\t", header = T, row.names = 1)

pruned.tree2022 <- treedata(SBtree, unlist(matrix2022.mod[11,matrix2022.mod[11,]>0]), warnings = F)$phy #prune tree
plot(pruned.tree2022) #check tree 

prune.sum.function <- function(x){
  tmp.tree <- treedata(pruned.tree2022, x[x>0])$phy
  sum(tmp.tree$edge.length)
} #function to calculate PD from phylogeny

PD22.mod <- apply(matrix2022.mod, MARGIN = 1, prune.sum.function) #save values
print(PD22.mod) #view values 

###SES 2022##########

pd_mod22<- ses.pd(matrix2022.mod, pruned.tree2022, null.model = c("sample.pool"),
       runs = 5000, iterations = 5000, include.root=TRUE) #output shows PD and SES for all modules for 2022

#format data table
pd_mod22 = subset(pd_mod22, select = -c(ntaxa,pd.obs,pd.rand.mean,pd.rand.sd,pd.obs.rank,runs) ) #remove unnecessary columns
names(pd_mod22)[names(pd_mod22) == "pd.obs.z"] <- "SES"
names(pd_mod22)[names(pd_mod22) == "pd.obs.p"] <- "P_values" #rename columns to match other datasets 

pd_mod22<- pd_mod22[-c(11),]

pd_mod22$Type <- c("PD") #add column for metric type 
pd_mod22$Site <- c("Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Middle elevation (3165 m)", "Middle elevation (3165 m)","Middle elevation (3165 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)") #add column for site name 
pd_mod22$Year <- c("2022")
pd_mod22$Module <- c("Beginning","Middle","Middle2","End","Beginning","Middle","End","Beginning","Middle","End")

#MPD--------------------------------------------------------------
##2021#########

pruned.tree2021 <- treedata(SBtree, unlist(matrix2021[4,matrix2021[4,]>0]), warnings = F)$phy
plot(pruned.tree2021)
dist.mat2021 <- cophenetic(pruned.tree2021)
dist.mat2021
#function for all modules at once 
new.mpd.function <- function(x){
  com.names<- names(x[x>0])
  mean(as.dist(dist.mat2021[com.names,com.names]))
}

mpd_2021 <- apply(matrix2021.mod, MARGIN = 1, new.mpd.function) #save MPD values for all modules
print(mpd_2021) #view MPD values

###SES MPD 2021#########
mpd_mod21<-ses.mpd(matrix2021.mod, dist.mat2021, null.model = c("sample.pool"),
                   abundance.weighted = FALSE, runs = 5000, iterations = 5000) #output shows MPD values and SES for all modules in all 2021 sites 

#format data table
mpd_mod21 = subset(mpd_mod21, select = -c(ntaxa,mpd.obs,mpd.rand.mean,mpd.rand.sd,mpd.obs.rank,runs) ) #remove unnecessary columns

names(mpd_mod21)[names(mpd_mod21) == "mpd.obs.z"] <- "SES"
names(mpd_mod21)[names(mpd_mod21) == "mpd.obs.p"] <- "P_values" #rename columns to match other datasets 

mpd_mod21<- mpd_mod21[-c(10),]

mpd_mod21$Type <- c("MPD") #add column for metric type 
mpd_mod21$Site <- c("Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Middle elevation (3165 m)", "Middle elevation (3165 m)","Middle elevation (3165 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)") #add column for site name 
mpd_mod21$Year <- c("2021")
mpd_mod21$Module <- c("Beginning","Middle","End","Beginning","Middle","End","Beginning","Middle","End")


##2022#########

dist.mat2022 <- cophenetic(pruned.tree2022) #make 2022 distane matrix from pruned phylogeny 
dist.mat2022 #check matrix 
#function for all modules at once
new.mpd.function <- function(x){
  com.names<- names(x[x>0])
  mean(as.dist(dist.mat2022[com.names,com.names]))
}

mpd_2022 <- apply(matrix2022.mod, MARGIN = 1, new.mpd.function) #save all MPD values
print(mpd_2022) #view MPD values

###SES MPD 2022######
mpd_mod22<-ses.mpd(matrix2022.mod, dist.mat2022, null.model = c("sample.pool"),
        abundance.weighted = FALSE, runs = 5000, iterations = 5000) #output shows MPD and SES for all modules in all sites for 2022

#format data table
mpd_mod22 = subset(mpd_mod22, select = -c(ntaxa,mpd.obs,mpd.rand.mean,mpd.rand.sd,mpd.obs.rank,runs) ) #remove unnecessary columns

names(mpd_mod22)[names(mpd_mod22) == "mpd.obs.z"] <- "SES"
names(mpd_mod22)[names(mpd_mod22) == "mpd.obs.p"] <- "P_values" #rename columns to match other datasets 

mpd_mod22<- mpd_mod22[-c(11),]

mpd_mod22$Type <- c("MPD") #add column for metric type 
mpd_mod22$Site <- c("Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Middle elevation (3165 m)", "Middle elevation (3165 m)","Middle elevation (3165 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)") #add column for site name 
mpd_mod22$Year <- c("2022")
mpd_mod22$Module <- c("Beginning","Middle","Middle2","End","Beginning","Middle","End","Beginning","Middle","End")

#MNTD------------------------------------------------------------------------
##2021###############
new.mntd.function <- function(x){
  com.names <- names(x[x>0])
  my.com.dist <- dist.mat2021[com.names,com.names]
  diag(my.com.dist) <- NA
  mean(apply(my.com.dist, MARGIN = 1, min), na.rm=TRUE)
}

mntd.2021.mod <- apply(matrix2021.mod, MARGIN = 1, new.mntd.function) #save MNTD values for all modules in all sites for 2021
mntd.2021.mod #view values 

###SES 2021 MNTD###########
mntd_mod21 <- ses.mntd(matrix2021.mod, dist.mat2021, null.model = c("sample.pool"),
         abundance.weighted=FALSE, runs = 5000, iterations = 5000) #shows MNTD values and SES for all modules in all sites for 2021

#format data table
mntd_mod21 = subset(mntd_mod21, select = -c(ntaxa,mntd.obs,mntd.rand.mean,mntd.rand.sd,mntd.obs.rank,runs) ) #remove unnecessary columns

names(mntd_mod21)[names(mntd_mod21) == "mntd.obs.z"] <- "SES"
names(mntd_mod21)[names(mntd_mod21) == "mntd.obs.p"] <- "P_values" #rename columns to match other datasets 

mntd_mod21<- mntd_mod21[-c(10),]

mntd_mod21$Type <- c("MNTD") #add column for metric type 
mntd_mod21$Site <- c("Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Middle elevation (3165 m)", "Middle elevation (3165 m)","Middle elevation (3165 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)") #add column for site name 
mntd_mod21$Year <- c("2021")
mntd_mod21$Module <- c("Beginning","Middle","End","Beginning","Middle","End","Beginning","Middle","End")

##2022#############
new.mntd.function <- function(x){
  com.names <- names(x[x>0])
  my.com.dist <- dist.mat2022[com.names,com.names]
  diag(my.com.dist) <- NA
  mean(apply(my.com.dist, MARGIN = 1, min), na.rm=TRUE)
}

mntd.2022.mod <- apply(matrix2022.mod, MARGIN = 1, new.mntd.function) #save MNTD values for all modules in all sites for 2022
mntd.2022.mod #view values 

###SES 2022 MNTD############
mntd_mod22 <- ses.mntd(matrix2022.mod, dist.mat2022, null.model = c("sample.pool"),
         abundance.weighted=FALSE, runs = 5000, iterations = 5000) #shows MNTD values and SES for all modules in all sites for 2022

#format data table
mntd_mod22 = subset(mntd_mod22, select = -c(ntaxa,mntd.obs,mntd.rand.mean,mntd.rand.sd,mntd.obs.rank,runs) ) #remove unnecessary columns

names(mntd_mod22)[names(mntd_mod22) == "mntd.obs.z"] <- "SES"
names(mntd_mod22)[names(mntd_mod22) == "mntd.obs.p"] <- "P_values" #rename columns to match other datasets 

mntd_mod22<- mntd_mod22[-c(11),]

mntd_mod22$Type <- c("MNTD") #add column for metric type 
mntd_mod22$Site <- c("Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Middle elevation (3165 m)", "Middle elevation (3165 m)","Middle elevation (3165 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)") #add column for site name 
mntd_mod22$Year <- c("2022")
mntd_mod22$Module <- c("Beginning","Middle","Middle2","End","Beginning","Middle","End","Beginning","Middle","End")

#combine into one big dataset-------------
all_mod2021 <- rbind(pd_mod21,mpd_mod21,mntd_mod21)
all_mod2022 <- rbind(pd_mod22, mpd_mod22,mntd_mod22)

all_mod <- rbind(all_mod2021,all_mod2022)

