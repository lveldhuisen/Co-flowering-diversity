#file includes analysis for PD, MPD and MNTD by week of the flowering season 
#and code for the new figure with these results 


library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(viridis)
library(pals)
library(Polychrome)
library(picante)
library(ape)
library(geiger)

#import Smith & Brown 2018 tree and check data 
SBtree <- read.tree(file = "ALLMB.tre")
write.tree(SBtree) #test if imported correctly, can take a long time though 
is.rooted(SBtree) #check tree, should say true 

#2021 data--------------
#make community matrix 
matrix_weeks_2021 <- read.table("files_phylogeneticmetrics_byweek/comm_matrix_weeks2021.txt", sep = "\t", header = T, row.names = 1)

#prune tree to just these species for PBM
pruned.tree21 <- treedata(SBtree, unlist(matrix_weeks_2021[31,matrix_weeks_2021[31,]>0]), warnings = F)$phy
class(pruned.tree21)
pruned.tree21
plot(pruned.tree21)#check tree 

##PD####
pd_weeks <- ses.pd(matrix_weeks_2021, pruned.tree21, null.model = c("sample.pool"),
       runs = 5000, iterations = 5000, include.root=TRUE) #output shows standard effect size and absolute value of PD

###edit table to combine with other metrics####
pd_weeks = subset(pd_weeks, select = -c(ntaxa,pd.obs,pd.rand.mean,pd.rand.sd,pd.obs.rank,runs) ) #remove unnecessary columns

pd_weeks <- pd_weeks %>% 
  rename(SES = pd.obs.z,
         P_value = pd.obs.p) #rename columns to match other datasets 

pd_weeks$Type <- c("PD") #add column for metric type 
pd_weeks$Site <- c("High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)", "Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)", "Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","all") #add column for site name 
pd_weeks$Week <- c("1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10","all")


##MPD#####
mpd_weeks <- ses.mpd(matrix_weeks_2021, cophenetic(pruned.tree21), null.model = c("sample.pool"),
        abundance.weighted = FALSE, runs = 5000, iterations = 5000) #output shows MPD and SES for all 2021 weeks

##MNTD####
mntd_weeks <- ses.mntd(matrix_weeks_2021, cophenetic(pruned.tree21), null.model = c("sample.pool"),
                     abundance.weighted = FALSE, runs = 5000, iterations = 5000) #output shows MPD and SES for all 2021 weeks
