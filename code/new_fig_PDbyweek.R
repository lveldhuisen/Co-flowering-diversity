#file includes analysis for PD, MPD and MNTD by week of the flowering season 
#and code for the new figure with these results 


library(ggplot2)
library(dplyr)
library(ggpubr)
library(viridis)
library(pals)
library(Polychrome)
library(picante)

#import Smith & Brown 2018 tree and check data 
SBtree <- read.tree(file = "ALLMB.tre")
write.tree(SBtree) #test if imported correctly, can take a long time though 
is.rooted(SBtree) #check tree, should say true 

