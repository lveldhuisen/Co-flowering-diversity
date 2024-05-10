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
pd_weeks21 <- ses.pd(matrix_weeks_2021, pruned.tree21, null.model = c("sample.pool"),
       runs = 5000, iterations = 5000, include.root=TRUE) #output shows standard effect size and absolute value of PD

###edit table to combine with other metrics####
pd_weeks21 = subset(pd_weeks21, select = -c(ntaxa,pd.obs,pd.rand.mean,pd.rand.sd,pd.obs.rank,runs) ) #remove unnecessary columns

pd_weeks21 <- pd_weeks21 %>% 
  rename(SES = pd.obs.z,
         P_value = pd.obs.p) #rename columns to match other datasets 

pd_weeks21$Type <- c("PD") #add column for metric type 
pd_weeks21$Site <- c("High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)", "Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)", "Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","all") #add column for site name 
pd_weeks21$Week <- c("1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10","all")

##MPD#####
mpd_weeks21 <- ses.mpd(matrix_weeks_2021, cophenetic(pruned.tree21), null.model = c("sample.pool"),
        abundance.weighted = FALSE, runs = 5000, iterations = 5000) #output shows MPD and SES for all 2021 weeks
###edit table to combine with other metrics####
mpd_weeks21 = subset(mpd_weeks21, select = -c(ntaxa,mpd.obs,mpd.rand.mean,mpd.rand.sd,mpd.obs.rank,runs) ) #remove unnecessary columns
mpd_weeks21 <- mpd_weeks21 %>% 
  rename(SES = mpd.obs.z,
         P_value = mpd.obs.p) #rename columns to match other datasets 

mpd_weeks21$Type <- c("MPD") #add column for metric type 
mpd_weeks21$Site <- c("High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)", "Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)", "Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","all") #add column for site name 
mpd_weeks21$Week <- c("1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10","all")

##MNTD####
mntd_weeks21 <- ses.mntd(matrix_weeks_2021, cophenetic(pruned.tree21), null.model = c("sample.pool"),
                     abundance.weighted = FALSE, runs = 5000, iterations = 5000) #output shows MPD and SES for all 2021 weeks

###edit table to combine with other metrics####

mntd_weeks21 = subset(mntd_weeks21, select = -c(ntaxa,mntd.obs,mntd.rand.mean,mntd.rand.sd,mntd.obs.rank,runs) ) #remove unnecessary columns
mntd_weeks21 <- mntd_weeks21 %>% 
  rename(SES = mntd.obs.z,
         P_value = mntd.obs.p) #rename columns to match other datasets 

mntd_weeks21$Type <- c("MNTD") #add column for metric type 
mntd_weeks21$Site <- c("High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)", "Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)", "Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","all") #add column for site name 
mntd_weeks21$Week <- c("1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10","all")

##combine all metrics into one dataset####
all2021 <- rbind(pd_weeks21, mpd_weeks21, mntd_weeks21)
all2021 <- all2021[-c(91),]
all2021$Year <- c("2021")

#make 2021 figure---------------------------------
fig_pd_weeks21 <- ggplot(all2021, aes(fill = Type, y=SES, x=fct_relevel(Week, c("1","2","3","4","5","6","7","8","9","10")))) + 
  geom_bar(position = "dodge",stat = "identity") +
  xlab("Week") + 
  ylab("Standard effect size")+
  theme_light() + 
  guides(fill=guide_legend(title="Phylogenetic metric"))+
  scale_fill_manual(values=c("#c385b3",
                             "#cdd870",
                             "#4ea6c4"))+
  ylim(-5.9,2) +
  facet_grid(~factor(Site, levels = c("Low elevation (2815 m)","Middle elevation (3165 m)","High elevation (3380 m)")))
plot(fig_pd_weeks21)

#2022 data---------------------
#make community matrix 
matrix_weeks_2022 <- read.table("files_phylogeneticmetrics_byweek/comm_matrix_weeks2022.txt", sep = "\t", header = T, row.names = 1)

#prune tree to just these species for PBM
pruned.tree22 <- treedata(SBtree, unlist(matrix_weeks_2022[31,matrix_weeks_2022[31,]>0]), warnings = F)$phy
class(pruned.tree22)
pruned.tree22
plot(pruned.tree22)#check tree 

##PD####
pd_weeks22 <- ses.pd(matrix_weeks_2022, pruned.tree22, null.model = c("sample.pool"),
                     runs = 5000, iterations = 5000, include.root=TRUE) #output shows standard effect size and absolute value of PD

###edit table to combine with other metrics####
pd_weeks22 = subset(pd_weeks22, select = -c(ntaxa,pd.obs,pd.rand.mean,pd.rand.sd,pd.obs.rank,runs) ) #remove unnecessary columns

pd_weeks22 <- pd_weeks22 %>% 
  rename(SES = pd.obs.z,
         P_value = pd.obs.p) #rename columns to match other datasets 
pd_weeks22 <- pd_weeks22[-c(31),]

pd_weeks22$Type <- c("PD") #add column for metric type 
pd_weeks22$Site <- c("High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)", "Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)", "Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)") #add column for site name 
pd_weeks22$Week <- c("1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10")

##MPD#####
mpd_weeks22 <- ses.mpd(matrix_weeks_2022, cophenetic(pruned.tree22), null.model = c("sample.pool"),
                       abundance.weighted = FALSE, runs = 5000, iterations = 5000) #output shows MPD and SES for all 2022 weeks
###edit table to combine with other metrics####
mpd_weeks22 = subset(mpd_weeks22, select = -c(ntaxa,mpd.obs,mpd.rand.mean,mpd.rand.sd,mpd.obs.rank,runs) ) #remove unnecessary columns
mpd_weeks22 <- mpd_weeks22 %>% 
  rename(SES = mpd.obs.z,
         P_value = mpd.obs.p) #rename columns to match other datasets 
mpd_weeks22 <- mpd_weeks22[-c(31),]

mpd_weeks22$Type <- c("MPD") #add column for metric type 
mpd_weeks22$Site <- c("High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)", "Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)", "Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)") #add column for site name 
mpd_weeks22$Week <- c("1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10")

##MNTD####
mntd_weeks22 <- ses.mntd(matrix_weeks_2022, cophenetic(pruned.tree22), null.model = c("sample.pool"),
                         abundance.weighted = FALSE, runs = 5000, iterations = 5000) #output shows MPD and SES for all 2022 weeks

###edit table to combine with other metrics####

mntd_weeks22 = subset(mntd_weeks22, select = -c(ntaxa,mntd.obs,mntd.rand.mean,mntd.rand.sd,mntd.obs.rank,runs) ) #remove unnecessary columns
mntd_weeks22 <- mntd_weeks22 %>% 
  rename(SES = mntd.obs.z,
         P_value = mntd.obs.p) #rename columns to match other datasets 
mntd_weeks22 <- mntd_weeks22[-c(31),]

mntd_weeks22$Type <- c("MNTD") #add column for metric type 
mntd_weeks22$Site <- c("High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)","High elevation (3380 m)", "Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)","Middle elevation (3165 m)", "Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)","Low elevation (2815 m)") #add column for site name 
mntd_weeks22$Week <- c("1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10")

##combine all metrics into one dataset####
all2022 <- rbind(pd_weeks22, mpd_weeks22, mntd_weeks22)
all2022$Year <- c("2022")

all_weeks_pd <- rbind(all2021,all2022)

#make 2022 figure---------------------------------
fig_pd_weeks22 <- ggplot(all2022, aes(fill = Type, y=SES, x=fct_relevel(Week, c("1","2","3","4","5","6","7","8","9","10")))) + 
  geom_bar(position = "dodge",stat = "identity") +
  xlab("Week") + 
  ylab("Standard effect size")+
  theme_light() + 
  guides(fill=guide_legend(title="Phylogenetic metric"))+
  scale_fill_manual(values=c("#c385b3",
                             "#cdd870",
                             "#4ea6c4"))+
  ylim(-5.9,2) +
  facet_grid(~factor(Site, levels = c("Low elevation (2815 m)","Middle elevation (3165 m)","High elevation (3380 m)")))
plot(fig_pd_weeks22)

#make figure with both years combined-------------
fig_pd_weeks <- ggplot(all_weeks_pd, aes(fill = Type, y=SES, x=fct_relevel(Week, c("1","2","3","4","5","6","7","8","9","10")))) + 
  geom_bar(position = "dodge",stat = "identity") +
  xlab("Week") + 
  ylab("Standard effect size")+
  theme_light(base_size = 20) + 
  guides(fill=guide_legend(title="Phylogenetic metric"))+
  scale_fill_manual(values=c("#c385b3",
                             "#cdd870",
                             "#4ea6c4"))+
  ylim(-5.9,2) +
  facet_grid(Year ~factor(Site, levels = c("Low elevation (2815 m)","Middle elevation (3165 m)","High elevation (3380 m)")))

plot(fig_pd_weeks)
