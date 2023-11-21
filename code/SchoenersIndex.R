#this file contains calculations for Schoener's Index for phenology and fitness, 
#and for supplemental figure S1.
#all files necessary for this script are store in GitHub 


install.packages("tidyverse")
install.packages("spaa")
install.packages("reshape")
install.packages("reshape2")
install.packages("readr")
library(tidyverse)
library(spaa)
library(reshape)
library(reshape2)
library(readr)

#2021 data-------------------------

#Schoener's Index for phenology----------------
##PBM (high site) ########################

mat_PBM_2021 <- read_csv("files for code/2021/PBM_phenology_matrix_2021.csv") 
PBM_SI_2021 <- niche.overlap(mat_PBM_2021, method = "schoener") #calculate Schoener's Index for all pairs
hist(PBM_SI_2021)

#create new table with renamed columns 
PBM_21_melt <- melt(as.matrix(PBM_SI_2021), varnames = c("species1")) 
colnames(PBM_21_melt) <- c("species1", "species2", "SI")

#add column for site name
PBM_21_melt$site <- c("PBM")
print(PBM_21_melt)

##Pfeiler (middle site)##################
mat_Pfeiler_2021 <- read_csv("files for code/2021/Pfeiler_phenology_matrix_2021.csv") 
Pfeiler_SI_2021 <- niche.overlap(mat_Pfeiler_2021, method = "schoener") #calculate Schoener's Index for all pairs
hist(Pfeiler_SI_2021)

#create new table with renamed columns 
Pfeiler_21_melt <- melt(as.matrix(Pfeiler_SI_2021), varnames = c("species1"))
colnames(Pfeiler_21_melt) <- c("species1", "species2", "SI")

#add column for site name
Pfeiler_21_melt$site <- c("Pfeiler")
print(Pfeiler_21_melt)

##Road (low site) #################
mat_Road_2021 <- read_csv("files for code/2021/Road_phenology_matrix_2021.csv")  
Road_SI_2021<-niche.overlap(mat_Road_2021, method = "schoener") #calculate Schoener's Index for all pairs
hist(Road_SI_2021)

#create new table with renamed columns  
Road_21_melt <- melt(as.matrix(Road_SI_2021), varnames = c("species1"))
colnames(Road_21_melt) <- c("species1", "species2", "SI")

#add column for site name 
Road_21_melt$site <- c("Road")
print(Road_21_melt)

#merge all sites into one dataframe 
Merged2021 <- do.call("rbind", list(Road_21_melt, Pfeiler_21_melt, PBM_21_melt))
print(Merged2021)

write.csv(as.matrix(Merged2021), file="files for code/2021/SI_pheno_allsites_2021.csv") #save as csv
 
##compare distributions for SI between sites 2021##########################
install.packages("ggpubr")
library(ggpubr)

#test for supplementary materials 
kruskal.test(SI ~ site, data = Merged2021)

##supplementary material Fig S1 #####
ggboxplot(Merged2021, x = "site", y = "SI", 
           color = "site", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
           order = c("Road", "Pfeiler", "PBM"),
           ylab = "Schoener's Index - phenology", xlab = "Site", title = "2021") + theme(legend.position = "none") + scale_x_discrete(labels=c('Low', 'Middle', 'High'))
 
 
#Schoener's Index for fitness--------------------------------------------------
 
##PBM (high site) ################
mat_PBM_fitness_2021 <- read_csv("files for code/2021/PBM_fitness_matrix_2021.csv")
PBM_SI_fitness_2021 <- niche.overlap(mat_PBM_fitness_2021, method = "schoener") #calculate Schoener's Index for all pairs
hist(PBM_SI_fitness_2021)

#create data table with renamed columns 
PBM_21_fitness_melt <- melt(as.matrix(PBM_SI_fitness_2021), varnames = c("species1"))
colnames(PBM_21_fitness_melt) <- c("species1", "species2", "SI_f")

#add column for site name 
PBM_21_fitness_melt$site <- c("PBM")
print(PBM_21_fitness_melt)
 
##Pfeiler (middle site) #################
mat_Pfeiler_fitness_2021 <- read_csv("files for code/2021/Pfeiler_fitness_matrix_2021.csv")
Pfeiler_SI_fitness_2021 <- niche.overlap(mat_Pfeiler_fitness_2021, method = "schoener") #calculate Schoener's Index for all pairs
hist(Pfeiler_SI_fitness_2021)

#create data table with renamed columns  
Pfeiler_21_fitness_melt <- melt(as.matrix(Pfeiler_SI_fitness_2021), varnames = c("species1"))
colnames(Pfeiler_21_fitness_melt) <- c("species1", "species2", "SI_f")

#add column for site name 
Pfeiler_21_fitness_melt$site <- c("Pfeiler")
print(Pfeiler_21_fitness_melt)
 
 
##Road (low site) ###################### 
mat_Road_fitness_2021 <-read_csv("files for code/2021/Road_fitness_matrix_2021.csv")  
Road_SI_fitness_2021<-niche.overlap(mat_Road_fitness_2021, method = "schoener") #calculate Schoener's Index for all pairs
hist(Road_SI_fitness_2021)

#create data table with renamed columns
Road_21_fitness_melt <- melt(as.matrix(Road_SI_fitness_2021), varnames = c("species1"))
head(Road_21_fitness_melt)
colnames(Road_21_fitness_melt) <- c("species1", "species2", "SI_f")

#add column for site name  
Road_21_fitness_melt$site <- c("Road")
print(Road_21_fitness_melt)
 
 
##create merged dataframe for all sites fitness data 2021---------------------
Merged2021_fitness <- do.call("rbind", list(Road_21_fitness_melt, Pfeiler_21_fitness_melt, 
                                             PBM_21_fitness_melt))
print(Merged2021_fitness)
 

#2022 data---------------

##Schoener's Index phenology------------------
###PBM #####
mat_PBM_2022 <- read.csv("files for code/2022/PBM_phenology_matrix_2022.csv")
PBM_SI_22 <- niche.overlap(mat_PBM_2022, method = "schoener") #calculate Schoener's index for all pairs
hist(PBM_SI_22)

#create new data table with column names 
PBM_22_melt <- melt(as.matrix(PBM_SI_22), varnames = c("species1"))
colnames(PBM_22_melt) <- c("species1", "species2", "SI")
print(PBM_22_melt)

#add column for site name 
PBM_22_melt$site <- NA
PBM_22_melt$site <- c("PBM")
keeps <- c("species1","species2","SI","site")
PBM_22_melt = PBM_22_melt[keeps]
print(PBM_22_melt)
 
###Pfeiler#### 
mat_Pfeiler_2022 <- read.csv("files for code/2022/Pfeiler_phenology_matrix_2022.csv", header = TRUE)
Pfeiler_SI_22 <- niche.overlap(mat_Pfeiler_2022, method = "schoener") #calculate Schoener's index for all pairs
hist(Pfeiler_SI_22)

#create new data table with column names 
Pfeiler_22_melt <- melt(as.matrix(Pfeiler_SI_22), varnames = c("species1"))
colnames(Pfeiler_22_melt) <- c("species1", "species2", "SI")
print(Pfeiler_22_melt)

#add column for site name 
Pfeiler_22_melt$site <- c("Pfeiler")
head(Pfeiler_22_melt)
keeps <- c("species1","species2","SI","site")
Pfeiler_22_melt = Pfeiler_22_melt[keeps]
print(Pfeiler_22_melt)

###Road####
mat_Road_2022 <- read.csv("files for code/2022/Road_phenology_matrix_2022.csv", header = TRUE)
Road_SI_22 <- niche.overlap(mat_Road_2022, method = "schoener") #calculate Schoener's index for all pairs

#create new data table with column names 
Road_22_melt <- melt(as.matrix(Road_SI_22), varnames = c("species1"))
colnames(Road_22_melt) <- c("species1", "species2", "SI")

#add column for site name 
Road_22_melt$site <- c("Road")
print(Road_22_melt)
keeps <- c("species1","species2","SI","site")
Road_22_melt = Road_22_melt[keeps]
print(Road_22_melt)


###combine all three sites into 1 dataframe-------------------------------------

Merged2022 <- do.call("rbind", list(Road_22_melt, Pfeiler_22_melt, PBM_22_melt))
print(Merged2022)

write.csv(as.matrix(Merged2022), file="files for code/2022/SI_pheno_allsites_2022.csv") #save as csv

###compare distributions for 2022 phenology##################
unique(Merged2022$site)

###figure and tests for Supplemental Info fig S1 2022------------
kruskal.test(SI ~ site, data = Merged2022)
ggboxplot(Merged2022, x = "site", y = "SI", 
          color = "site", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Road", "Pfeiler", "PBM"),
          ylab = "Schoener's Index - phenology", xlab = "Site", title = "2022") + theme(legend.position = "none") + scale_x_discrete(labels=c('Low', 'Middle', 'High'))

##Schoener's index for fitness --------------------------------------------------

###PBM######
mat_PBM_fitness_2022 <- read.csv("files for code/2022/PBM_fitness_matrix_2022.csv", header = TRUE)
PBM_SI_fitness_2022 <- niche.overlap(mat_PBM_fitness_2022, method = "schoener")
hist(PBM_SI_fitness_2022)

#make data table with column names
PBM_22_fitness_melt <- melt(as.matrix(PBM_SI_fitness_2022), varnames = c("species1"))
colnames(PBM_22_fitness_melt) <- c("species1", "species2", "SI")

#add column for site
PBM_22_fitness_melt$site <- c("PBM")
print(PBM_22_fitness_melt)

###Pfeiler################
mat_Pfeiler_fitness_2022 <- read.csv("files for code/2022/Pfeiler_fitness_matrix_2022.csv")
Pfeiler_SI_fitness_2022 <- niche.overlap(mat_Pfeiler_fitness_2022, method = "schoener")
hist(Pfeiler_SI_fitness_2022)

#make dataframe with columns
Pfeiler_22_fitness_melt <- melt(as.matrix(Pfeiler_SI_fitness_2022), varnames = c("species1"))
colnames(Pfeiler_22_fitness_melt) <- c("species1", "species2", "SI")

#add column for site name
Pfeiler_22_fitness_melt$site <- c("Pfeiler")
print(Pfeiler_22_fitness_melt)


###Road#################
mat_Road_fitness_2022 <-read.csv("files for code/2022/Road_fitness_matrix_2022.csv", header = TRUE)  
Road_SI_fitness_2022<-niche.overlap(mat_Road_fitness_2022, method = "schoener")
hist(Road_SI_fitness_2022)

#make dataframe
Road_22_fitness_melt <- melt(as.matrix(Road_SI_fitness_2022), varnames = c("species1"))
head(Road_22_fitness_melt)
colnames(Road_22_fitness_melt) <- c("species1", "species2", "SI")

#add column for site name
Road_22_fitness_melt$site <- c("Road")
print(Road_22_fitness_melt)


###create merged dataframe for all sites fitness data 2022---------------------
Merged2022_fitness <- do.call("rbind", list(Road_22_fitness_melt, Pfeiler_22_fitness_melt, 
                                            PBM_22_fitness_melt))
head(Merged2022_fitness)


 
#add columns to categorize SI values 
Merged2022$category <- c("phenology")
print(Merged2022)

Merged2022_fitness$category <- c("fitness")
head(Merged2022_fitness)

#create merged dataframe for all sites phenology and fitness data 2022
library("reshape")

Merged2022_both <- do.call("rbind", list(Merged2022, Merged2022_fitness))
print(Merged2022_both)


#test distributions of flowering
kruskal.test(SI ~ site, data = Merged2021)
kruskal.test(SI ~ site, data = Merged2022)

install.packages("FSA")
library(FSA)
dunnTest(SI ~ site,
         data=Merged2022,
         method="bonferroni")
