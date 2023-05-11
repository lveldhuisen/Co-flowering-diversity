#calculate Schoener's Index by Site for only 2021 data
install.packages("tidyverse")
library(tidyverse)

setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/RMBL/Summer 2021/Data files")

dat <- read.csv("Phenology_fitness_data_2021_trimmed.csv", header = TRUE)

#create new column with proportion flowering 
dat2 <- dat %>% group_by(Site, Species) %>%
  mutate(total_flowers = sum(Number_flowering_units),
         p_ik = Number_flowering_units/total_flowers)

#make matrix 
mat = data.matrix(dat2[1:286,1:9])

#testing for loops 
add = 0
for (i in 1:286){
  add = add + mat[i,9]
}
add

#make matrices for each site with only week, site, species and proportion flowering
wanted_columns = c(1,5,9)

site1_mat = mat[which(mat[,3] %in% 1),wanted_columns]
site2_mat = mat[which(mat[,3] %in% 2),wanted_columns]
site3_mat = mat[which(mat[,3] %in% 3),wanted_columns]



#make empty vectors 
site1_species = c()
site1_shoner = c()
site2_species = c()
site2_shoner = c()
site3_species = c()
site3_shoner = c()

#testing for loops 
add = 0
for (i in 1:max(site1_mat[,2])){
  spec_1 = which(site1_mat[,2] %in% i)
  if (is.na(spec_1[1])){
    append(site1_species,i)
  }
}
site1_species

for (j in (i+1):max(site1_mat[,2])){
  spec_2 = which(site1_mat[,2] %in% j)
  append(site1_species,spec_1)
}
write.table(site1_mat, file = "data.txt", sep = "\t", row.names = FALSE, col.names = TRUE)

# Read the data into a dataframe
data <- read.table("data.txt", header = TRUE)

# Create a new dataframe with only unique pairs of species
unique_species <- unique(data[,c("Species")])

# Create a list of all possible pairs of species
pairs_of_species <- combn(unique_species$Species, 2)

# Create an empty dataframe to store the results
results <- data.frame(species1 = character(), species2 = character(), S = numeric())

# Loop through each pair of species
for(i in 1:ncol(pairs_of_species)) {
  species1 <- pairs_of_species[1,i]
  species2 <- pairs_of_species[2,i]
  
  # Subset the data for each species in the pair
  species1_data <- subset(data, Species == species1)
  species2_data <- subset(data, Species == species2)
  
  # Merge the data for each species in the pair
  merged_data <- merge(species1_data, species2_data, by = "Week", all = TRUE)
  
  #If there is no input for a certain week it has a S value of 0
  merged_data[is.na(merged_data)] <- 0
  
  # Calculate the Schoener's index for each pair
  S <- sum(merged_data$p_ik.x * log(merged_data$p_ik.x + merged_data$p_ik.y))
  
  # Add the results to the results dataframe
  results <- rbind(results, data.frame(species1 = species1, species2 = species2, S = S))
}

#This is the code I've been using from here down -----------------------------
#test spaa package------------------------------------------------------ 
install.packages("spaa")
library("spaa")
library(reshape)
library(reshape2)

#2021 SI calculations and turned into data tables -------------------------
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/RMBL/Summer 2021/Data files")

#PBM SI + dataframe 21
mat_PBM_2021 <- read.csv("PBM_phenology_matrix_2021.csv", header = TRUE)
PBM_SI_2021 <- niche.overlap(mat_PBM_2021, method = "schoener")
hist(PBM_SI_2021)

PBM_21_melt <- melt(as.matrix(PBM_SI_2021), varnames = c("species1"))
colnames(PBM_21_melt) <- c("species1", "species2", "SI")

PBM_21_melt$site <- c("PBM")
print(PBM_21_melt)

#Pfeiler SI + dataframe 21
mat_Pfeiler_2021 <- read.csv("pfeiler_phenology_matrix_2021.csv", header = TRUE)
Pfeiler_SI_2021 <- niche.overlap(mat_Pfeiler_2021, method = "schoener")
hist(Pfeiler_SI_2021)

Pfeiler_21_melt <- melt(as.matrix(Pfeiler_SI_2021), varnames = c("species1"))
colnames(Pfeiler_21_melt) <- c("species1", "species2", "SI")

Pfeiler_21_melt$site <- c("Pfeiler")
print(Pfeiler_21_melt)

 #Road SI + dataframe 21
mat_Road_2021 <-read.csv("road_phenology_matrix_2021.csv", header = TRUE)  
 Road_SI_2021<-niche.overlap(mat_Road_2021, method = "schoener")
 hist(Road_SI_2021)
 
 Road_21_melt <- melt(as.matrix(Road_SI_2021), varnames = c("species1"))
 colnames(Road_21_melt) <- c("species1", "species2", "SI")
 
 Road_21_melt$site <- c("Road")
 print(Road_21_melt)
 
 Merged2021 <- do.call("rbind", list(Road_21_melt, Pfeiler_21_melt, PBM_21_melt))
 print(Merged2021)
 
 #compare distributions for SI between sites 2021 
 library(tidyverse)
 library(ggpubr)
 
 kruskal.test(SI ~ site, data = Merged2021)
 
 chisq.test(Merged2021$site,Merged2021$SI)
 
 ggboxplot(Merged2021, x = "site", y = "SI", 
           color = "site", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
           order = c("Road", "Pfeiler", "PBM"),
           ylab = "Schoener's Index", xlab = "Site", title = "2021 phenology")
 ###remove zeros----------------------------
 Merged_2021_nozeros <- filter(Merged2021, SI > 1.1E-15)
 
 kruskal.test(SI ~ site, data = Merged_2021_nozeros)
 
 
 ggboxplot(Merged_2021_nozeros, x = "site", y = "SI", 
           color = "site", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
           order = c("Road", "Pfeiler", "PBM"),
           ylab = "Schoener's Index", xlab = "Site", title = "2021 phenology")
 
 
 #SI for fitness data 2021--------------------------------------------------
 setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/RMBL/Summer 2021/Data files")
 library(tidyverse)
 library(ggpubr)
 library(spaa)
 
#SI for PBM fitness 2021
  mat_PBM_fitness_2021 <- read.csv("PBM_fitness_formatrix.csv", header = TRUE)
 PBM_SI_fitness_2021 <- niche.overlap(mat_PBM_fitness_2021, method = "schoener")
 hist(PBM_SI_fitness_2021)
 
 PBM_21_fitness_melt <- melt(as.matrix(PBM_SI_fitness_2021), varnames = c("species1"))
 colnames(PBM_21_fitness_melt) <- c("species1", "species2", "SI_f")
 
 PBM_21_fitness_melt$site <- c("PBM")
 print(PBM_21_fitness_melt)
 
 #SI for Pfeiler fitness 2021 
 mat_Pfeiler_fitness_2021 <- read.csv("Pfeiler_fitness_formatrix.csv", header = TRUE)
 Pfeiler_SI_fitness_2021 <- niche.overlap(mat_Pfeiler_fitness_2021, method = "schoener")
 hist(Pfeiler_SI_fitness_2021)
 
 Pfeiler_21_fitness_melt <- melt(as.matrix(Pfeiler_SI_fitness_2021), varnames = c("species1"))
 colnames(Pfeiler_21_fitness_melt) <- c("species1", "species2", "SI_f")
 
 Pfeiler_21_fitness_melt$site <- c("Pfeiler")
 print(Pfeiler_21_fitness_melt)
 
 
 #SI for Road fitness 2021 
 mat_Road_fitness_2021 <-read.csv("road_fitness_formatrix.csv", header = TRUE)  
 Road_SI_fitness_2021<-niche.overlap(mat_Road_fitness_2021, method = "schoener")
 hist(Road_SI_fitness_2021)
 
 Road_21_fitness_melt <- melt(as.matrix(Road_SI_fitness_2021), varnames = c("species1"))
 head(Road_21_fitness_melt)
 colnames(Road_21_fitness_melt) <- c("species1", "species2", "SI_f")
 
 Road_21_fitness_melt$site <- c("Road")
 print(Road_21_fitness_melt)
 
 
 #create merged dataframe for all sites fitness data 2021---------------------
 Merged2021_fitness <- do.call("rbind", list(Road_21_fitness_melt, Pfeiler_21_fitness_melt, 
                                             PBM_21_fitness_melt))
 print(Merged2021_fitness)
 
 #compare distributions for fitness SI between sites 2021 
 library(tidyverse)
 library(ggpubr)
 
 kruskal.test(SI_f ~ site, data = Merged2021_fitness)
 
 chisq.test(Merged2021_fitness$site,Merged2021_fitness$SI_f)
 
 ggboxplot(Merged2021_fitness, x = "site", y = "SI_f", 
           color = "site", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
           order = c("Road", "Pfeiler", "PBM"),
           ylab = "Schoener's Index", xlab = "Site", title = "2021 fitness")
 
 ###remove 0s and compare fitness SI between sites 
 Merged2021_fitness_nozeros <- filter(Merged2021_fitness, SI_f > 1.1E-15)
 kruskal.test(SI_f ~ site, data = Merged2021_fitness_nozeros)
 
 ggboxplot(Merged2021_fitness_nozeros, x = "site", y = "SI_f", 
           color = "site", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
           order = c("Road", "Pfeiler", "PBM"),
           ylab = "Schoener's Index", xlab = "Site", title = "2021 fitness")

 #2022 SI calculations, everything below here is 2022 data only---------------
 setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/RMBL/Summer 2022/data files")
 
 mat_PBM_2022 <- read.csv("PBM_phenology_matrix_2022.csv", header = TRUE)
 PBM_SI_22 <- niche.overlap(mat_PBM_2022, method = "schoener")
 hist(PBM_SI_22)
 
mat_Pfeiler_2022 <- read.csv("Pfeiler_phenology_matrix_2022.csv", header = TRUE)
Pfeiler_SI_22 <- niche.overlap(mat_Pfeiler_2022, method = "schoener")
hist(Pfeiler_SI_22)

print(Pfeiler_SI_22)

mat_Road_2022 <- read.csv("Road_phenology_matrix_2022.csv", header = TRUE)
Road_SI_22 <- niche.overlap(mat_Road_2022, method = "schoener")
print(Road_SI_22)



#turn SI matrices into data tables for 2022 data ------------------------------
install.packages("reshape2")
install.packages("tidyverse")
library("reshape2")
library("tidyverse")

Road_22_melt <- melt(as.matrix(Road_SI_22), varnames = c("species1"))
colnames(Road_22_melt) <- c("species1", "species2", "SI")



Road_22_melt$site <- c("Road")
print(Road_22_melt)

keeps <- c("species1","species2","SI","site")
Road_22_melt = Road_22_melt[keeps]
print(Road_22_melt)

Pfeiler_22_melt <- melt(as.matrix(Pfeiler_SI_22), varnames = c("species1"))
colnames(Pfeiler_22_melt) <- c("species1", "species2", "SI")
print(Pfeiler_22_melt)

Pfeiler_22_melt$site <- c("Pfeiler")
head(Pfeiler_22_melt)

keeps <- c("species1","species2","SI","site")
Pfeiler_22_melt = Pfeiler_22_melt[keeps]
print(Pfeiler_22_melt)

PBM_22_melt <- melt(as.matrix(PBM_SI_22), varnames = c("species1"))
colnames(PBM_22_melt) <- c("species1", "species2", "SI")
print(PBM_22_melt)



PBM_22_melt$site <- NA
PBM_22_melt$site <- c("PBM")

keeps <- c("species1","species2","SI","site")
PBM_22_melt = PBM_22_melt[keeps]
print(PBM_22_melt)

#combine all three sites into 1 dataframe-------------------------------------
install.packages("reshape")
library("reshape")

Merged2022 <- do.call("rbind", list(Road_22_melt, Pfeiler_22_melt, PBM_22_melt))
print(Merged2022)

write.csv(as.matrix(Merged), file="/Users/leahvedlhuisen/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona\ PhD/Research/RMBL/Summer\ 2022/data\ files/SI_allsites_2022.csv")

#compare distributions for 2022 phenology 
unique(Merged2022$site)

library(dplyr)
library(ggpubr)
library(ggplot2)

#remove zeros 
Merged22_nozero <- filter(Merged2022, SI > 1.11E-15)
head(Merged22_nozero)
hist(Merged22_nozero$SI)

kruskal.test(SI ~ site, data = Merged22_nozero)
ggboxplot(Merged22_nozero, x = "site", y = "SI", 
          color = "site", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Road", "Pfeiler", "PBM"),
          ylab = "Schoener's Index", xlab = "Site", title = "2022")

#SI for fitness data 2022--------------------------------------------------
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/RMBL/Summer 2022/data files")
library(tidyverse)
library(ggpubr)
library(spaa)

#SI for PBM fitness 2022
mat_PBM_fitness_2022 <- read.csv("fitness_correlation_PBM_22.csv", header = TRUE)
PBM_SI_fitness_2022 <- niche.overlap(mat_PBM_fitness_2022, method = "schoener")
hist(PBM_SI_fitness_2022)

PBM_22_fitness_melt <- melt(as.matrix(PBM_SI_fitness_2022), varnames = c("species1"))
colnames(PBM_22_fitness_melt) <- c("species1", "species2", "SI")

PBM_22_fitness_melt$site <- c("PBM")
print(PBM_22_fitness_melt)

#SI for Pfeiler fitness 2022 
mat_Pfeiler_fitness_2022 <- read.csv("fitness_correlation_Pfeiler_22.csv", header = TRUE)
Pfeiler_SI_fitness_2022 <- niche.overlap(mat_Pfeiler_fitness_2022, method = "schoener")
hist(Pfeiler_SI_fitness_2022)

Pfeiler_22_fitness_melt <- melt(as.matrix(Pfeiler_SI_fitness_2022), varnames = c("species1"))
colnames(Pfeiler_22_fitness_melt) <- c("species1", "species2", "SI")

Pfeiler_22_fitness_melt$site <- c("Pfeiler")
print(Pfeiler_22_fitness_melt)


#SI for Road fitness 2022 
mat_Road_fitness_2022 <-read.csv("fitness_correlation_Road_22.csv", header = TRUE)  
Road_SI_fitness_2022<-niche.overlap(mat_Road_fitness_2022, method = "schoener")
hist(Road_SI_fitness_2022)

Road_22_fitness_melt <- melt(as.matrix(Road_SI_fitness_2022), varnames = c("species1"))
head(Road_22_fitness_melt)
colnames(Road_22_fitness_melt) <- c("species1", "species2", "SI")

Road_22_fitness_melt$site <- c("Road")
print(Road_22_fitness_melt)


#create merged dataframe for all sites fitness data 2022---------------------
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



#compare distributions for fitness SI between sites 2022
library(tidyverse)
library(ggpubr)

kruskal.test(SI_f ~ site, data = Merged2022_fitness)

chisq.test(Merged2022_fitness$site,Merged2022_fitness$SI_f)

ggboxplot(Merged2022_fitness, x = "site", y = "SI_f", 
          color = "site", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Road", "Pfeiler", "PBM"),
          ylab = "Schoener's Index", xlab = "Site", title = "2022 fitness")

#remove zeros 
Merged2022_fitness_nozero <- filter(Merged2022_fitness, SI > 1.11E-15)
head(Merged2022_fitness_nozero)
hist(Merged2022_fitness_nozero$SI)

kruskal.test(SI ~ site, data = Merged2022_fitness_nozero)

ggboxplot(Merged2022_fitness_nozero, x = "site", y = "SI", 
          color = "site", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Road", "Pfeiler", "PBM"),
          ylab = "Schoener's Index", xlab = "Site", title = "2022 fitness")


# mae averaging coflowering cuz she insisted ------------------------------


write.csv(as.matrix(PBM_SI_22), file="/Users/leahvedlhuisen/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona\ PhD/Research/RMBL/Summer\ 2022/data\ files/cofloweringMatrix.csv")
# average in excel cuz we're lazy
pbm_av <- read.csv(file="/Users/leahvedlhuisen/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona\ PhD/Research/RMBL/Summer\ 2022/data\ files/cofloweringAverage_PBM2022.csv")

write.csv(as.matrix(Road_SI_22), file="/Users/leahvedlhuisen/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona\ PhD/Research/RMBL/Summer\ 2022/data\ files/cofloweringMatrix_Road2022.csv")

# average in excel cuz we're lazy
Road_av <- read.csv(file="/Users/leahvedlhuisen/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona\ PhD/Research/RMBL/Summer\ 2022/data\ files/cofloweringAverages_Road2022.csv")

write.csv(as.matrix(Pfeiler_SI_22), file="/Users/leahvedlhuisen/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona\ PhD/Research/RMBL/Summer\ 2022/data\ files/cofloweringMatrix_Pfeiler2022.csv")
# average in excel cuz we're lazy
Pfeiler_av <- read.csv(file="/Users/leahvedlhuisen/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona\ PhD/Research/RMBL/Summer\ 2022/data\ files/cofloweringAverage_Pfeiler2022.csv")

# add site names
pbm_av$site <- c("PBM")
Road_av$site <- c("Road")
Pfeiler_av$site <- c("Pfeiler")

MergedAverages <- do.call("rbind", list(pbm_av, Road_av, Pfeiler_av))

chisq.test(MergedAverages$site, MergedAverages$average)
chisq.test(Merged$site,Merged$SI)

kruskal.test(average ~ site, data = MergedAverages)

install.packages("dplyr")
library(dplyr)

hist(MergedAverages$average)
res.aov <- aov(average ~ site, data = MergedAverages)
summary(res.aov)

ggboxplot(MergedAverages, x = "site", y = "average", 
          color = "site", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Road", "Pfeiler", "PBM"),
          ylab = "average SI", xlab = "site")
  