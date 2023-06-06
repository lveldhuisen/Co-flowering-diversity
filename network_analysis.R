##network analysis with SI co-flowering values
 
install.packages("ggraph")
install.packages("tidygraph")
library(ggraph)
library(tidygraph)
library(tidyverse)
library(spaa)
library(reshape2)
library(reshape)
library(igraph)

###2021 SI calculations and turned into data tables -------------------------
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/RMBL/Summer 2021/Data files")

#PBM SI + dataframe 21
mat_PBM_2021 <- read.csv("PBM_phenology_matrix_2021.csv", header = TRUE)
PBM_SI_2021 <- niche.overlap(mat_PBM_2021, method = "schoener")
hist(PBM_SI_2021)

PBM_21_melt <- melt(as.matrix(PBM_SI_2021), varnames = c("species1"))
colnames(PBM_21_melt) <- c("species1", "species2", "SI")

PBM_21_melt$site <- c("PBM")
print(PBM_21_melt)

keeps <- c("species1","species2","SI")
PBM_21 = PBM_21_melt[keeps]
print(PBM_21)

forGephi_PBM <- PBM_21
head(forGephi_PBM)

write.csv(as.matrix(forGephi_PBM), file="/Users/leahvedlhuisen/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona\ PhD/Research/RMBL/Summer\ 2021/data\ files/forgephi_PBM.csv")

#Pfeiler SI + dataframe 21
mat_Pfeiler_2021 <- read.csv("pfeiler_phenology_matrix_2021.csv", header = TRUE)
Pfeiler_SI_2021 <- niche.overlap(mat_Pfeiler_2021, method = "schoener")
hist(Pfeiler_SI_2021)

Pfeiler_21_melt <- melt(as.matrix(Pfeiler_SI_2021), varnames = c("species1"))
colnames(Pfeiler_21_melt) <- c("species1", "species2", "SI")
head(Pfeiler_21_melt)

write.csv(as.matrix(forGephi_PBM), file="/Users/leahvedlhuisen/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona\ PhD/Research/Chapter 1/network analyses/pfeiler21.csv")


#Road SI + dataframe 21
mat_Road_2021 <-read.csv("road_phenology_matrix_2021.csv", header = TRUE)  
Road_SI_2021<-niche.overlap(mat_Road_2021, method = "schoener")
hist(Road_SI_2021)

Road_21_melt <- melt(as.matrix(Road_SI_2021), varnames = c("species1"))
colnames(Road_21_melt) <- c("species1", "species2", "SI")

head(Road_21_melt)

Merged2021 <- do.call("rbind", list(Road_21_melt, Pfeiler_21_melt, PBM_21_melt))
print(Merged2021)

#network analysis using igraph-------------------------------------------------- 
#####Road 2021##############################
install.packages("igraph")
library(igraph)

setwd("/Users/leahvedlhuisen/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1/network analyses")

road21data <- read.csv("cleanroad21.csv", header = TRUE)
head(road21data)

# Duplicate data frame
data_positive <- road21data                     
# Set negative values to 0
data_positive[data_positive < 4.46e-14] <- 0     
data_positive 
road21data <- data_positive
#remove x column 
road21data = subset(road21data, select = -c(X) )
#rename SI column to weight
colnames(road21data)[3] ="weight"
#remove the species that only interacts with itself (floating point)
road21data1 <- road21data %>% filter(species1 != "Mertensia.ciliata")
road21data1 <- na.omit(road21data)
fivenum(road21data1$weight)

graph_from_data_frame(d=road21data1, directed = FALSE)
head(road21data1)

write.csv(road21data1,file="/Users/leahvedlhuisen/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona\ PhD/Research/Chapter 1/phd-chapter-1/forteddy_road21.csv")


###make igraph object 
road21_igraph=graph_from_data_frame(d=road21data1,directed = FALSE)
head(road21_igraph)
is_weighted(road21_igraph)
print(road21_igraph)
summary(road21_igraph)

as_data_frame(road21_igraph)

plot(road21_igraph,edge.arrow.size=.5, vertex.color=communitiesRoad, vertex.size=3,
     vertex.frame.color="blue", vertex.label.color="black", 
     vertex.label.cex=.5, vertex.label.dist=2, edge.curved=0.5, layout=layout_with_lgl)


class(road21_igraph)

#modularity analysis based on Blondel et al 2008, same as Arceo-Gomez et al 2018
communitiesRoad <- cluster_louvain(road21_igraph, weights = NULL, resolution = 1)
membership(communitiesRoad)
R21mod <- modularity(communitiesRoad)

class(road21_igraph)
gsize(PBM22_igraph)

#####Pfeiler 2021##############################

library(dplyr)
library(tidyr)
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1/network analyses")

p21data <- read.csv("cleanpfeiler21.csv", header = TRUE)
head(p21data)

#remove X column 
p21data = subset(p21data, select = -c(X) )
#rename SI column to weight
colnames(p21data)[3] ="weight"

# Duplicate data frame
pfeiler_positive <- p21data                    
# Set negative values to 0
pfeiler_positive[pfeiler_positive < 4.46e-14] <- 0     
pfeiler_positive 
p21data <- pfeiler_positive

p21data <- na.omit(p21data)
fivenum(p21data$weight)


graph_from_data_frame(d=p21data, directed = FALSE)

pfeiler21_igraph=graph_from_data_frame(d=p21data,directed = FALSE)
print(pfeiler21_igraph)
head(pfeiler21_igraph)
class(pfeiler21_igraph)

#modularity 
communitiesPF <- cluster_louvain(pfeiler21_igraph, weights = NULL, resolution = 1)
membership(communitiesPF)
Pf21mod <- modularity(communitiesPF)


#####PBM 2021##############################
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1/network analyses")

PBM21data <- read.csv("cleanPBM21.csv", header = TRUE)
head(PBM21data)


#remove extra x column
PBM21data = subset(PBM21data, select = -c(X) )
head(PBM21data)
#rename SI column to weight
colnames(PBM21data)[3] ="weight"
head(PBM21data)


# Duplicate data frame
PBM_positive <- PBM21data                   
# Set negative values to 0
PBM_positive[PBM_positive < 4.46e-14] <- 0     
PBM_positive
PBM21data <- PBM_positive
#remove all NAs
PBM21data <- na.omit(PBM21data)
fivenum(PBM21data$weight)


graph_from_data_frame(d=PBM21data, directed = FALSE)

PBM21_igraph=graph_from_data_frame(d=PBM21data,directed = FALSE)
print(PBM21_igraph)
head(PBM21_igraph)

plot(PBM21_igraph,edge.arrow.size=.5, vertex.color="gold", vertex.size=3, 
     vertex.frame.color="blue", vertex.label=V(pfeiler21_igraph)$species1, vertex.label.color="black", 
     vertex.label.cex=.5, vertex.label.dist=2, edge.curved=0.5,edge.width=weights, layout=layout_with_lgl)

class(PBM21_igraph)

#modularity 
communitiesPBM <- cluster_louvain(PBM21_igraph, weights = NULL, resolution = 1)
membership(communitiesPBM)
PBM21mod <- modularity(communitiesPBM)

#####Road 2022##############################
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1/network analyses")

Road22data <- read.csv("cleanRoad22.csv", header = TRUE)
head(Road22data)

#remove extra x column
Road22data = subset(Road22data, select = -c(X) )
head(Road22data)
#rename SI column to weight
colnames(Road22data)[3] ="weight"
head(Road22data)

# Duplicate data frame
Road_positive <- Road22data                   
# Set negative values to 0
Road_positive[Road_positive < 4.46e-14] <- 0     
Road_positive
Road22data <- Road_positive
#remove all NAs
Road22data <- na.omit(Road22data)
fivenum(Road22data$weight)


graph_from_data_frame(d=Road22data, directed = FALSE)

Road22_igraph=graph_from_data_frame(d=Road22data,directed = FALSE)
print(Road22_igraph)
head(Road22_igraph)

plot(Road22_igraph,edge.arrow.size=.5, vertex.color="gold", vertex.size=3, 
     vertex.frame.color="blue", vertex.label=V(pfeiler21_igraph)$species1, vertex.label.color="black", 
     vertex.label.cex=.5, vertex.label.dist=2, edge.curved=0.5,edge.width=weights, layout=layout_with_lgl)

class(Road22_igraph)

#modularity 
communitiesRoad22 <- cluster_louvain(Road22_igraph, weights = NULL, resolution = 1)
membership(communitiesRoad22)
R22mod <- modularity(communitiesRoad22)

#####Pfeiler 2022##############################
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1/network analyses")

Pfeiler22data <- read.csv("cleanPfeiler22.csv", header = TRUE)
head(Pfeiler22data)

#remove x column 
Pfeiler22data = subset(Pfeiler22data, select = -c(X) )
#rename SI column to weight
colnames(Pfeiler22data)[3] ="weight"
head(Pfeiler22data)

# Duplicate data frame
Pfeiler_positive <- Pfeiler22data                   
# Set negative values to 0
Pfeiler_positive[Pfeiler_positive < 4.46e-14] <- 0     
Pfeiler_positive
Pfeiler22data <- Pfeiler_positive
#remove all NAs
Pfeiler22data <- na.omit(Pfeiler22data)
fivenum(Pfeiler22data$weight)
hist(Pfeiler22data$weight)

graph_from_data_frame(d=Pfeiler22data, directed = FALSE)

Pfeiler22_igraph=graph_from_data_frame(d=Pfeiler22data,directed = FALSE)
print(Pfeiler22_igraph)
head(Pfeiler22_igraph)

plot(Pfeiler22_igraph,edge.arrow.size=.5, vertex.color="gold", vertex.size=3, 
     vertex.frame.color="blue", vertex.label=V(pfeiler21_igraph)$species1, vertex.label.color="black", 
     vertex.label.cex=.5, vertex.label.dist=2, edge.curved=0.5,edge.width=weights, layout=layout_with_lgl)

class(Pfeiler22_igraph)

#modularity 
communitiesPf22 <- cluster_louvain(Pfeiler22_igraph, weights = NULL, resolution = 1)
membership(communitiesPf22)
Pf22mod <- modularity(communitiesPf22)

#####Pfeiler 2022##############################
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1/network analyses")

PBM22data <- read.csv("cleanPBM22.csv", header = TRUE)
head(PBM22data)

#remove x column 
PBM22data = subset(PBM22data, select = -c(X) )
#rename SI column to weight
colnames(PBM22data)[3] ="weight"
head(PBM22data)

# Duplicate data frame
PBM_positive <- PBM22data                   
# Set negative values to 0
PBM_positive[PBM_positive < 4.46e-14] <- 0     
PBM_positive
PBM22data <- PBM_positive
#remove all NAs
PBM22data <- na.omit(PBM22data)
fivenum(PBM22data$weight)


graph_from_data_frame(d=PBM22data, directed = FALSE)

PBM22_igraph=graph_from_data_frame(d=PBM22data,directed = FALSE)
print(PBM22_igraph)
head(PBM22_igraph)

plot(PBM22_igraph,edge.arrow.size=.5, vertex.color="gold", vertex.size=3, 
     vertex.frame.color="blue", vertex.label=V(pfeiler21_igraph)$species1, vertex.label.color="black", 
     vertex.label.cex=.5, vertex.label.dist=2, edge.curved=0.5,edge.width=weights, layout=layout_with_lgl)

class(PBM22_igraph)

#modularity 
communitiesPBM22 <- cluster_louvain(PBM22_igraph, weights = NULL, resolution = 1)
membership(communitiesPBM22)
PBM22mod <- modularity(communitiesPBM22)


#Null models for all sites and years--------------------------------------------
install.packages("bipartite")
library(bipartite)
install.packages("vegan")
library(vegan)
install.packages("Matrix")
library(Matrix)

setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1/network analyses")

#####Road 2021##############################
#turn igraph object from above back into matrix 
road21_matrix <- as.matrix(as_adjacency_matrix(road21_igraph))
#create null networks 
road21_null <- nullmodel(road21_matrix, method="r2d")
print(road21_null)
class(road21_null)

graph

#create empty vector
null_modularity = c()
#create list with null network modularity values 
for (i in 1:length(road21_null)) {
  current_model = road21_null[[i]]
  igraphcurrent = graph_from_adjacency_matrix(current_model,mode = "undirected")
  current_analysis = cluster_louvain(igraphcurrent, weights = NULL, resolution = 1)
  current_modularity = modularity(current_analysis)
  null_modularity = append(null_modularity,current_modularity)
  
}

#calculate z scores for Road 21 nulls 
hist(null_modularity)

R21Z <- (R21mod - mean(null_modularity))/sd(null_modularity)
R21Z

#####Pfeiler 2021############################## 
#turn igraph object back into matrix 
pfeiler21_matrix <- as.matrix(as_adjacency_matrix(pfeiler21_igraph))
#create nullnetworks 
pfeiler21_null <- nullmodel(pfeiler21_matrix, method="r2d")
print(pfeiler21_null)
class(pfeiler21_null)

graph

#create empty vector
PF21null_modularity = c()
#create list with null network modularity values 
for (i in 1:length(pfeiler21_null)) {
  current_model = pfeiler21_null[[i]]
  igraphcurrent = graph_from_adjacency_matrix(current_model,mode = "undirected")
  current_analysis = cluster_louvain(igraphcurrent, weights = NULL, resolution = 1)
  current_modularity = modularity(current_analysis)
  PF21null_modularity = append(PF21null_modularity,current_modularity)
  
}
#calculate z scores for pfeiler 21 nulls 
PF21Z <- (Pf21mod - mean(PF21null_modularity))/sd(PF21null_modularity)
PF21Z

#####PBM 2021##############################
#turn igraph object back into matrix 
PBM21_matrix <- as.matrix(as_adjacency_matrix(PBM21_igraph))
#create nullnetworks 
PBM21_null <- nullmodel(PBM21_matrix, method="r2d")
print(PBM21_null)
class(PBM21_null)

graph

#create empty vector
PBM21null_modularity = c()
#create list with null network modularity values 
for (i in 1:length(PBM21_null)) {
  current_model = PBM21_null[[i]]
  igraphcurrent = graph_from_adjacency_matrix(current_model,mode = "undirected")
  current_analysis = cluster_louvain(igraphcurrent, weights = NULL, resolution = 1)
  current_modularity = modularity(current_analysis)
  PBM21null_modularity = append(PBM21null_modularity,current_modularity)
  
}

#calculate z scores for PBM 21 nulls 

PBM21Z <- (PBM21mod - mean(PBM21null_modularity))/sd(PBM21null_modularity)
PBM21Z

hist(PBM21null_modularity)


####2022 data######################
#####Road 2022##############################
#turn igraph object back into matrix 
road22_matrix <- as.matrix(as_adjacency_matrix(Road22_igraph))
#create null networks 
road22_null <- nullmodel(road22_matrix, method="r2d")
print(road22_null)
class(road22_null)

#create empty vector
R22null_modularity = c()
#create list with null network modularity values 
for (i in 1:length(road22_null)) {
  current_model = road22_null[[i]]
  igraphcurrent = graph_from_adjacency_matrix(current_model,mode = "undirected")
  current_analysis = cluster_louvain(igraphcurrent, weights = NULL, resolution = 1)
  current_modularity = modularity(current_analysis)
  R22null_modularity = append(R22null_modularity,current_modularity)
  
}

#calculate z scores for Road 22 nulls 
R22Z <- (R22mod - mean(R22null_modularity))/sd(R22null_modularity)
R22Z

#####Pfeiler 2022##############################
#turn igraph object back into matrix 
pfeiler22_matrix <- as.matrix(as_adjacency_matrix(Pfeiler22_igraph))
#create null networks 
pfeiler22_null <- nullmodel(pfeiler22_matrix, method="r2d")
print(pfeiler22_null)
class(pfeiler22_null)

#create empty vector
Pf22null_modularity = c()
#create list with null network modularity values 
for (i in 1:length(pfeiler22_null)) {
  current_model = pfeiler22_null[[i]]
  igraphcurrent = graph_from_adjacency_matrix(current_model,mode = "undirected")
  current_analysis = cluster_louvain(igraphcurrent, weights = NULL, resolution = 1)
  current_modularity = modularity(current_analysis)
  Pf22null_modularity = append(Pf22null_modularity,current_modularity)
  
}

#calculate z scores for pfeiler 2022 nulls 
Pf22Z <- (Pf22mod - mean(Pf22null_modularity))/sd(Pf22null_modularity)
Pf22Z

#####PBM 2022##############################
#turn igraph object back into matrix 
PBM22_matrix <- as.matrix(as_adjacency_matrix(PBM22_igraph))
#create null networks 
PBM22_null <- nullmodel(PBM22_matrix, method="r2d")
print(PBM22_null)
class(PBM22_null)

#create empty vector
PBM22null_modularity = c()
#create list with null network modularity values 
for (i in 1:length(PBM22_null)) {
  current_model = PBM22_null[[i]]
  igraphcurrent = graph_from_adjacency_matrix(current_model,mode = "undirected")
  current_analysis = cluster_louvain(igraphcurrent, weights = NULL, resolution = 1)
  current_modularity = modularity(current_analysis)
  PBM22null_modularity = append(PBM22null_modularity,current_modularity)
  
}

#calculate z scores for pbm 2022 nulls 
PBM22Z <- (PBM22mod - mean(PBM22null_modularity))/sd(PBM22null_modularity)
PBM22Z
