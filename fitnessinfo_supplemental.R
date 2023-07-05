#calculate mean and standard deviation of fruits/unit and seeds/fruit for supplemental info 
library(tidyverse)
setwd("/Users/leahvedlhuisen/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1")

fitness2021 <- read.csv("2021_fitness_data1.csv", header = TRUE)

fitness2021_double <- fitness2021
na.omit(fitness2021_double)

##2021------------------------
###standard deviation for 2021 fruit####### 
SD_fruit_2021 <- aggregate(fitness2021_double$Number_of_fruits_per_tagged_unit, list(fitness2021_double$Species), FUN=sd) 
Year = c(2021)
SD_fruit_2021$Year = Year

names(SD_fruit_2021)[1] <- "Species"
names(SD_fruit_2021)[2] <- "SD fruit"

###mean for 2021 fruit########
mean_fruit_2021 <- aggregate(fitness2021_double$Number_of_fruits_per_tagged_unit, list(fitness2021_double$Species), FUN=mean) 
Year = c(2021)
mean_fruit_2021$Year = Year

names(mean_fruit_2021)[1] <- "Species"
names(mean_fruit_2021)[2] <- "Mean fruit"

###standard deviation for 2021 seeds####### 
SD_seeds_2021 <- aggregate(fitness2021_double$Seeds.fruit, list(fitness2021_double$Species), FUN=sd) 
Year = c(2021)
SD_seeds_2021$Year = Year

names(SD_seeds_2021)[1] <- "Species"
names(SD_seeds_2021)[2] <- "SD seeds"