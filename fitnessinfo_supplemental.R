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
names(SD_fruit_2021)[2] <- "SD fruits per unit"

###mean for 2021 fruit########
mean_fruit_2021 <- aggregate(fitness2021_double$Number_of_fruits_per_tagged_unit, list(fitness2021_double$Species), FUN=mean) 
Year = c(2021)
mean_fruit_2021$Year = Year

names(mean_fruit_2021)[1] <- "Species"
names(mean_fruit_2021)[2] <- "Mean fruit per unit"

###standard deviation for 2021 seeds####### 
SD_seeds_2021 <- aggregate(fitness2021_double$Seeds.fruit, list(fitness2021_double$Species), FUN=sd) 
Year = c(2021)
SD_seeds_2021$Year = Year

names(SD_seeds_2021)[1] <- "Species"
names(SD_seeds_2021)[2] <- "SD seeds per fruit"

###mean for 2021 seeds########
mean_seeds_2021 <- aggregate(fitness2021_double$Seeds.fruit, list(fitness2021_double$Species), FUN=mean) 
Year = c(2021)
mean_seeds_2021$Year = Year

names(mean_seeds_2021)[1] <- "Species"
names(mean_seeds_2021)[2] <- "Mean seeds per fruit"

##combine 2021 into one table 
df_merge <- merge(SD_fruit_2021, mean_fruit_2021, by = "Species")
df.fruits = subset(df_merge, select = -c(Year.x) )

merge_seeds <- merge(SD_seeds_2021, mean_seeds_2021, by = "Species")
df.seeds = subset(merge_seeds, select = -c(Year.x) )

merge_all <- merge(df.seeds, df.fruits, by = "Species")
merge_all = subset(merge_all, select = -c(Year.y.x) )

##2022----------------------------------------------------------
fitness2022 <- read.csv("2022_fitness_data.csv", header = TRUE)

na.omit(fitness2022)

###standard deviation for 2022 fruit####### 
SD_fruit_2022 <- aggregate(fitness2022$Number_fruits_per_unit, list(fitness2022$Species), FUN=sd) 
Year = c(2022)
SD_fruit_2022$Year = Year

names(SD_fruit_2022)[1] <- "Species"
names(SD_fruit_2022)[2] <- "SD fruits per unit"

###mean for 2022 fruit########
mean_fruit_2022 <- aggregate(fitness2022$Number_fruits_per_unit, list(fitness2022$Species), FUN=mean) 
Year = c(2022)
mean_fruit_2022$Year = Year

names(mean_fruit_2022)[1] <- "Species"
names(mean_fruit_2022)[2] <- "Mean fruit per unit"

###standard deviation for 2022 seeds####### 
SD_seeds_2022 <- aggregate(fitness2022$Seeds_per_fruit, list(fitness2022$Species), FUN=sd) 
Year = c(2022)
SD_seeds_2022$Year = Year

names(SD_seeds_2022)[1] <- "Species"
names(SD_seeds_2022)[2] <- "SD seeds per fruit"

###mean for 2022 seeds########

fitness2022_extra <- fitness2022

fitness2022_extra <- fitness2022_extra[-c(221, 222, 223, 224,225), ]

mean_seeds_2022 <- read.csv("2022_mean_seedsperfruit.csv", header = TRUE)
Year = c(2022)
mean_seeds_2022$Year = Year

names(mean_seeds_2022)[1] <- "Species"
names(mean_seeds_2022)[2] <- "Mean seeds per fruit"


##combine 2022 into one table 
df_merge <- merge(SD_fruit_2022, mean_fruit_2022, by = "Species")
df.fruits.22 = subset(df_merge, select = -c(Year.x) )
names(df.fruits.22)[4] <- "Year"


merge_seeds22 <- merge(SD_seeds_2022, mean_seeds_2022, by = "Species")
df.seeds22 = subset(merge_seeds22, select = -c(Year.x) )
names(df.seeds22)[4] <- "Year"

merge_all22 <- merge(df.seeds22, df.fruits.22, by = "Species")
merge_all22 = subset(merge_all22, select = -c(Year.x) )
names(merge_all22)[6] <- "Year"

##turn 2021 and 2022 combined tables into excel files 
write.csv(merge_all, "/Users/leahvedlhuisen/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1\fitness_summary2021.csv", row.names=FALSE)

write.csv(merge_all22, "/Users/leahvedlhuisen/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1\fitness_summary2022.csv", row.names=FALSE)


