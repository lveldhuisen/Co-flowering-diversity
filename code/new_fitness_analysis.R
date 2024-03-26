library(tidyverse)


#bring in data to combine
phenology <- read.csv("combined_raw_phenology.csv")
fitness <- read.csv("fitness.csv")

#rename week column to match fitness data
colnames(phenology)[3] <- 'Flowering_week'

#remove unnecessary columns 
fitness = subset(fitness, select = -c(6,7,8,9,10) )
phenology = subset(phenology, select = -c())

#combine into one dataframe

test <- merge(phenology, fitness, by.x=c('Flowering_week', 'Site','Species'), 
      by.y=c('Flowering_week', 'Site','Species'))
