library(tidyverse)
library(dplyr)
library(ggpubr)


#bring in data to combine
phenology <- read.csv("phenology.csv")
phenology_wmodules <- read.csv("combined_raw_phenology.csv")
fitness <- read.csv("fitness.csv")

#bring modules into phenology df
names(phenology_wmodules)[3] <- "Flowering_week"
test2 <- left_join(phenology, phenology_wmodules, by=c('Flowering_week', 'Site','Species', "Year"))

#fix column categories 
fitness$Year <- as.integer(fitness$Year)
fitness$Flowering_week <- as.integer(fitness$Flowering_week)

#remove unnecessary columns 
fitness = subset(fitness, select = -c(1,6,7,8,9,10) )
phenology = subset(phenology, select = -c(2,7))

#remove underscores from species names in fitness df 
fitness$Species<-gsub("_", " ", fitness$Species)

#fix viola typo
phenology$Species<-gsub("Viola nuttalliii", "Viola nuttallii", phenology$Species)
combined_df$Species<-gsub("Senecio interrigrimus", "Senecio integerrimus",combined_df$Species)
combined_df$Species<-gsub("Lupinus spp", "Lupinus bakeri", combined_df$Species)
combined_df$Species<-gsub("Dasiphora fruticosa ", "Dasiphora fruticosa", combined_df$Species)
combined_df$Species<-gsub("Eriogonum umbellatum var. Porteri", "Eriogonum umbellatum", combined_df$Species)
combined_df$Species<-gsub("Aquilegia caerulea", "Aquilegia coerulea", combined_df$Species)
combined_df$Species<-gsub("Gayophytum spp", "Gayophytum diffusum", combined_df$Species)

#combine into one dataframe

combined_df <- left_join(phenology, fitness, by=c('Flowering_week', 'Site','Species', "Year",'Plot'))


#merging rows to combine fruit counts 
combined_df <- combined_df %>%
  group_by(Flowering_week, Year, Site, Species, Plot, Number_flowering_units) %>%
  summarise(across(c(Number_units_w_fruit), sum))

#add column with proportion of flowering units producing fruit
combined_df <- combined_df %>%
  mutate(Proportion_fruiting = Number_units_w_fruit / Number_flowering_units)

#relpace NAs with 0
combined_df[is.na(combined_df)] <- 0


#scatter plots for flowers and proportion fruiting
ggplot(combined_df, aes(x=Number_flowering_units, y=Proportion_fruiting)) + 
  geom_point() + 
  facet_wrap(~Species, scales = "free_x") + 
  ylim(0,1)+
  geom_smooth(method=lm)+
  stat_cor(aes(label = after_stat(rr.label)), color = "red", geom = "label")


