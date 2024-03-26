library(tidyverse)


#bring in data to combine
phenology <- read.csv("phenology.csv")
fitness <- read.csv("fitness.csv")

#switch from date to year
fitness <- fitness %>% mutate(Year = ifelse(substr(Date, nchar(Date) - 1, nchar(Date)) == "21", 2021, 2022)) %>% dplyr::select(Site, Species, Number_units_w_fruit, Flowering_week, Year)

fitness$Year <- as.integer(fitness$Year)
fitness$Flowering_week <- as.integer(fitness$Flowering_week)

#remove unnecessary columns 
fitness = subset(fitness, select = -c(1,6,7,8,9,10) )
phenology = subset(phenology, select = -c(2,7))

#remove underscores from species names in fitness df 
fitness$Species<-gsub("_", " ", fitness$Species)

phenology$Species<-gsub("Viola nuttalli", "Viola nuttallii", phenology$Species)


#combine into one dataframe

combined_df <- left_join(phenology, fitness, by=c('Flowering_week', 'Site','Species', "Year",'Plot'))

combined_df$Species<-gsub("Senecio interrigrimus", "Senecio integerrimus",combined_df$Species)
combined_df$Species<-gsub("Lupinus spp", "Lupinus bakeri", combined_df$Species)

#merging rows to combine fruit counts 
combined_df <- combined_df %>%
  group_by(Flowering_week, Year, Site, Species, Plot, Number_flowering_units) %>%
  summarise(across(c(Number_units_w_fruit), sum))


