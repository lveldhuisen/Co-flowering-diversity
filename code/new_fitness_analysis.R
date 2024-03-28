library(tidyverse)
library(dplyr)
library(ggpubr)


#bring in data to combine
phenology <- read.csv("phenology.csv")
phenology_wmodules <- read.csv("combined_raw_phenology.csv")
fitness <- read.csv("fitness.csv")


unique(phenology$Species)

#cleaning up "combined_raw_phenology.csv"
phenology_wmodules = subset(phenology_wmodules, select = -c(2))
names(phenology_wmodules)[2] <- "Flowering_week"
names(phenology_wmodules)[3] <- "Number_flowering_units"

unique(phenology_wmodules$Species)

#combined both phenology datasets
combined_phenology <- left_join(phenology, phenology_wmodules, by=c('Flowering_week', 'Site','Species', "Year","Number_flowering_units"))

#export to excel to fix few module errors by hand 
write_csv(combined_phenology, file = "combined_phenology.csv")
#bring back in with all module errors fixed
combined_phenology <- read.csv("combined_phenology.csv")

#clean up "fitness.csv"
fitness = subset(fitness, select = -c(1,6,7,8,9,10) )
fitness$Year <- as.integer(fitness$Year)
fitness$Flowering_week <- as.integer(fitness$Flowering_week)
#remove underscores from species names in fitness df 
fitness$Species<-gsub("_", " ", fitness$Species)

#combine into one dataframe

combined_df <- left_join(combined_phenology, fitness, by=c('Flowering_week', 'Site','Species', "Year",'Plot'))


#merging rows to combine fruit counts 
combined_df <- combined_df %>%
  group_by(Flowering_week, Year, Site, Species, Plot, Number_flowering_units) %>%
  summarise(across(c(Number_units_w_fruit), sum))

#add column with proportion of flowering units producing fruit
combined_df <- combined_df %>%
  mutate(Proportion_fruiting = Number_units_w_fruit / Number_flowering_units)

#relpace NAs with 0
combined_df[is.na(combined_df)] <- 0

#split into separate dataframes for 2021 and 2022
alldata_2021 <- subset(combined_df, Year == '2021')
PBM2021 <- subset(alldata_2021, Site == 'PBM')
Pfeiler2021 <- subset(alldata_2021, Site == 'Pfeiler')
Road2021 <- subset(alldata_2021, Site == 'Road')


alldata_2022 <- subset(combined_df, Year == '2022')


#scatter plots for flowers and proportion fruiting
regressions2021 <- ggplot(alldata_2021, aes(x=Number_flowering_units, y=Proportion_fruiting)) + 
  geom_point() + 
  facet_wrap(~Species, scales = "free_x") + 
  ylim(0,1)+
  geom_smooth(method=lm)+
  stat_cor(aes(label = after_stat(rr.label)), color = "red", geom = "label")

plot(regressions2021)

pbm2021fig <- ggplot(PBM2021, aes(x=Number_flowering_units, y=Proportion_fruiting)) + 
  geom_point() + 
  facet_wrap(~Species, scales = "free_x") + 
  ylim(0,1)+
  geom_smooth(method=lm)+
  stat_cor(aes(label = after_stat(rr.label)), color = "red", geom = "label")

plot(pbm2021fig)

ggscatter(
  alldata_2022, x = "Number_flowering_units", y = "Proportion_fruiting",
  add = "reg.line"
) +
  facet_wrap(~Species) +
  stat_cor(label.y = 4.4) +
  stat_regline_equation(label.y = 4.2)



regressions2022 <- ggplot(alldata_2022, aes(x=Number_flowering_units, y=Proportion_fruiting)) + 
  geom_point() + 
  facet_wrap(~Species, scales = "free_x") + 
  ylim(0,1)+
  geom_smooth(method=lm)+
  stat_cor(aes(label = after_stat(rr.label)), color = "red", geom = "label")

plot(regressions2022)
