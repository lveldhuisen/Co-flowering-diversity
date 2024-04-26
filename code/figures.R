#Contains: code for all figures. All files for this code are in "files_Figures" folder.
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggpubr")
install.packages("viridis")  
install.packages("pals")
install.packages("Polychrome")

library(ggplot2)
library(dplyr)
library(ggpubr)
library(viridis)
library(pals)
library(Polychrome)

#dataframes for figures 
df_site <- read.csv("results_bysite.csv")
df_mod <- read.csv("results_bymodule.csv")

#Figure 1: histogram of families by module----------------------------

#this data file is in the "files_Figures" folder 
df_pheno <- read.csv("combined_raw_phenology.csv")

#change site names in data
df_pheno["Site"][df_pheno["Site"] == "Road"] <- "Low elevation (2815 m)" 
df_pheno["Site"][df_pheno["Site"] == "Pfeiler"] <- "Middle elevation (3165 m)" 
df_pheno["Site"][df_pheno["Site"] == "PBM"] <- "High elevation (3380 m)" 
df_pheno["Family"][df_pheno["Family"] == "Fabaceae\n"] <- "Fabaceae"


##make individual histograms by site -------------------------------------
#I combined each individual histogram to make Figure 1 in Adobe Illustrator

###road 2021#######

sub_pheno_r21 <- subset(df_pheno, 
                       Site %in% c("Road") & Year == "2021") #subset data for site and year

manualcolors<-c('cornflowerblue','grey','tomato3','seagreen1',
                '#7CE3D8','plum3','lightsalmon',
                'darkgray','cadetblue1','#DDAD4B','seagreen','mediumorchid4','orange') #manually set colors for families 

ggplot(data = sub_pheno_r21, aes(x=factor(Module, levels = c('Beginning','Middle','End')),y=Number_flowering, fill=Family)) + 
  geom_bar(stat="identity") +
  theme(legend.position = "right") + scale_fill_manual(values=manualcolors) + 
  ylab("Number of flowering units") + xlab("Flowering module") + theme_light() +ggtitle("Road 2021") + ylim(0,1050) #plot 

###pfeiler 2021#######
sub_pheno_pf21 <- subset(df_pheno, 
                        Site %in% c("Pfeiler") & Year == "2021") #subset data for site and year

manualcolors<-c('mediumvioletred','cornflowerblue', 'black','wheat4','seagreen1',
                'darkolivegreen1','pink3',
                'yellowgreen','cadetblue1','mediumorchid4','orange','yellow') #manually set colors for families 

ggplot(data = sub_pheno_pf21, aes(x=factor(Module, levels = c('Beginning','Middle','End')),y=Number_flowering, fill=Family)) + 
  geom_bar(stat="identity") +
  theme(legend.position = "right") + scale_fill_manual(values=manualcolors) + 
  ylab("Number of flowering units") + xlab("Flowering module") + theme_light() + ggtitle("Pfeiler 2021") + ylim(0,1050)

###PBM 2021#######
sub_pheno_pbm21 <- subset(df_pheno, 
                         Site %in% c("PBM") & Year == "2021") #subset data for site and year

manualcolors<-c('cornflowerblue', 'black','wheat4',
                'moccasin','#7CE3D8','Indianred1','plum3','pink3',
                'darkblue','#DDAD4B','mediumorchid4','orange','yellow','purple') #manually set colors for families 

ggplot(data = sub_pheno_pbm21, aes(x=factor(Module, levels = c('Beginning','Middle','End')),y=Number_flowering, fill=Family)) + 
  geom_bar(stat="identity") +
  theme(legend.position = "right") + scale_fill_manual(values=manualcolors) + 
  ylab("Number of flowering units") + xlab("Flowering module") + theme_light() + ggtitle("PBM 2021") +ylim(0,1050)

###road 2022##########
sub_pheno_r22 <- subset(df_pheno, 
                        Site %in% c("Road") & Year == "2022") #subset data for site and year

manualcolors<-c('cornflowerblue', 'black','wheat4','tomato3','seagreen1',
                '#7CE3D8','lightsalmon',
                'cadetblue1','seagreen','mediumorchid4','orange','yellow') #manually set colors for families 

ggplot(data = sub_pheno_r22, aes(x=factor(Module, levels = c('Beginning','Middle','Middle2','End')),y=Number_flowering, fill=Family)) + 
  geom_bar(stat="identity") +
  theme(legend.position = "right") + scale_fill_manual(values=manualcolors) + 
  ylab("Number of flowering units") + xlab("Flowering module") + theme_light() + ggtitle("Road 2022") + ylim(0,1050)

###pfeiler 2022#######
sub_pheno_pf22 <- subset(df_pheno, 
                         Site %in% c("Pfeiler") & Year == "2022") #subset data for site and year

manualcolors<-c('mediumvioletred','cornflowerblue', 'black','wheat4','seagreen1','purple','darkolivegreen1','Indianred1','pink3',
                'yellowgreen','mediumorchid4','orange','yellow','grey') #manually set colors for families 

ggplot(data = sub_pheno_pf22, aes(x=factor(Module, levels = c('Beginning','Middle','End')),y=Number_flowering, fill=Family)) + 
  geom_bar(stat="identity") +
  theme(legend.position = "right") + scale_fill_manual(values=manualcolors) + 
  ylab("Number of flowering units") + xlab("Flowering module") + theme_light() + ggtitle("Pfeiler 2022")+ylim(0,1050)

###PBM 2022#######
sub_pheno_pbm22 <- subset(df_pheno, 
                          Site %in% c("PBM") & Year == "2022") #subset data for site and year

manualcolors<-c('mediumvioletred','cornflowerblue', 'black','wheat4','#7CE3D8','darkolivegreen1','darkblue','#DDAD4B','seagreen','mediumorchid4','orange','yellow','grey') #manually set colors for families 

ggplot(data = sub_pheno_pbm22, aes(x=factor(Module, levels = c('Beginning','Middle','End')),y=Number_flowering, fill=Family)) + 
  geom_bar(stat="identity") +
  theme(legend.position = "right") + scale_fill_manual(values=manualcolors) + 
  ylab("Number of flowering units") + xlab("Flowering module") + theme_light() + ggtitle("PBM 2022") + ylim(0,1050)

###do all sites and years together#####
manualcolors_test<-c('mediumvioletred','cornflowerblue', 'black','wheat4','#7CE3D8','darkolivegreen1','darkblue','#DDAD4B','seagreen','mediumorchid4','darksalmon','yellow','grey','moccasin','yellow1','purple','brown','turquoise','turquoise4','brown1','deeppink','darkgoldenrod1','darkolivegreen3') #manually set colors for families

ggplot(data = df_pheno, aes(x=factor(Module, levels = c('Beginning','Middle','Middle2','End')),y=Number_flowering, fill=Family)) + 
  geom_bar(stat="identity") +
  theme(legend.position = "right") + 
  scale_fill_manual(values=manualcolors_test) + 
  ylab("Number of flowering units") + 
  xlab("Flowering module") + 
  theme_light(base_size = 20) + 
  ylim(0,1050)+
  facet_grid(Year ~ factor(Site, levels = c("Low elevation (2815 m)","Middle elevation (3165 m)","High elevation (3380 m)")))

#Figure 2: phenology and fitness correlations-------------------------------------------
#we generated a figure for each site and year, and combined them to make Fig 2 in Adobe Illustrator 

#bring in data, this file is in the "files_Figures" folder
df_all <- read_csv("results_ALL.csv")

###road 2021#######
subDataR21 <- subset(df_all, 
                     Site %in% c("Road") & Year == "2021") #subset data by site and year 

R21_reg <- lm(SI_fitness ~ SI, data = subDataR21) #linear regression
summary(R21_reg)  
plot(R21_reg$residuals) #check residuals 

ggplot(subDataR21, aes(x=SI, y=SI_fitness)) + geom_point(shape=20, size=5) + theme_light()+ geom_smooth(method = "lm") + ggtitle("road 2021") #plot regression 

ggplot(subDataR21, aes(x=SI, y=SI_fitness)) + geom_jitter(shape=20, size=5, width = 0.015, height = 0.015) + theme_light()+ geom_smooth(method = "lm") + ggtitle("road 2021") 

###pfeiler 2021#######
subDataPf21 <- subset(df_all, 
                      Site %in% c("Pfeiler") & Year == "2021") #subset data by site and year 

Pf21_reg <- lm(SI_fitness ~ SI, data = subDataPf21) #linear regression
summary(Pf21_reg)
plot(Pf21_reg$residuals)

ggplot(subDataPf21, aes(x=SI, y=SI_fitness)) + geom_jitter(shape=20,size=5, width = 0.015, height = 0.015) + theme_light()+ geom_smooth(method = "lm") + ggtitle("pfeiler 2021") #plot regression 

###PBM 2021#######
subDataPBM21 <- subset(df_all, 
                       Site %in% c("PBM") & Year == "2021") #subset data by site and year 

PBM21_reg <- lm(SI_fitness ~ SI, data = subDataPBM21) #linear regression
summary(PBM21_reg)
plot(PBM21_reg$residuals)

ggplot(subDataPBM21, aes(x=SI, y=SI_fitness)) + geom_jitter(shape=20, size=5, width = 0.015, height = 0.015) + theme_light()+ geom_smooth(method = "lm") + ggtitle("pbm 2021") #plot regression 

###road 2022#######
subDataR22 <- subset(df_all, 
                     Site %in% c("Road") & Year == "2022") #subset data by site and year 

R22_reg <- lm(SI_fitness ~ SI, data = subDataR22) #linear regression
summary(R22_reg)
plot(R22_reg$residuals)

ggplot(subDataR22, aes(x=SI, y=SI_fitness)) + geom_jitter(shape=20, size=5, width = 0.015, height = 0.015) + theme_light()+ geom_smooth(method = "lm") + ggtitle("road 2022") #plot regression 

###pfeiler 2022#######
subDataPf22 <- subset(df_all, 
                      Site %in% c("Pfeiler") & Year == "2022") #subset data by site and year 

Pf22_reg <- lm(SI_fitness ~ SI, data = subDataPf22) #linear regression
summary(Pf22_reg)
plot(Pf22_reg$residuals)

ggplot(subDataPf22, aes(x=SI, y=SI_fitness)) + geom_jitter(shape=20,size=5, width = 0.015, height = 0.015) + theme_light()+ geom_smooth(method = "lm") + ggtitle("pfeiler 2022") #plot regression 

###PBM 2022#######
subDataPBM22 <- subset(df_all, 
                       Site %in% c("PBM") & Year == "2022") #subset data by site and year 

PBM22_reg <- lm(SI_fitness ~ SI, data = subDataPBM22) #linear regression
summary(PBM22_reg)
plot(PBM22_reg$residuals)

ggplot(subDataPBM22, aes(x=SI, y=SI_fitness)) + geom_jitter(shape=20,size=5, width = 0.015, height = 0.015) + theme_light()+ geom_smooth(method = "lm") + ggtitle("pbm 2022") #plot regression 

###test big faceted graph with regression lines########## 
df_all %>%
  mutate(Site = factor(Site, labels = c("PBM", "Pfeiler", "Road"))) %>%
  ggplot(aes(x = SI, y = SI_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(Year ~ Site) +
  theme_light() +
  stat_cor(aes(label = after_stat(rr.label)), color = "red", geom = "label") #same data but different layout from final Fig 2

#Figure 3: networks, generated all network figures individually in Gephi, and combined them using Adobe Illustrator------------------------------

#Figure 4: phylogenetic diversity by week (new for revision)------------------------
##make figure with both years combined#####
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

#Figure 5: phylogenetic SES values by modules--------------------------------------
#like other figures, I generated figures for each site and year and combined them to make the final figure 4 in Adobe Illustrator 

#find this file in "files_Figures" folder 
df_mod_figs <- read.csv("results_modules_combined.csv")

##road 2021#######
sub_mod_r21 <- subset(df_mod_figs, 
                        Site %in% c("Road") & Year == "2021") #subset data by site and year 

road21_SES <- ggplot(sub_mod_r21, aes(fill=Type, y=SES, x=fct_relevel(Module, c("beginning","middle","end")))) + 
         geom_bar(position = "dodge",stat = "identity") +
  xlab("Module") + 
  theme_light() + 
  guides(fill=guide_legend(title="Phylogenetic metric"))+
  scale_fill_manual(values=c("#c385b3",
                             "#cdd870",
                             "#4ea6c4")) + ggtitle("Road 2021") + ylim(-5,2) #plot figure 

print(road21_SES)
##pfeiler 2021#######
sub_mod_pf21 <- subset(df_mod_figs, 
                      Site %in% c("Pfeiler") & Year == "2021") #subset data by site and year 

pfeiler21_SES <- ggplot(sub_mod_pf21, aes(fill=Type, y=SES, x=fct_relevel(Module, c("beginning","middle","end")))) + 
  geom_bar(position = "dodge",stat = "identity") +
  xlab("Module") + 
  theme_light() + 
  guides(fill=guide_legend(title="Phylogenetic metric"))+
  scale_fill_manual(values=c("#c385b3",
                             "#cdd870",
                             "#4ea6c4")) + ggtitle("Pfeiler 2021")+ ylim(-5,2) #plot figure
##PBM 2021#######
sub_mod_pbm21 <- subset(df_mod_figs, 
                       Site %in% c("PBM") & Year == "2021") #subset data by site and year 

PBM2021_SES <- ggplot(sub_mod_pbm21, aes(fill=Type, y=SES, x=fct_relevel(Module, c("beginning","middle","end")))) + 
  geom_bar(position = "dodge",stat = "identity") +
  xlab("Module") + 
  theme_light() + 
  guides(fill=guide_legend(title="Phylogenetic metric"))+
  scale_fill_manual(values=c("#c385b3",
                             "#cdd870",
                             "#4ea6c4")) + ggtitle("PBM 2021")+ ylim(-5,2) #plot figure

print(PBM2021_SES)

p<-ggplot(data=sub_mod_pbm21, aes(x=Module, y=SES, fill=Type)) +
  geom_bar(stat="identity", position = "dodge")+
  theme_light()+
  scale_fill_manual(values=c("#c385b3",
                             "#cdd870",
                             "#4ea6c4")) + ggtitle("PBM 2021")+ ylim(-5,2) #plot figure
  
p
##road 2022#######
sub_mod_r22 <- subset(df_mod_figs, 
                      Site %in% c("Road") & Year == "2022") #subset data by site and year 

road2022_SES <- ggplot(sub_mod_r22, aes(fill=Type, y=SES, x=fct_relevel(Module, c("beginning","middle",
                                                                  "middle2", "end")))) + 
  geom_bar(position = "dodge",stat = "identity") +
  xlab("Module") + 
  theme_light() + 
  guides(fill=guide_legend(title="Phylogenetic metric"))+
  scale_fill_manual(values=c("#c385b3",
                             "#cdd870",
                             "#4ea6c4")) + ggtitle("Road 2022")+ ylim(-5,2) #plot figure
##pfeiler 2022#######
sub_mod_pf22 <- subset(df_mod_figs, 
                       Site %in% c("Pfeiler") & Year == "2022") #subset data by site and year 

pfeiler2022 <- ggplot(sub_mod_pf22, aes(fill=Type, y=SES, x=fct_relevel(Module, c("beginning","middle","end")))) + 
  geom_bar(position = "dodge",stat = "identity") +
  xlab("Module") + 
  theme_light() + 
  guides(fill=guide_legend(title="Phylogenetic metric"))+
  scale_fill_manual(values=c("#c385b3",
                             "#cdd870",
                             "#4ea6c4")) + ggtitle("Pfeiler 2022")+ ylim(-5,2) #plot figure
##PBM 2022#######
sub_mod_pbm22 <- subset(df_mod_figs, 
                        Site %in% c("PBM") & Year == "2022") #subset data by site and year 

PBM2022_SES <- ggplot(sub_mod_pbm22, aes(fill=Type, y=SES, x=fct_relevel(Module, c("beginning","middle","end")))) + 
  geom_bar(position = "dodge",stat = "identity") +
  xlab("Module") + 
  theme_light() + 
  guides(fill=guide_legend(title="Phylogenetic metric"))+
  scale_fill_manual(values=c("#c385b3",
                             "#cdd870",
                             "#4ea6c4")) + ggtitle("PBM 2022")+ ylim(-5,2) #plot figure

##all sites and years together in one faceted figured#####
#the code to generate the datasets is in the file titled "S&B_phylogenetics_bymodule.R"





#species counts for "Flowering" section in Results--------------------
species_all <- length(unique(df_all$species1))
df_all %>% filter(Site == "PBM") %>% summarise(n = n_distinct(species1)) 
median(df_all$SI)
hist(df_all$SI)

portion0.1<- nrow(df_all[df_all$SI < '0.1', ])/length(df_all$SI)
portionzero <- nrow(df_all[df_all$SI == '0', ])/length(df_all$SI)
portionhigh <- nrow(df_all[df_all$SI > '0.7', ])/length(df_all$SI)

nrow(df_all[df_all$SI_fitness < '0.1', ])/length(df_all$SI_fitness)
nrow(df_all[df_all$SI_fitness > '0.70', ])
length(df_all$SI_fitness)

sum(df_pheno[which(df_pheno$Site=='PBM'), 4])
