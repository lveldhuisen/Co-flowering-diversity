#Contains: code for all figures. All files for this code are in "files_Figures" folder.
library(ggplot2)
library(dplyr)
library(ggpubr)
library(viridis)
library(pals)
library(Polychrome)
library(patchwork)
library(forcats)
library(ggridges)
library(hrbrthemes)


#Main text figures----------------
#data frames for figures 
df_site <- read.csv("files_Figures/results_bysite.csv")
df_mod <- read.csv("files_Figures/results_bymodule.csv")

#Figure: histogram of families by module----------------------------

#this data file is in the "files_Figures" folder 
df_pheno <- read.csv("files_Figures/combined_raw_phenology.csv")

#change site names in data
df_pheno["Site"][df_pheno["Site"] == "Road"] <- "Low elevation (2815 m)" 
df_pheno["Site"][df_pheno["Site"] == "Pfeiler"] <- "Middle elevation (3165 m)" 
df_pheno["Site"][df_pheno["Site"] == "PBM"] <- "High elevation (3380 m)" 
df_pheno["Family"][df_pheno["Family"] == "Fabaceae\n"] <- "Fabaceae"


##make individual histograms by site######
#combined each individual histogram to make Figure 1 in Adobe Illustrator

###road 2021#######

sub_pheno_r21 <- subset(df_pheno, 
                       Site %in% c("Low elevation (2815 m)") & Year == "2021") #subset data for site and year

manualcolors<-c('cornflowerblue','grey','tomato3','seagreen1',
                '#7CE3D8','plum3','lightsalmon',
                'darkgray','cadetblue1','#DDAD4B','seagreen','mediumorchid4','orange') #manually set colors for families 

Low2021_fig <- ggplot(data = sub_pheno_r21, 
                      aes(x=factor(Module, levels = c('Beginning','Middle','End')),y=Number_flowering, fill=Family)) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(legend.position = "none") + 
  scale_fill_manual(values=manualcolors) + 
  ylab("Number of flowering units") + 
  xlab("Flowering module") + 
  theme_light(base_size = 24)+ 
  ylim(0,1050) #plot 
plot(Low2021_fig)

###pfeiler 2021#######
sub_pheno_pf21 <- subset(df_pheno, 
                        Site %in% c("Middle elevation (3165 m)") & Year == "2021") 
#subset data for site and year

manualcolors<-c('mediumvioletred','cornflowerblue', 'black','wheat4','seagreen1',
                'darkolivegreen1','pink3',
                'yellowgreen','cadetblue1','mediumorchid4','orange','yellow') 
#manually set colors for families 

Middle2021_fig <- ggplot(data = sub_pheno_pf21, 
                         aes(x=factor(Module, levels = c('Beginning','Middle','End')),
                             y=Number_flowering, fill=Family)) + 
  geom_bar(stat="identity", show.legend = F) + 
  scale_fill_manual(values=manualcolors) + 
  ylab("Number of flowering units") + 
  xlab("Flowering module") + 
  theme_light(base_size = 24) + 
  ylim(0,1050)

plot(Middle2021_fig)

###PBM 2021#######
sub_pheno_pbm21 <- subset(df_pheno, 
                         Site %in% c("High elevation (3380 m)") & Year == "2021") 
#subset data for site and year

manualcolors<-c('cornflowerblue', 'black','wheat4',
                'moccasin','#7CE3D8','Indianred1','plum3','pink3',
                'darkblue','#DDAD4B','mediumorchid4','orange','yellow','purple')
#manually set colors for families 

high2021_fig <- ggplot(data = sub_pheno_pbm21, 
                       aes(x=factor(Module, levels = c('Beginning','Middle','End')),
                           y=Number_flowering, fill=Family)) + 
  geom_bar(stat="identity", show.legend = F) +
  theme(legend.position = "right") + 
  scale_fill_manual(values=manualcolors) + 
  ylab("Number of flowering units") + 
  xlab("Flowering module") + 
  theme_light(base_size = 24) + 
  ylim(0,1050)

plot(high2021_fig)

###road 2022##########
sub_pheno_r22 <- subset(df_pheno, 
                        Site %in% c("Low elevation (2815 m)") & Year == "2022") 
#subset data for site and year

manualcolors<-c('cornflowerblue', 'black','wheat4','tomato3','seagreen1',
                '#7CE3D8','lightsalmon',
                'cadetblue1','seagreen','mediumorchid4','orange','yellow') 
#manually set colors for families 

low2022_fig <- ggplot(data = sub_pheno_r22, 
                      aes(x=factor(Module, levels = c('Beginning','Middle','Middle2','End')),
                          y=Number_flowering, fill=Family)) + 
  geom_bar(stat="identity", show.legend = F) +
  theme(legend.position = "right") + 
  scale_fill_manual(values=manualcolors) + 
  ylab("Number of flowering units") + 
  xlab("Flowering module") + 
  theme_light(base_size = 24) + 
  ylim(0,1050)

plot(low2022_fig)

###pfeiler 2022#######
sub_pheno_pf22 <- subset(df_pheno, 
                         Site %in% c("Middle elevation (3165 m)") & Year == "2022") 
#subset data for site and year

manualcolors<-c('mediumvioletred','cornflowerblue', 'black','wheat4','seagreen1',
                'purple','darkolivegreen1','Indianred1','pink3',
                'yellowgreen','mediumorchid4','orange','yellow','grey') 
#manually set colors for families 

middle2022_fig <- ggplot(data = sub_pheno_pf22, 
                         aes(x=factor(Module, levels = c('Beginning','Middle','End')),
                             y=Number_flowering, fill=Family)) + 
  geom_bar(stat="identity", show.legend = F) +
  theme(legend.position = "right") + 
  scale_fill_manual(values=manualcolors) + 
  ylab("Number of flowering units") + 
  xlab("Flowering module") + 
  theme_light(base_size = 24) +
  ylim(0,1050)

plot(middle2022_fig)

###PBM 2022#######
sub_pheno_pbm22 <- subset(df_pheno, 
                          Site %in% c("High elevation (3380 m)") & Year == "2022") 
#subset data for site and year

manualcolors<-c('mediumvioletred','cornflowerblue', 'black','wheat4',
                '#7CE3D8','darkolivegreen1','darkblue','#DDAD4B','seagreen',
                'mediumorchid4','orange','yellow','grey') #manually set colors for families 

high2022_fig <- ggplot(data = sub_pheno_pbm22, 
                       aes(x=factor(Module, levels = c('Beginning','Middle','End')),
                           y=Number_flowering, fill=Family)) + 
  geom_bar(stat="identity", show.legend = F) +
  theme(legend.position = "right") + 
  scale_fill_manual(values=manualcolors) + 
  ylab("Number of flowering units") + 
  xlab("Flowering module") + 
  theme_light(base_size = 24) + 
  ylim(0,1050)

plot(high2022_fig)

###combine each individual plot into one to avoid middle2 blank columns####
###this is the figure used in manuscript revision May 2024#########
fig1_2021 <- Low2021_fig + Middle2021_fig + high2021_fig + 
  plot_layout(axes = "collect", axis_titles = "collect")
plot(fig1_2021)

fig1_2022 <- low2022_fig + middle2022_fig + high2022_fig + 
  plot_layout(axes = "collect", axis_titles = "collect")
plot(fig1_2022)

fig1 <- fig1_2021 / fig1_2022 + 
  plot_layout(axis_titles = "collect", axes = "collect")
plot(fig1)

###all sites and years together#####
###this fig is the same as above, but has blanks spots for middle2 module####
manualcolors_test<-c('mediumvioletred','cornflowerblue', 'black','wheat4',
                     '#7CE3D8','darkolivegreen1','darkblue','#DDAD4B','seagreen',
                     'mediumorchid4','darksalmon','yellow','grey','moccasin',
                     'yellow1','purple','brown','turquoise','turquoise4','brown1',
                     'deeppink','darkgoldenrod1','darkolivegreen3') #manually set colors for families

ggplot(data = df_pheno, aes(x=factor(Module, levels = c('Beginning','Middle','Middle2','End')),
                            y=Number_flowering, fill=Family)) + 
  geom_bar(stat="identity") +
  theme(legend.position = "right") + 
  scale_fill_manual(values=manualcolors_test) + 
  ylab("Number of flowering units") + 
  xlab("Flowering module") + 
  theme_light(base_size = 24) + 
  ylim(0,1050)+
  facet_grid(Year ~ factor(Site, levels = c("Low elevation (2815 m)","Middle elevation (3165 m)","High elevation (3380 m)")))

#Figure: phenology and fitness correlations (not used in revision)--------------
#we generated a figure for each site and year, and combined them to make fig in Adobe Illustrator 

#bring in data, this file is in the "files_Figures" folder
df_all <- read.csv("files_Figures/results_ALL.csv")

###road 2021#######
subDataR21 <- subset(df_all, 
                     Site %in% c("Road") & Year == "2021") #subset data by site and year 

R21_reg <- lm(SI_fitness ~ SI, data = subDataR21) #linear regression
summary(R21_reg)  
plot(R21_reg$residuals) #check residuals 

ggplot(subDataR21, aes(x=SI, y=SI_fitness)) + 
  geom_point(shape=20, size=5) +
  theme_light()+ geom_smooth(method = "lm") + 
  ggtitle("road 2021") #plot regression 

ggplot(subDataR21, aes(x=SI, y=SI_fitness)) + 
  geom_jitter(shape=20, size=5, width = 0.015, height = 0.015) + 
  theme_light()+ 
  geom_smooth(method = "lm") + 
  ggtitle("road 2021") 

###pfeiler 2021#######
subDataPf21 <- subset(df_all, 
                      Site %in% c("Pfeiler") & Year == "2021") #subset data by site and year 

Pf21_reg <- lm(SI_fitness ~ SI, data = subDataPf21) #linear regression
summary(Pf21_reg)
plot(Pf21_reg$residuals)

ggplot(subDataPf21, aes(x=SI, y=SI_fitness)) + 
  geom_jitter(shape=20,size=5, width = 0.015, height = 0.015) + 
  theme_light()+ geom_smooth(method = "lm") + 
  ggtitle("pfeiler 2021") #plot regression 

###PBM 2021#######
subDataPBM21 <- subset(df_all, 
                       Site %in% c("PBM") & Year == "2021") #subset data by site and year 

PBM21_reg <- lm(SI_fitness ~ SI, data = subDataPBM21) #linear regression
summary(PBM21_reg)
plot(PBM21_reg$residuals)

ggplot(subDataPBM21, aes(x=SI, y=SI_fitness)) + 
  geom_jitter(shape=20, size=5, width = 0.015, height = 0.015) + 
  theme_light()+ 
  geom_smooth(method = "lm") + 
  ggtitle("pbm 2021") #plot regression 

###road 2022#######
subDataR22 <- subset(df_all, 
                     Site %in% c("Road") & Year == "2022") #subset data by site and year 

R22_reg <- lm(SI_fitness ~ SI, data = subDataR22) #linear regression
summary(R22_reg)
plot(R22_reg$residuals)

ggplot(subDataR22, aes(x=SI, y=SI_fitness)) + 
  geom_jitter(shape=20, size=5, width = 0.015, height = 0.015) + 
  theme_light()+ 
  geom_smooth(method = "lm") + 
  ggtitle("road 2022") #plot regression 

###pfeiler 2022#######
subDataPf22 <- subset(df_all, 
                      Site %in% c("Pfeiler") & Year == "2022") #subset data by site and year 

Pf22_reg <- lm(SI_fitness ~ SI, data = subDataPf22) #linear regression
summary(Pf22_reg)
plot(Pf22_reg$residuals)

ggplot(subDataPf22, aes(x=SI, y=SI_fitness)) + 
  geom_jitter(shape=20,size=5, width = 0.015, height = 0.015) + 
  theme_light()+ 
  geom_smooth(method = "lm") + 
  ggtitle("pfeiler 2022") #plot regression 

###PBM 2022#######
subDataPBM22 <- subset(df_all, 
                       Site %in% c("PBM") & Year == "2022") #subset data by site and year 

PBM22_reg <- lm(SI_fitness ~ SI, data = subDataPBM22) #linear regression
summary(PBM22_reg)
plot(PBM22_reg$residuals)

ggplot(subDataPBM22, aes(x=SI, y=SI_fitness)) + 
  geom_jitter(shape=20,size=5, width = 0.015, height = 0.015) + 
  theme_light()+ 
  geom_smooth(method = "lm") + 
  ggtitle("pbm 2022") #plot regression 

###test big faceted graph with regression lines
df_all %>%
  mutate(Site = factor(Site, labels = c("PBM", "Pfeiler", "Road"))) %>%
  ggplot(aes(x = SI, y = SI_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(Year ~ Site) +
  theme_light() +
  stat_cor(aes(label = after_stat(rr.label)), color = "red", geom = "label") #same data but different layout from final Fig 2

#new Figure for May 2024 revisions: distributions of all species flowering color coded by module-----------
#bring in data
data_days <- read.csv("files_Figures/combined_raw_phenology_days_updates.csv")

#create object to rename sites with elevations
site_names <- c(
  `Road` = "Low (2815 m)",
  `Pfeiler` = "Middle (3165 m)",
  `PBM` = "High (3380 m)"
)

#reorder site names
data_days$Site <- factor(data_days$Site, levels = c("Road","Pfeiler","PBM"))
data_days$Year <- as.factor(data_days$Year)

#rename modules
data_days$Module <- gsub("Beginning","Early", data_days$Module)
data_days$Module <- gsub("Middle","Mid", data_days$Module)
data_days$Module <- gsub("Middle2","Mid2", data_days$Module)
data_days$Module <- gsub("End","Late", data_days$Module)

#reorder modules
data_days$Module <- factor(data_days$Module, levels = c("Early", "Mid", "Mid2", "Late"))

#plot
new_fig<- ggplot(data_days, aes(x = Day, y = reorder(Species, Week, decreasing = T),
                                Group = Species,linetype = Year, color = factor(Module))) +
  geom_density_ridges(scale = 1, show.legend = TRUE, alpha = 0.2, rel_min_height = 0.005) +
  scale_y_discrete(expand = c(0, 0), name = "Species") +
  facet_wrap(~Site, labeller = as_labeller(site_names)) +
  theme_bw(base_size = 18) +
  theme(axis.text.y = element_text(face = "italic"))+
  scale_fill_manual( values = c("grey"))+
  scale_colour_manual(values = c("magenta4", "orange", "turquoise2","forestgreen"), name ="Module")+
  scale_x_continuous(limits = c(0,75),breaks = c(0,35,70),labels = c("1", "5", "10"), name = 'Week')

plot(new_fig)

#Figure: networks, generated all network figures individually in Gephi, 
#and combined them using Adobe Illustrator------------------------------

#Figure: phylogenetic diversity by week (new for revision May 2024)------------------------
##make figure with both years combined#####

#bring in data 
all_weeks_pd <- read.csv("files_Figures/results_byweek.csv")
all_weeks_pd$Week <- as.factor(all_weeks_pd$Week)

#rename pd metrics for no acronyms

all_weeks_pd$Type <- gsub("MPD","Mean phylogenetic distance", all_weeks_pd$Type)
all_weeks_pd$Type <- gsub("MNTD","Mean nearest taxon distance", all_weeks_pd$Type)
all_weeks_pd$Type <- gsub("PD","Faith's phylogenetic diversity", all_weeks_pd$Type)

#make figure
fig_pd_weeks <- ggplot(all_weeks_pd, 
                       aes(fill = Type, y=SES, 
                           x=fct_relevel(Week, c("1","2","3","4","5","6","7","8","9","10")))) + 
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

###line plot for diversity by weeks#######
ggplot(all_weeks_pd,aes(fill = Type, y=SES,group=Type, color = Type,
           x=fct_relevel(Week, c("1","2","3","4","5","6","7","8","9","10")))) +
  geom_point()+
  geom_line(size=1.5)+
  scale_color_manual(values=c("Mean nearest taxon distance"="#c385b3",
                             "Mean phylogenetic distance"="#cdd870",
                             "Faith's phylogenetic diversity"="#4ea6c4"))+
  xlab("Week") + 
  ylab("Standard effect size")+
  theme_light(base_size = 20) + 
  guides(fill = "none",color=guide_legend(title="Phylogenetic metric"))+
  ylim(-5.9,2.3) +
  facet_grid(Year ~factor(Site, levels = c("Low elevation (2815 m)","Middle elevation (3165 m)","High elevation (3380 m)")))

#Figure: phylogenetic SES values by modules--------------------------------------
#like other figures, we generated figures for each site and year and combined them to make the final figure 4 in Adobe Illustrator 

#find this file in "files_Figures" folder 
df_mod_figs <- read.csv("files_Figures/results_modules_combined.csv")

##road 2021#######
sub_mod_r21 <- subset(df_mod_figs, 
                        Site %in% c("Road") & Year == "2021") #subset data by site and year 

road21_mod_fig <- ggplot(sub_mod_r21, aes(fill=Type, y=SES, x=fct_relevel(Module, c("beginning","middle","end")))) + 
         geom_bar(position = "dodge",stat = "identity", show.legend = F) +
  xlab("Module") + 
  theme_light() + 
  guides(fill=guide_legend(title="Phylogenetic metric"))+
  scale_fill_manual(values=c("#c385b3",
                             "#cdd870",
                             "#4ea6c4")) +
  ylim(-5,2) #plot figure 
plot(road21_mod_fig)

##pfeiler 2021#######
sub_mod_pf21 <- subset(df_mod_figs, 
                      Site %in% c("Pfeiler") & Year == "2021") #subset data by site and year 

pfeiler21_SES <- ggplot(sub_mod_pf21, aes(fill=Type, y=SES, x=fct_relevel(Module, c("beginning","middle","end")))) + 
  geom_bar(position = "dodge",stat = "identity", show.legend = F) +
  xlab("Module") + 
  theme_light() + 
  guides(fill=guide_legend(title="Phylogenetic metric"))+
  scale_fill_manual(values=c("#c385b3",
                             "#cdd870",
                             "#4ea6c4")) +
  ylim(-5,2) #plot figure

plot(pfeiler21_SES)

##PBM 2021#######
sub_mod_pbm21 <- subset(df_mod_figs, 
                       Site %in% c("PBM") & Year == "2021") #subset data by site and year 

PBM2021_SES <- ggplot(sub_mod_pbm21, aes(fill=Type, y=SES, x=fct_relevel(Module, c("beginning","middle","end")))) + 
  geom_bar(position = "dodge",stat = "identity", show.legend = F) +
  xlab("Module") + 
  theme_light() + 
  guides(fill=guide_legend(title="Phylogenetic metric"))+
  scale_fill_manual(values=c("#c385b3",
                             "#cdd870",
                             "#4ea6c4")) +
  ylim(-5,2.2) #plot figure

plot(PBM2021_SES)

##road 2022#######
sub_mod_r22 <- subset(df_mod_figs, 
                      Site %in% c("Road") & Year == "2022") #subset data by site and year 

road2022_SES <- ggplot(sub_mod_r22, aes(fill=Type, y=SES, x=fct_relevel(Module, c("beginning","middle",
                                                                  "middle2", "end")))) + 
  geom_bar(position = "dodge",stat = "identity", show.legend = F) +
  xlab("Module") + 
  theme_light() + 
  guides(fill=guide_legend(title="Phylogenetic metric"))+
  scale_fill_manual(values=c("#c385b3",
                             "#cdd870",
                             "#4ea6c4")) +
  ylim(-5,2) #plot figure
plot(road2022_SES)

##pfeiler 2022#######
sub_mod_pf22 <- subset(df_mod_figs, 
                       Site %in% c("Pfeiler") & Year == "2022") #subset data by site and year 

pfeiler2022 <- ggplot(sub_mod_pf22, aes(fill=Type, y=SES, x=fct_relevel(Module, c("beginning","middle","end")))) + 
  geom_bar(position = "dodge",stat = "identity", show.legend = F) +
  xlab("Module") + 
  theme_light() + 
  guides(fill=guide_legend(title="Phylogenetic metric"))+
  scale_fill_manual(values=c("#c385b3",
                             "#cdd870",
                             "#4ea6c4")) +
  ylim(-5,2) #plot figure

plot(pfeiler2022)

##PBM 2022#######
sub_mod_pbm22 <- subset(df_mod_figs, 
                        Site %in% c("PBM") & Year == "2022") #subset data by site and year 

PBM2022_SES <- ggplot(sub_mod_pbm22, aes(fill=Type, y=SES, x=fct_relevel(Module, c("beginning","middle","end")))) + 
  geom_bar(position = "dodge",stat = "identity", show.legend = F) +
  xlab("Module") + 
  theme_light() + 
  guides(fill=guide_legend(title="Phylogenetic metric"))+
  scale_fill_manual(values=c("#c385b3",
                             "#cdd870",
                             "#4ea6c4")) +
  ylim(-5,2) #plot figure

plot(PBM2022_SES)

##combine individual figures with patchwork######
mod_2021 <- road21_mod_fig + pfeiler21_SES + PBM2021_SES + plot_layout(axes = "collect", axis_titles = "collect")
plot(mod_2021)

mod_2022 <- road2022_SES + pfeiler2022 + PBM2022_SES + plot_layout(axes = "collect", axis_titles = "collect")
plot(mod_2022)

pd_mod_all <- mod_2021 / mod_2022 +  plot_layout(axes = "collect", axis_titles = "collect")
plot(pd_mod_all)

##all sites and years together in one faceted figured#####
#the code to generate the datasets is in the file titled "S&B_phylogenetics_bymodule.R"

#make figure with both years combined-------------
fig_pd_mod <- ggplot(all_mod, aes(fill = Type, y=SES, x=fct_relevel(Module, c("Beginning","Middle","Middle2","End")))) + 
  geom_bar(position = "dodge",stat = "identity") +
  xlab("Module") + 
  ylab("Standard effect size")+
  theme_light(base_size = 20) + 
  guides(fill=guide_legend(title="Phylogenetic metric"))+
  scale_fill_manual(values=c("#c385b3",
                             "#cdd870",
                             "#4ea6c4"))+
  ylim(-5.2,2.2) +
  facet_grid(Year ~factor(Site, levels = c("Low elevation (2815 m)","Middle elevation (3165 m)","High elevation (3380 m)")))

plot(fig_pd_mod)



#species counts for "Flowering" section in Results--------------------
species_all <- length(unique(df_all$species1))
df_all %>% filter(Site == "PBM") %>% summarise(n = n_distinct(species1)) 
median(df_all$SI)
hist(df_all$SI)

quantile(df_all$SI)

length(df_all$SI)

sum(df_pheno[which(df_pheno$Site=='PBM'), 4])

#Supplemental figures-------------
setwd("/Users/leahvedlhuisen/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/Chapter 1/AllData_AmNat/files_SchoenersIndex")

SI_df <- read.csv("Schoeners_all.csv")

ggplot(SI_df, aes(x=SI)) + 
  geom_histogram(binwidth=0.1)+
  theme_bw(base_size = 18)+
  xlab("Schoener's index") +
  ylab("Number of species pairs")

SI_df %>% count(Site, Year)
