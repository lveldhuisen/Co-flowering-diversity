install.packages("ggridges")
install.packages("hrbrthemes")
library(ggridges)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)

#bring in data
data <- read.csv("combined_raw_phenology.csv")

#subset data by site
subset_road <- subset(data, 
                      Site %in% c("Road"))

subset_Pfeiler <- subset(data, 
                      Site %in% c("Pfeiler"))

subset_PBM <- subset(data, 
                      Site %in% c("PBM"))

  
  
#individual sites 
##road###
ggplot(subset_road, aes(x = Week, y = Species, fill = Module, color = Year)) +
  geom_density_ridges(
    scale = 1, rel_min_height = .01) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), name = "Week") +
  scale_fill_manual(values = c("magenta4", "orange","turquoise2","forestgreen")) +
  scale_color_manual(values = c("grey", "#0072B250","lightgreen","black"), guide = "none") 

ggplot(subset_road, aes(x = Week, y = Species, Group = Species, height = height) +
  geom_density_ridges(scale = 0.2, stat = "identity", height = 200) +
  theme(legend.position = "none") +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0), name = "Week")) 



##pfeiler###

missingvalues<- ggplot(subset_Pfeiler, aes(x = Week, y = Species, Group= Species, fill = Module)) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), name = "Week")+
  facet_wrap(~Year)

print(missingvalues)

all <- ggplot(subset_Pfeiler, aes(x = Week, y = Species, Group= Species, fill = Module)) +
  geom_density_ridges(stat = "binline", bins = 10, scale = 0.95, draw_baseline = FALSE)+
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), name = "Week")+
  facet_wrap(~Year)
print(all)
##PBM####

##all###
all_sites_density <- ggplot(data, aes(x = Week, y = reorder(Species, Week, decreasing = T), Group = Species, fill = Module, color = factor(Year))) +
  geom_density_ridges(scale = 0.9, show.legend = TRUE) +
  theme(legend.position = "below")+
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0.00001, 0), name = "Week") +
  facet_wrap(~Site) +
  theme_bw() +
  theme(axis.text.y = element_text(face = "italic"))

plot(all_sites_density)

#test w days instead of weeks to include species w 1 week only of flowering
data_days <- read.csv("combined_raw_phenology_days_updates.csv")

#create object to rename sites 
site_names <- c(
  `Road` = "Low (2815 m)",
  `Pfeiler` = "Middle (3165 m)",
  `PBM` = "High (3380 m)"
)

#reorder site names
data_days$Site <- factor(data_days$Site, levels = c("Road","Pfeiler","PBM"))
data_days$Year <- as.factor(data_days$Year)

#reorder modules
data_days$Module <- factor(data_days$Module, levels = c("Beginning", "Middle", "Middle2", "End"))

ggplot(data_days, aes(x = Day, y = reorder(Species, Week, decreasing = T),
                      Group = Species,linetype = Year, color = factor(Module))) +
  geom_density_ridges(scale = 1.5, show.legend = TRUE, alpha = 0.2, rel_min_height =0.008 ) +
  scale_y_discrete(expand = c(0, 0), name = "Species") +
  facet_wrap(~Site, labeller = as_labeller(site_names)) +
  theme_bw() +
  theme(axis.text.y = element_text(face = "italic"))+
  scale_fill_manual( values = c("grey"))+
  scale_colour_manual(values = c("magenta4", "orange", "turquoise2","forestgreen"), name ="Module")+
  scale_x_continuous(breaks = c(7,35,70),labels = c("1", "5", "10"), name = 'Week') 

scale_x_continuous(expand = c(0.00001, 0), name = "Week")

vignette("ggplot2-specs")

###fill area under curve with semi-transparent grey, outline w solid vs dashed for years and different colors for lines, try to make middle and middle2 similar colors 
## average flowering units per week over 7 days, use this for density plot
##divide #flowering per week by 7, google how to expand dataset from weeks to days 

#Val trying something out here----

dat1 <- data %>% group_by(Species, Site) %>%
  count() %>%
  filter( n == 1)

dat2 <- left_join(dat1, data, by = c("Species", "Site"))

# End Val's test -

all_sites_histo <- ggplot(data, aes(x = Week, y = Species, Group= Species, fill = factor(Year), color = Module)) +
  geom_density_ridges(stat = "binline", bins = 10, scale = 0.95, draw_baseline = FALSE)+
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), name = "Week")+
  facet_wrap(~Site)
print(all_sites_histo)

ggplot(data, aes(Week, reorder(Species, Week, decreasing = T), fill= Number_flowering)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE) +
  theme_ipsum() +
  ylab("Species")
