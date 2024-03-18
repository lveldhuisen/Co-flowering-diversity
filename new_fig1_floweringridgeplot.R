install.packages("ggridges")
library(ggridges)
library(ggplot2)
library(tidyverse)

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

#Val trying something out here----

dat1 <- data %>% group_by(Species, Site) %>%
  count() %>%
  filter( n == 1)

# End Val's test -

all_sites_histo <- ggplot(data, aes(x = Week, y = Species, Group= Species, fill = factor(Year), color = Module)) +
  geom_density_ridges(stat = "binline", bins = 10, scale = 0.95, draw_baseline = FALSE)+
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), name = "Week")+
  facet_wrap(~Site)
print(all_sites_histo)
