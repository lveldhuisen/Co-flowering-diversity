#this file contains code for figures and analyses that were not included in the final manuscript
#code to generate the objects used here is in the "SchoenersIndex.R" file 



###compare distributions for fitness SI between sites 2021#################
library(tidyverse)
library(ggpubr)

kruskal.test(SI_f ~ site, data = Merged2021_fitness)

chisq.test(Merged2021_fitness$site,Merged2021_fitness$SI_f)

ggboxplot(Merged2021_fitness, x = "site", y = "SI_f", 
          color = "site", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Road", "Pfeiler", "PBM"),
          ylab = "Schoener's Index", xlab = "Site", title = "2021 fitness")