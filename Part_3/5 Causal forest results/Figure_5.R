### clear memory
rm(list=ls())

### Required libraries
require(tidyverse)
require(ggplot2)
require(scales)
require(gridExtra)
require(ggpubr)
require(grf)
require(xtable)

# which dataset?
data.set = "NWallnoloan"


### directories
#setwd(..)
project.dir = getwd()
data.dir = paste(project.dir, "Causal Forest Output/Forests",sep="/")
output.dir = paste(project.dir,"output",sep="/")

# load data, forest, and some additional functions
load(file = paste(data.dir, paste(paste(data.set, "forest", sep="_"), "Rdata", sep="."), sep="/"))


htes <- as_tibble(forest.main$predictions)

# Figure 5: Histogram of observation-level treatment effects
hist.hte = ggplot(data = htes, aes(x=V1)) +
  geom_density(aes(y=stat(count / sum(count))), size = 0.8, alpha=0, position="identity") +
  scale_y_continuous(labels = percent_format()) +
  geom_hline(yintercept = 0, size=0.8) +
  scale_x_continuous(limits = c(-0.04, 0.03), breaks = c(-0.04,-0.04,-0.03,-0.02,-0.01,0,0.01,0.02,0.03)) +
  xlab("Heterogeneous Treatment Effect") +
  ylab("Percent of Sample") +
  theme_bw() + 
  geom_vline(xintercept = -0.0024, linetype = "dashed") + 
  geom_vline(xintercept = -0.0054, linetype = "dotdash") +
  annotate(geom = "text", label = "ATE = -0.24% (t-stat = -1.326)", ##Note: these numbers are from Table IA.6 in the internet appendix
          x = 0.009, y = 0.0155, colour = "black", size=3.5)+
  annotate(geom = "text", label = "ATT = -0.54% (t-stat = -5.670)", 
           x = -0.017, y = 0.0125, colour = "black", size=3.5)

hist.hte

ggsave(hist.hte, 	
       		filename = paste(project.dir, "output/Figure_5.pdf", sep="/"),
             	width = 9, height = 6, units = "in")
       