### clear memory
rm(list=ls())

# libraries
require(tidyverse)
require(ggplot2)

# which dataset?
data.set = "NWallnoloan"

### directories
#setwd(..)
project.dir = getwd()
data.dir = paste(project.dir, "Causal Forest Output/Forest Data",sep="/")
output.dir = paste(project.dir,"output",sep="/")

# load in data
load(file = paste(data.dir, paste(paste(data.set, "forest_data", sep="_"), "Rdata", sep="."), sep="/"))

# additional functions
source(paste(project.dir, "functions_additional.R", sep="/"))

# Figure IA.7: Propensity to be in default around threshold
n.prop = 15
mean.what = mean.by(data$slack.ratio, data$pred.bind,
                    n.prop, c(-0.75,0,0.75))
mean.what$ci.mult = qt(0.975, mean.what$count-1)
plot_propensity <- ggplot() +
  geom_point(data = data[abs(data$slack.ratio) < 0.75,],
             aes(x = slack.ratio, y = pred.bind), alpha = 0.1, color = "darkblue", shape = 4) +
  geom_crossbar(data = mean.what, aes(x = x, y = mean, ymin = mean - ci.mult * se, ymax = mean + ci.mult * se), color = "darkblue") +
  geom_vline(xintercept = 0, linetype = "longdash") +
  scale_x_continuous(limits = c(-0.75,0.75), breaks = c(-0.75,-0.50,-0.25,0,0.25,0.50,0.75), ) +
  xlab(label = "Slack Ratio") +
  ylab(label = "Default Propensity") +
  theme_bw()
 
plot_propensity
  
ggsave(paste(output.dir,"Figure_IA_7.pdf",sep="/"),width=9,height=6)
