### clear memory
rm(list=ls())

### Required libraries
require(tidyverse)
require(grf)
require(rdd)
require(xtable)

# which dataset?
data.set = "NWallnoloan"

### directories
#setwd(..)
project.dir = getwd()
forest.dir = paste(project.dir,"Causal Forest Output/Forests",sep="/")
data.dir = paste(project.dir, "Causal Forest Output/Forest Data",sep="/")
output.dir = paste(project.dir,"output",sep="/")

# load data, forest, and some additional functions
load(file = paste(data.dir, paste(paste(data.set, "forest_data", sep="_"), "Rdata", sep="."), sep="/"))
load(file = paste(forest.dir, paste(data.set, "forest.RData", sep="_"),sep="/"))
source(paste(project.dir, "functions_additional.R", sep="/"))

# find IK bandwidth 
bdwidth <- IKbandwidth(data$slack.ratio, data$investment)

# Table 5: Average treatment effects for different bandwidths
out_table_6 <- ate_table(forest.main,
                         subsets = cbind(abs(data$slack.ratio) <= bdwidth,
                                         abs(data$slack.ratio) <= bdwidth * 2,
                                         abs(data$slack.ratio) <= bdwidth / 2),
                         subset.names = c("Slack ratio < bandwidth", "Slack ration < 2 x bandwidth", 
                                          "Slack ratio < bandwidth / 2"),
                         bind = data$bind,
                         N.full = dim(data)[1])
print(out_table_6)
print(xtable(out_table_6[c("Estimate","Standard.Error","T.stat","P.value","N")], align=c("l","c","c","c","c","c"), digits = c(0,4,4,3,3,0),
             caption = paste("Treatment Effect for default for differing bandwidth subsamples using data: ", data.set, sep=""),
             label = "tab: bdwidth_TE"),
      file=paste(output.dir, "Table_5.tex",sep="/"),
      caption.placement = "top",
      size = "\\footnotesize")
