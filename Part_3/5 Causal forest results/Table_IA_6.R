### clear memory
rm(list=ls())

### Required libraries
require(tidyverse)
require(grf)
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
load(file=paste(forest.dir, paste(data.set, "forest.RData", sep="_"),sep="/"))
source(paste(project.dir, "functions_additional.R", sep="/"))

# Table IA 6: Average treatment effects for main sample 
table_3 <- ate_table(forest.main,
                      bind = data$bind,
                      N.full = dim(data)[1])

# print table to screen
print(table_3)

# output LaTeX tables of Table IA.6
print(xtable(table_3[c("Estimate","Standard.Error","T.stat","P.value","N")], align=c("l","c","c","c","c","c"), digits = c(0,4,4,3,3,0),
             caption = paste("Treatment Effect for default using data: ", data.set, sep=""),
             label = paste0("tab:",data.set,"_TE")),
      file=paste(output.dir, "Table_IA_6.tex",sep="/"),
      caption.placement = "top",
      size = "\\footnotesize")
