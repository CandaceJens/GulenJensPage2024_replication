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
load(file = paste(forest.dir, paste(data.set, "forest.RData", sep="_"),sep="/"))
source(paste(project.dir, "functions_additional.R", sep="/"))

# find levels of size (assets) that split distribution into thirds
size.quant = quantile(data$lag.log.assets, c(1/3,2/3))

# Table IA 7: Average treatment effects for Cashflow/Assets subsamples
out_table_6 <- ate_table(forest.main,
                         subsets = cbind(data$lag.log.assets < size.quant[1] & data$cashflow < 0,
                                         data$lag.log.assets < size.quant[1] & data$cashflow >= 0,
                                         data$lag.log.assets > size.quant[2] & data$cashflow < 0,
                                         data$lag.log.assets > size.quant[2] & data$cashflow >= 0),
                         subset.names = c("Small & Cashflow < 0", "Small & Cashflow >= 0", 
                                          "Large & Cashflow < 0", "Large & Cashflow >= 0"),
                         bind = data$bind,
                         N.full = dim(data)[1])
print(out_table_6)
print(xtable(out_table_6[c("Estimate","Standard.Error","T.stat","P.value","N")], align=c("l","c","c","c","c","c"), digits = c(0,4,4,3,3,0),
             caption = paste("Treatment Effect for default for small/large & positive/negative cash flow subsamples using data: ", data.set, sep=""),
             label = "tab:size_cf_TE"),
      file=paste(output.dir, "Table_IA_7.tex",sep="/"),
      caption.placement = "top",
      size = "\\footnotesize")
