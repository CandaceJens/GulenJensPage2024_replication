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

# find levels of Macro Q that split distribution into thirds
q.quant = quantile(data$lag.macro.q, c(1/3,2/3))


# Table 5: Average treatment effects for Cashflow/Macro Q subsamples
out_table_5 <- ate_table(forest.main,
                        subsets = cbind(data$lag.macro.q < q.quant[1] & data$cashflow < 0,
                                        data$lag.macro.q < q.quant[1] & data$cashflow >= 0,
                                        data$lag.macro.q > q.quant[2] & data$cashflow < 0,
                                        data$lag.macro.q > q.quant[2] & data$cashflow >= 0),
                        subset.names = c("Low Q & Neg CF", "Low Q & Pos CF", 
                                         "High Q & Neg CF", "High Q & Pos CF"),
                        bind = data$bind,
                        N.full = dim(data)[1])
print(out_table_5)
print(xtable(out_table_5[c("Estimate","Standard.Error","T.stat","P.value","N")], align=c("l","c","c","c","c","c"), digits = c(0,4,4,3,3,0),
             caption = paste("Treatment Effect for default for low/high macro Q & positive/negative cash flow subsamples using data: ", data.set, sep=""),
             label = "tab:q_cf_TE"),
      file=paste(output.dir, "Table_4.tex",sep="/"),
      caption.placement = "top",
      size = "\\footnotesize")
      