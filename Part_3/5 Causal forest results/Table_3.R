### clear memory
rm(list=ls())

### Required libraries
require(tidyverse)
require(grf)
require(xtable)
require(hash)

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

# random seed for use below
my.seed = 467062

### define variables used in the causal forests along with long version of the name
X.names.vars = c("lag.macro.q","cashflow","lag.cashflow","lag.log.assets", "lag.altman_alt", "initial_slack_ratio", 
                 "syndicate_size", "has.rating.all", "cash_assets")
X.names.time = c("year","quarter")
X.names = c(X.names.vars,X.names.time,"firm_fe","slack.ratio")
var.name.dictionary <- hash()
X.names.long <- c("Lag Macro Q", "Cashflow", "Lag Cashflow", "Lag Assets", "Lag Altman Z",
                  "Initial Slack", "Syndicate Size", "Has Credit Rating", "Cash / Assets",
                  "Year", "Quarter", "Firm Fixed Effect", "Distance to Default")
for (i in 1:length(X.names)) var.name.dictionary[[X.names[i]]] = X.names.long[i]

# Table 4: Variable importance for causal forest
var.import = my_variable_importance(data[X.names], forest = forest.main, seed = my.seed, num.perturb = 100)
var.import.out = data.frame(AbsMeanDiff = var.import$absMeanDiff,
                            SDDiff = var.import$diffSD,
                            RatioDiff = var.import$diffVar / var(forest.main$predictions))
colnames(var.import.out) = c("Absolute Mean Difference","Standard Deviation of Differences",
                             "Ratio of Difference Variance to HTE Variance")
var.import$var.names = X.names.long
for (i in 1:length(X.names)) var.import$var.names[i] <- values(var.name.dictionary[var.import$variable[i]])
rownames(var.import.out) = var.import$var.names

var.import.out
print(xtable(var.import.out, align=c("l","c","c","c"), digits = c(0,4,4,4),
             caption = paste("Variable importance ranking for causal forest using data: ", data.set, sep=""),
             label = "tab:var_import"),
      file=paste(output.dir, "Table_3.tex", sep="/"),
      caption.placement = "top",
      size = "\\footnotesize")
