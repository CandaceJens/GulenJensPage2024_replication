rm(list=ls())

##Loads required packages 
require(lfe)
require(stargazer)
require(rdrobust)
require(dplyr)

### directories
project.dir = getwd()
data.dir = paste(project.dir, "Samples",sep="/")

####Functions are in a separate file (functions.R)
source(paste(project.dir, "functions.R", sep="/"))


# load in net worth sample
load(paste(data.dir, "NWallnoloan.Rdata", sep="/"))
assign("NWallnoloan", tmp, envir = .GlobalEnv)
data <- NWallnoloan[!is.na(NWallnoloan[["Investment/Capital win"]]) & !is.na(NWallnoloan[["slack_ratio win"]]),]
data$slack_ratio_win <- data[["slack_ratio win"]]

data$slack_ratio_win_sq <- data$slack_ratio_win*data$slack_ratio_win
data$slack_ratio_win_cubed <- data$slack_ratio_win*data$slack_ratio_win*data$slack_ratio_win
data$slack_ratio_win_fourth <- data$slack_ratio_win*data$slack_ratio_win*data$slack_ratio_win*data$slack_ratio_win
data$slack_ratio_win_inter <- data$bind * data$slack_ratio_win
data$slack_ratio_win_sq_inter <- data$bind * data$slack_ratio_win_sq
data$slack_ratio_win_cubed_inter <- data$bind * data$slack_ratio_win_cubed
data$slack_ratio_win_fourth_inter <- data$bind * data$slack_ratio_win_fourth

# scale up investment by 100
data[["Investment/Capital win"]] <- 100 * data[["Investment/Capital win"]]

####################
####################
###Panel A.3:  regressions, Limiting OLS to the bandwidth from RDrobust:

# compute bandwidth
optBW <- rdbwselect(data$`Investment/Capital win`, data$slack_ratio_win, 0)

#####Hard coding cutoff using bw calculated above:
data.bw <- subset(data, between(data$slack_ratio_win, -optBW$bws[1], optBW$bws[2]))

dim(data)
dim(data.bw)

###

####Regressions:
reg.1b <- felm(`Investment/Capital win` ~ `bind` | factor(gvkey) + factor(yrq), data=data.bw)
reg.2b <- felm(`Investment/Capital win` ~ `bind` + `lag Macro q win` + `Cash Flow win` + `Log(Assets) win` | factor(gvkey) + factor(yrq), data=data.bw)
reg.3b <- felm(`Investment/Capital win` ~ `bind` + `lag Macro q win` + `Cash Flow win` + `Log(Assets) win` + `slack_ratio_win` | factor(gvkey) + factor(yrq), data=data.bw)
reg.4b <- felm(`Investment/Capital win` ~ `bind` + `lag Macro q win` + `Cash Flow win` + `Log(Assets) win` + `slack_ratio_win` + `slack_ratio_win_sq` | factor(gvkey) + factor(yrq), data=data.bw)
reg.5b <- felm(`Investment/Capital win` ~ `bind` + `lag Macro q win` + `Cash Flow win` + `Log(Assets) win` + `slack_ratio_win` + `slack_ratio_win_sq` + `slack_ratio_win_cubed` | factor(gvkey) + factor(yrq), data=data.bw)
reg.6b <- felm(`Investment/Capital win` ~ `bind` + `lag Macro q win` + `Cash Flow win` + `Log(Assets) win` + `slack_ratio_win` + `slack_ratio_win_sq` + `slack_ratio_win_cubed` + `slack_ratio_win_fourth` | factor(gvkey) + factor(yrq), data=data.bw)


stargazer(reg.1b, reg.2b, reg.3b, reg.4b, reg.5b, reg.6b,
          type = "text",
          
          keep = c("bindTRUE", "slack_ratio_win", "slack_ratio_win_sq", "slack_ratio_win_inter", "slack_ratio_win_sq_inter",
                   "lag Macro q win", "Cash Flow win", "Log(Assets) win"),
          order = c("bindTRUE", "slack_ratio_win", "slack_ratio_win_sq", "slack_ratio_win_inter", "slack_ratio_win_sq_inter",
                    "lag Macro q win", "Cash Flow win", "Log(Assets) win"),
          omit.stat = c("f"),
          df = FALSE,
          digits = 2,
          out = "output/Table_7_Panel_C.tex")
