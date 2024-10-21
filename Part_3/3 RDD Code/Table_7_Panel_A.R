rm(list=ls())

##Loads required packages 
require(lfe)
require(stargazer)

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
###Panel A.1:  regressions

####Regressions:
reg.1 <- felm(`Investment/Capital win` ~ `bind` | factor(gvkey) + factor(yrq), data=data)
reg.2 <- felm(`Investment/Capital win` ~ `bind` + `lag Macro q win` + `Cash Flow win` + `Log(Assets) win` | factor(gvkey) + factor(yrq), data=data)
reg.3 <- felm(`Investment/Capital win` ~ `bind` + `lag Macro q win` + `Cash Flow win` + `Log(Assets) win` + `slack_ratio_win` | factor(gvkey) + factor(yrq), data=data)
reg.4 <- felm(`Investment/Capital win` ~ `bind` + `lag Macro q win` + `Cash Flow win` + `Log(Assets) win` + `slack_ratio_win` + `slack_ratio_win_sq` | factor(gvkey) + factor(yrq), data=data)
reg.5 <- felm(`Investment/Capital win` ~ `bind` + `lag Macro q win` + `Cash Flow win` + `Log(Assets) win` + `slack_ratio_win` + `slack_ratio_win_sq` + `slack_ratio_win_cubed` | factor(gvkey) + factor(yrq), data=data)
reg.6 <- felm(`Investment/Capital win` ~ `bind` + `lag Macro q win` + `Cash Flow win` + `Log(Assets) win` + `slack_ratio_win` + `slack_ratio_win_sq` + `slack_ratio_win_cubed` + `slack_ratio_win_fourth` | factor(gvkey) + factor(yrq), data=data)


stargazer(reg.1, reg.2, reg.3, reg.4, reg.5, reg.6,  
          type = "text", 
          keep = c("bindTRUE", "slack_ratio_win", "slack_ratio_win_sq", "slack_ratio_win_cubed", "slack_ratio_win_fourth",
                   "lag Macro q win", "Cash Flow win", "Log(Assets) win"), 
          order = c("bindTRUE", "slack_ratio_win", "slack_ratio_win_sq", "slack_ratio_win_cubed", "slack_ratio_win_fourth",
                    "lag Macro q win", "Cash Flow win", "Log(Assets) win"), 	
          omit.stat = c("f"),
          df = FALSE,
          digits = 2,
          out = "output/Table_7_Panel_A.tex")
