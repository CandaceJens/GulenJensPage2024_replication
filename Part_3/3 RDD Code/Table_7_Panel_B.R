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
###Panel A.2:  regressions with bind interaction

####Regressions:
reg.3a <- felm(`Investment/Capital win` ~ `bind` + `lag Macro q win` + `Cash Flow win` + `Log(Assets) win` + 
                 `slack_ratio_win` + `slack_ratio_win_inter` | factor(gvkey) + factor(yrq), data=data)
reg.4a <- felm(`Investment/Capital win` ~ `bind` + `lag Macro q win` + `Cash Flow win` + `Log(Assets) win` + 
                 `slack_ratio_win` + `slack_ratio_win_inter` + `slack_ratio_win_sq` + `slack_ratio_win_sq_inter` | factor(gvkey) + factor(yrq), data=data)
reg.5a <- felm(`Investment/Capital win` ~ `bind` + `lag Macro q win` + `Cash Flow win` + `Log(Assets) win` + 
                 `slack_ratio_win` + `slack_ratio_win_inter` + `slack_ratio_win_sq` + `slack_ratio_win_sq_inter` +
                 `slack_ratio_win_cubed` + `slack_ratio_win_cubed_inter` | factor(gvkey) + factor(yrq), data=data)
reg.6a <- felm(`Investment/Capital win` ~ `bind` + `lag Macro q win` + `Cash Flow win` + `Log(Assets) win` + 
                 `slack_ratio_win` + `slack_ratio_win_inter` + `slack_ratio_win_sq` + `slack_ratio_win_sq_inter` +
                 `slack_ratio_win_cubed` + `slack_ratio_win_cubed_inter` + `slack_ratio_win_fourth` + `slack_ratio_win_fourth_inter` | factor(gvkey) + factor(yrq), data=data)

stargazer(reg.3a, reg.4a, reg.5a, reg.6a,
          type = "text",
          keep = c("bindTRUE", "slack_ratio_win", "slack_ratio_win_sq", "slack_ratio_win_cubed", "slack_ratio_win_fourth",
                   "slack_ratio_win_inter", "slack_ratio_win_sq_inter", "slack_ratio_win_cubed_inter", "slack_ratio_win_fourth_inter",
                   "lag Macro q win", "Cash Flow win", "Log(Assets) win"),
          order = c("bindTRUE", "slack_ratio_win", "slack_ratio_win_sq", "slack_ratio_win_cubed", "slack_ratio_win_fourth",
                    "slack_ratio_win_inter", "slack_ratio_win_sq_inter", "slack_ratio_win_cubed_inter", "slack_ratio_win_fourth_inter",
                    "lag Macro q win", "Cash Flow win", "Log(Assets) win"),
          omit.stat = c("f"),
          df = FALSE,
          digits = 2,
          out = "output/Table_7_Panel_B.tex")
