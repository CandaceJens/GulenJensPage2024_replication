### clear memory
rm(list=ls())

##Requires grf_1.1.0.  To load this version of grf:
#packageurl <- "http://cran.r-project.org/src/contrib/Archive/grf/grf_1.1.0.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")
#Note: requires Rtools

#packageurl <- "http://cran.r-project.org/src/contrib/Archive/glmnet/glmnet_4.0-2.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")

### Required libraries
require(tidyverse)
require(grf) 
require(ggplot2)
require(scales)
require(xtable)
require(data.table)
require(fastDummies)
require(glmnet)

# which dataset?
data.set = "NWallnoloan"

### load in data
#setwd(..)
project.dir = getwd()
input.dir = paste(project.dir, "Samples",sep="/")
forest.dir = paste(project.dir,"Causal Forest Output/Forests",sep="/")
data.dir = paste(project.dir, "Causal Forest Output/Forest Data",sep="/")

# random seed for use below
my.seed = 467062

####Functions are in a separate file (functions.R)
source(paste(project.dir, "functions.R", sep="/"))
source(paste(project.dir, "functions_additional.R", sep="/"))

### Load data and clean it up for Forest (non-NA control variables removed) ###
load(file = paste(input.dir, paste(data.set, "Rdata", sep="."), sep="/"))

# rename the variables we care about
tmp.rename = data.frame(gvkey = tmp$gvkey,
                        year = tmp$fyearq,
                        quarter = tmp$fqtr,
                        yearquarter = tmp$yrq,
                        slack = tmp$`min_slack win`,
                        slack.ratio = tmp$`slack_ratio win`,
                        initial_slack = tmp$initial_slack,
                        initial_slack_ratio = tmp$`initial_slack_ratio win`,
                        bind = tmp$bind,
                        investment = tmp$`Investment/Capital win`,
                        log.assets = tmp$`Log(Assets) deflated`,
                        macro.q = tmp$`Macro q win`,
                        cashflow = tmp$`Cash Flow win`,
                        lag.cashflow = tmp$`lag Cash Flow win`,
                        roa = tmp$`ROA win`,
                        leverage = tmp$`Leverage win`,
                        capital.assets = tmp$`Capital/Assets win`,
                        altman = tmp$`altman win`,
                        altman_alt = tmp$`altman_alt win`,
                        lag.altman = tmp$lag.altman,
                        lag.altman_alt = tmp$lag.altman_alt,
                        lag.macro.q = tmp$`lag Macro q win`,
                        lag.log.assets = tmp$`lag Log(Assets) deflated`,
                        lag.roa = tmp$`lag ROA win`,
                        lag.leverage = tmp$`lag Leverage win`,
                        has.rating.all = tmp$has.rating.all,
                        has.rating = tmp$has.rating,
                        cash_assets = tmp$cash_assets,
                        lag_cash_assets = tmp$lag_cash_assets,
                        syndicate_size = tmp$syndicate_size
)
tmp.rename = tmp.rename[order(tmp.rename$gvkey,tmp.rename$year,tmp.rename$quarter),]
data <- subset(tmp.rename, !is.na(tmp.rename$slack) & !is.infinite(tmp.rename$slack) &
                 !is.na(tmp.rename$investment))
dim(data)

### define variables used in the causal forests
X.names.vars = c("lag.macro.q","cashflow","lag.cashflow","lag.log.assets", "lag.altman_alt", "initial_slack_ratio", 
                 "syndicate_size", "has.rating.all", "cash_assets")
X.names.time = c("year","quarter")
X.names = c(X.names.vars,X.names.time)
X.names.2 = c(X.names, "firm_fe", "slack.ratio")

# data with all control variables not NA
data <- data[c(X.names,"investment","bind","slack.ratio","gvkey")]
data <- data[complete.cases(data),]
dim(data)
summary(data)

# LASSO fixed effects
num.firms = length(unique(data$gvkey))
xmat = model.matrix(~ .^3 - 1, data = data[X.names.vars]) # don't include year/quarter here, will be a dummy later
xmatsq <- data[X.names.vars]^2
xmatcub <- data[X.names.vars]^3
yeardum = dummy_cols(data$year)[,2:(length(unique(data$year))+1)]
quarterdum = dummy_cols(data$quarter)[,2:(length(unique(data$quarter))+1)]
firmdum = dummy_cols(data$gvkey)[,2:(num.firms+1)]

x_lasso<-data.matrix(cbind(data$bind,xmat,xmatsq,xmatcub,yeardum,quarterdum,firmdum))
y_lasso<-data.matrix(data$investment)
n.not.id = ncol(x_lasso) - num.firms

out<-cv.glmnet(x=x_lasso,y=y_lasso,alpha=1,penalty.factor=c(rep(1,n.not.id),rep(0,num.firms)))

fe <- coef(out, s = "lambda.min")[(n.not.id+2):(num.firms+n.not.id+1),]
names(fe) <- as.numeric(sub("\\.data_","",names(fe)))
fe <- data.frame(gvkey = as.numeric(names(fe)),
                 firm_fe = fe)
data <- merge(x = data, y = fe, by = "gvkey", all.x = TRUE)

### Causal forest ####
set.seed(my.seed)
forest.bind <- regression_forest_wrapper(X = data[c(X.names,"firm_fe")],
                                         Y = data$bind,
                                         honesty = TRUE,
                                         honesty.fraction = 0.50,
                                         honesty.prune.leaves = TRUE,
                                         sample.fraction = 0.50,
                                         num.trees = 1000,
                                         tune.num.trees = 200,
                                         tune.num.reps = 50,
                                         tune.num.draws = 1000,
                                         seed = my.seed)
save(forest.bind, file=paste(forest.dir, paste(data.set, "forest_bind.RData", sep="_"),sep="/"))
set.seed(my.seed)
forest.invest <- regression_forest_wrapper(X = data[X.names.2],
                                           Y = data$investment,
                                           honesty = TRUE,
                                           honesty.fraction = 0.50,
                                           honesty.prune.leaves = TRUE,
                                           sample.fraction = 0.50,
                                           num.trees = 1000,
                                           tune.num.trees = 200,
                                           tune.num.reps = 50,
                                           tune.num.draws = 1000,
                                           seed = my.seed)
save(forest.invest, file=paste(forest.dir, paste(data.set, "forest_invest.RData", sep="_"),sep="/"))
set.seed(my.seed)
forest.main <- causal_forest_wrapper(X = data[X.names.2],
                                     Y = data$investment,
                                     W = data$bind,
                                     Y.hat = forest.invest$predictions,
                                     W.hat = forest.bind$predictions,
                                     honesty = TRUE,
                                     honesty.fraction = 0.50,
                                     honesty.prune.leaves = TRUE,
                                     sample.fraction = 0.50,
                                     num.trees = 1000,
                                     tune.num.trees = 200,
                                     tune.num.reps = 50,
                                     tune.num.draws = 1000,
                                     seed = my.seed)
save(forest.main, file=paste(forest.dir, paste(data.set, "forest.RData", sep="_"),sep="/"))

# save data used for causal forest
data$hte = forest.main$predictions
data$pred.invest = forest.main$Y.hat
data$pred.bind = forest.main$W.hat
save(data, file = paste(data.dir, paste(paste(data.set, "forest_data", sep="_"), "Rdata", sep="."), sep="/"))
