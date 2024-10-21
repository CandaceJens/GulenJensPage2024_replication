rm(list=ls())

##Loads required packages 
library(plyr)
library(tidyverse)

##Setting directory

project.dir <- getwd()

####Functions are in a separate file (functions.R)
source(paste(project.dir, "functions.R", sep="/"))

#### Load Data ####

oldls <- ls()
oldls <- ls()

load(paste0(project.dir, "/Data/collect_processed_data.Rdata"))


print(setdiff(ls(), oldls))

####collect_processed_data contains:
##covenant_violations -- covenant violations data (gvkey, cik, date, filingpath, filedate, formtype) 
##fc_link_table -- can have multiple slack obs for each firm-quarter
##fc_link_table_min_slack -- retains only minimum slack obs for each firm-quarter
##nw_link_table -- net worth data
##fundq_extended -- Compustat variables; Market-to-Book has been augmented with price data from CRSP

######Get rid of loans with negative initial slack:

  dim(fc_link_table)
fc_link_table <- subset(fc_link_table, initial_slack >= 0)
  dim(fc_link_table)




##this looks at each firm (gvkey) and date (datadate) and selects the minimum slack obs.
##for each quarter, we only care about the minimum slack (one default is sufficient to default)
# fc_link_table_min_slack <- fc_link_table %>%
#   group_by(gvkey, datadate) %>%
#   transmute(min_slack = min(slack, na.rm=TRUE)) %>%
#   ungroup()
# 
# ##merges the minimum slack back into the data:
# fc_link_table_min_slack <- merge(fc_link_table_min_slack, fc_link_table, by = c("gvkey", "datadate"), all = TRUE)

fc_link_table_min_slack <- fc_link_table %>%
  group_by(gvkey, datadate) %>%
  mutate(min_slack = min(slack, na.rm=TRUE)) %>%
  ungroup()
  
print(dim(fc_link_table_min_slack))

##To keep minimum slack:  (but there could be more than one facility at the minimum)
fc_link_table_min_slack <- filter(fc_link_table_min_slack, slack == min_slack)

print(dim(fc_link_table_min_slack))

##only keep the first observation for each gvkey datadate pair
fc_link_table_min_slack <- fc_link_table_min_slack[!duplicated(fc_link_table_min_slack[c("gvkey", "datadate")]),]

print(dim(fc_link_table_min_slack))

##merge to compustat:
tmp <- inner_join(fc_link_table_min_slack, fundq_extended)
print(dim(fc_link_table_min_slack))
print(dim(fundq_extended))
print(dim(tmp))


##bind variable is variable of interest in regressions:
tmp$bind <- tmp$min_slack < 0
print(dim(tmp))


###remove financials and limit data to post 1994::
lapply(tmp, function(foo){mean(is.na(foo))}) %>% as.data.frame()

tmp <- tmp %>%
  #filter(datadate > as.POSIXct("1994-01-01 01:00:00")) %>%
  filter(between(tmp$fyearq, 1994, 2017)) %>%
  filter(sich < 6000 | sich >= 7000)            # no financials


print(dim(tmp))

#### Winsorize 1% variables for regression:


tmp[["Macro q win"]] <- winsorize(tmp[["Macro q"]], probs = c(0.01, 0.99), truncate = TRUE)
tmp[["Cash Flow win"]] <- winsorize(tmp[["Cash Flow"]], probs = c(0.01, 0.99), truncate = TRUE)
tmp[["Log(Assets) win"]] <- winsorize(tmp[["Log(Assets)"]], probs = c(0.01, 0.99), truncate = TRUE)
tmp[["ROA win"]] <- winsorize(tmp[["ROA"]], probs = c(0.01, 0.99), truncate = TRUE)
tmp[["Investment/Capital win"]] <- winsorize(tmp[["Investment/Capital"]], probs = c(0.01, 0.99), truncate = TRUE)
tmp[["Leverage win"]] <- winsorize(tmp[["Leverage"]], probs = c(0.01, 0.99), truncate = TRUE)
tmp[["Capital/Assets win"]] <- winsorize(tmp[["Capital/Assets"]], probs = c(0.01, 0.99), truncate = TRUE)
tmp[["altman win"]] <- winsorize(tmp[["altman"]], probs = c(0.01, 0.99), truncate = TRUE)
tmp[["altman_alt win"]] <- winsorize(tmp[["altman_alt"]], probs = c(0.01, 0.99), truncate = TRUE)
tmp[["cash_assets win"]] <- winsorize(tmp[["cash_assets"]], probs = c(0.01, 0.99), truncate = TRUE)

tmp[["lag Macro q win"]] <- winsorize(tmp[["lag Macro q"]], probs = c(0.01, 0.99), truncate = TRUE)
tmp[["lag Log(Assets) deflated win"]] <- winsorize(tmp[["lag Log(Assets) deflated"]], probs = c(0.01, 0.99), truncate = TRUE)
tmp[["lag Cash Flow win"]] <- winsorize(tmp[["lag Cash Flow"]], probs = c(0.01, 0.99), truncate = TRUE)
tmp[["lag ROA win"]] <- winsorize(tmp[["lag ROA"]], probs = c(0.01, 0.99), truncate = TRUE)
tmp[["lag Leverage win"]] <- winsorize(tmp[["lag Leverage"]], probs = c(0.01, 0.99), truncate = TRUE)
tmp$lag.altman_win <- winsorize(tmp[["altman"]], probs = c(0.01, 0.99), truncate = TRUE)
tmp$lag.altman_alt_win <- winsorize(tmp[["altman_alt"]], probs = c(0.01, 0.99), truncate = TRUE)
tmp$lag.cash_assets_win <- winsorize(tmp[["cash_assets"]], probs = c(0.01, 0.99), truncate = TRUE)

tmp[["min_slack win"]] <- winsorize(tmp[["min_slack"]], probs = c(0.025, 0.975), truncate = TRUE)
tmp[["slack_ratio win"]] <- winsorize(tmp[["slack_ratio"]], probs = c(0.025, 0.975), truncate = TRUE)
tmp[["initial_slack_ratio win"]] <- winsorize(tmp[["initial_slack_ratio"]], probs = c(0.025, 0.975), truncate = TRUE)

tmp[["lag Macro q win sq"]] <- tmp[["lag Macro q win"]]^2

######

  dim(tmp)
tmp <- subset(tmp, tmp["Leverage win"] > 0)
  dim(tmp)

#############
###Save data:
save(tmp, file = paste(project.dir, "Samples/CRallnoloan.Rdata", sep = "/"))
