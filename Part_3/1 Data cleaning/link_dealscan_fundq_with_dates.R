# Merge in fundq into dealscan data and calculate slack

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

load("Data/merge_data.Rdata") # *_with_gvkey
load("Data/extend_compustat_data.Rdata") # fundq_extended
lenders <- read.csv(paste(project.dir, "Data/lenders.csv",sep="/")) # lenders -> syndicate size

# warning, this code takes a long time
syndicate <- ddply(lenders, "FacilityID", summarise, syndicate_size = length(FacilityID))
syndicate$facilityid = syndicate$FacilityID
syndicate = syndicate[c("facilityid","syndicate_size")] # syndicate size for merging with fc_link_table and nw_link_table_with_slack

print(setdiff(ls(), oldls))


#####

# test whether gvkeys have overlapping facilities
fcdata <- fcdata_with_gvkey %>%
  group_by(gvkey) %>%
  arrange(facilitystartdate) %>%
  mutate(lastfacilityend = lag(facilityenddate))

print(min(fcdata$facilitystartdate - fcdata$lastfacilityend, na.rm=TRUE))
print(names(fundq_extended))


##### Calculate current ratio slack #####

fundq_fc_subset <- subset(fundq_extended, gvkey %in% unique(fcdata$gvkey), select = c("gvkey", "datadate", "Current Ratio"))

print(dim(fundq_fc_subset))

fc_link_table <- inner_join(fundq_fc_subset,
                            fcdata_with_gvkey[c("gvkey", "packageid", "dealactivedate", "facilityid", "facilitystartdate", "facilityenddate", "dealactivedate", "initialratio", "final", "primarysiccode")],
			    by = "gvkey")
fc_link_table <- fc_link_table %>%
  filter(dealactivedate <= datadate)

fc_link_table$covenant_increase <- fc_link_table$final - fc_link_table$initialratio

fc_link_table$age_as_proportion_of_life <- as.numeric(fc_link_table$datadate - fc_link_table$dealactivedate) / as.numeric(fc_link_table$facilityenddate - fc_link_table$dealactivedate)

fc_link_table$datadateratio <- fc_link_table$initialratio + coalesce(fc_link_table$age_as_proportion_of_life * fc_link_table$covenant_increase, 0)


fc_link_table$slack <- fc_link_table[["Current Ratio"]] - fc_link_table$datadateratio
fc_link_table$slack_ratio <- fc_link_table$slack / fc_link_table$datadateratio


fc_earliest_date <- fc_link_table %>%
  filter(dealactivedate <= datadate) %>%
  group_by(packageid) %>%
  summarise(datadate = min(datadate, na.rm=TRUE))
fc_initial_slack <- fc_link_table %>%
  select(packageid, datadate, slack) %>%
  inner_join(fc_earliest_date) %>%
  select(packageid, slack) %>%
  na.omit() %>%
  dplyr::rename(initial_slack = slack)
fc_initial_slack_ratio <- fc_link_table %>%
  select(packageid, datadate, slack_ratio) %>%
  inner_join(fc_earliest_date) %>%
  select(packageid, slack_ratio) %>%
  na.omit() %>%
  dplyr::rename(initial_slack_ratio = slack_ratio)
fc_initial_slack <- full_join(fc_initial_slack, fc_initial_slack_ratio, by = c("packageid"))



fc_link_table <- fc_link_table %>%
  filter(facilitystartdate <= datadate,
         is.na(facilityenddate) | facilityenddate >= datadate) %>%
  left_join(fc_initial_slack, by = "packageid")

### add number of lenders in syndicate to fc_link_table
dim(fc_link_table)
fc_link_table = merge(x = fc_link_table, y = syndicate, by = "facilityid", all.x = TRUE, all.y = FALSE)
dim(fc_link_table)


str(fc_link_table)
summary(fc_link_table)
print(quantile(fc_link_table$slack, probs=seq(0.05, 0.95, 0.05), na.rm=TRUE))
print(quantile(fc_link_table$slack, probs=seq(0.01, 0.99, 0.07), na.rm=TRUE))
summary(subset(fc_link_table, slack < 10))

fc_link_table_min_slack <- fc_link_table %>%
  group_by(gvkey, datadate) %>%
  transmute(min_slack = min(slack, na.rm=TRUE)) %>%
  ungroup()

str(fc_link_table_min_slack)



##### Calculate Net Worth slack #####

print(names(nwdata_with_gvkey))
options(tibble.print_max = 56, tibble.width = Inf)
print(head(nwdata_with_gvkey))

fundq_nw_subset <- subset(fundq_extended, gvkey %in% unique(nwdata_with_gvkey$gvkey), select = c("gvkey", "datadate", "Net Worth", "Tangible Net Worth", "niq"))
fundq_nw_subset <- fundq_nw_subset %>%
  group_by(gvkey) %>%
  arrange(datadate) %>%
  mutate(posniq = ifelse(niq > 0, niq, 0),
         cum_income = cumsum(posniq)) %>%
  ungroup()

print(dim(fundq_nw_subset))
nw_link_table <- inner_join(fundq_nw_subset,
                            nwdata_with_gvkey[c("gvkey", "covenanttype", "packageid", "facilityid", "facilitystartdate", "dealactivedate", "facilityenddate", "baseamt", "percentofnetincome")],
			    by = "gvkey")
# find minimum cum_income for each packageid
nw_table_With_minincome <- nw_link_table %>%
  filter(dealactivedate <= datadate) %>%
  group_by(packageid) %>%
  summarise(min_cum_income = min(cum_income)) %>%
  ungroup() %>%
  right_join(nw_link_table) %>%
  mutate(niq_since_package_start = cum_income - min_cum_income,
         required_net_worth_bump = ifelse(is.na(percentofnetincome),
                                          0,
                                          niq_since_package_start * percentofnetincome / 100),
         `Required Net Worth` = required_net_worth_bump + (baseamt / 1000000))

str(nw_table_With_minincome)
summary(na.omit(nw_table_With_minincome[c("Net Worth", "Required Net Worth", "baseamt")]))


nw_table_with_slack <- nw_table_With_minincome %>%
  mutate(`Net Worth per Covenant` = ifelse(covenanttype == "Tangible Net Worth",
                                           `Tangible Net Worth`,
                                           `Net Worth`),
         # TK skinner and dichev scale slack by total assets, but atq is sometimes less than 0
         slack = `Net Worth per Covenant` - `Required Net Worth`,
         slack_ratio = slack / abs(`Required Net Worth`))

nw_earliest_date <- nw_table_with_slack %>%
  filter(dealactivedate <= datadate) %>%
  group_by(packageid) %>%
  summarise(datadate = min(datadate, na.rm=TRUE))
nw_initial_slack <- nw_table_with_slack %>%
  select(packageid, datadate, slack) %>%
  inner_join(nw_earliest_date) %>%
  select(packageid, slack) %>%
  na.omit() %>%
  rename(initial_slack = slack)
nw_initial_slack_ratio <- nw_table_with_slack %>%
  select(packageid, datadate, slack_ratio) %>%
  inner_join(nw_earliest_date) %>%
  select(packageid, slack_ratio) %>%
  na.omit() %>%
  rename(initial_slack_ratio = slack_ratio)
nw_initial_slack <- full_join(nw_initial_slack, nw_initial_slack_ratio, by = c("packageid"))



nw_table_with_slack <- nw_table_with_slack %>%
  filter(facilitystartdate <= datadate,
         is.na(facilityenddate) | facilityenddate >= datadate) %>%
  left_join(nw_initial_slack, by = "packageid")


### add number of lenders in syndicate to fc_link_table
dim(nw_table_with_slack)
nw_table_with_slack = merge(x = nw_table_with_slack, y = syndicate, by = "facilityid", all.x = TRUE, all.y = FALSE)
dim(nw_table_with_slack)

str(fc_link_table)
str(nw_table_with_slack)

save(fc_link_table,
     nw_table_with_slack,
     file = "Data/link_dealscan_fundq_with_dates.Rdata")
