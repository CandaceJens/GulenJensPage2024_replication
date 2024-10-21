# Merge in fundq into dealscan data and calculate slack
rm(list=ls())

##Setting directory
project.dir <- getwd()

#### Load Data ####
oldls <- ls()
oldls <- ls()

load(paste0(project.dir, "/Data/rawdata.Rdata"))
load(paste0(project.dir, "/Data/extend_compustat_data.Rdata")) # fundq_extended
load(paste0(project.dir, "/Data/link_dealscan_fundq_with_dates.Rdata"))

print(setdiff(ls(), oldls))

save(covenant_violations, fc_link_table, fundq_extended, nw_table_with_slack,
     file=paste0(project.dir, "/Data/collect_processed_data.Rdata"))
