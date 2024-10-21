rm(list=ls())

##Loads required packages 
library(plyr)
library(tidyverse)

##Setting directory
project.dir <- getwd()

####Functions are in a separate file (functions.R)
source(paste(project.dir, "functions.R", sep="/"))



#### Load Data ####

oldls <- c(ls(), "oldls")

load(paste0(project.dir, "/Data/rawdata.Rdata"))

rm(list="fundq")

print(setdiff(ls(), oldls))

#### Merge fundq into fcdata using link_data ####

print(intersect(names(fcdata), names(link_data)))

fcdata <- subset(fcdata, covenanttype == "Min. Current Ratio")

fcdata_with_gvkey <- inner_join(fcdata, link_data[c("facilityid", "gvkey")])
nwdata_with_gvkey <- inner_join(nwdata, link_data[c("facilityid", "gvkey")])


save(fcdata_with_gvkey, nwdata_with_gvkey, cpi, file = "Data/merge_data.Rdata")
