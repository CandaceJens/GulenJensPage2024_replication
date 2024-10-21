rm(list=ls())

##Loads required packages 
library(plyr)
library(tidyverse)
library(lubridate)
library(readxl)

##Setting directory
project.dir <- getwd()

####Functions are in a separate file (functions.R)
source(paste(project.dir, "functions.R", sep="/"))


cpi <- read.csv(paste(project.dir, "Data/cpi.csv",sep="/"))
cpi$datadate = ymd(cpi$datadate)
cpi$cpi <- cpi$cpi / 100

##### load data from Roberts website #####


roberts.xl.filenames <- list.files(paste0(project.dir, "/Data"), pattern = ".xlsx?$")

roberts.xl.files <- paste0(project.dir, "/Data/", roberts.xl.filenames)

print(roberts.xl.files)

for(thing in roberts.xl.files){
  print(thing)
  print(excel_sheets(thing))
}

link_data <- read_excel(paste0(project.dir, "/Data/dealscan_compustat_link.xlsx"),
                        sheet = "link_data")
# facid and bcoid are numeric
# gvkey is numeric
# facstartdate is POSIXct

link_data <- dplyr::rename(link_data, companyid = bcoid,
                               facilityid = facid)
str(link_data)
link_data <- link_data %>% select(companyid, facilityid, facstartdate, gvkey)
str(link_data)

covenant_violations <- read_excel(paste0(project.dir, "/Data/sec_covenant_violations_24_Sep_2012.xlsx"))
covenant_violations$date <- parse_date(covenant_violations$date, format = "%m/%d/%Y")
covenant_violations$fileddate <- parse_date(as.character(covenant_violations$fileddate),
                                            format = "%Y%m%d")

##### load WRDS #####

oldls <- ls()
load(paste0(project.dir, "/Data/dealscan.Rdata")) # fcdata, nwdata
print(setdiff(ls(), oldls))


fcdata$facilityamt <- fcdata$facilityamt / 1000000000
nwdata$facilityamt <- nwdata$facilityamt / 1000000000

oldls <- ls()
load(paste0(project.dir, "/Data/fundq.Rdata")) # fundq
print(setdiff(ls(), oldls))

msf <- na.omit(msf)
msf <- msf %>%
  mutate(yrmo = date.to.yrmo(date),
         mkval = mkval / 1000.0)

fundq <- fundq %>%
  mutate(yrmo = date.to.yrmo(datadate))

fundq <- left_join(fundq, msf)

fundq$mkvaltq <- coalesce(fundq$mkvaltq, fundq$mkval)


fundq$gvkey <- as.numeric(fundq$gvkey)

print(dim(fundq))
fundq <- fundq %>%
  arrange(gvkey, datadate, fyearq, fqtr)
dupkeys <- fundq[c("gvkey", "datadate")]

# most duplicated gvkey/datadate pairs seem to be identical except fiscal quarter,
# like the firm shifted its fiscal year end and reported the same quarter in two
# different fiscal years, so we'll keep the first of each pair
fundq <- fundq[!duplicated(dupkeys),, drop = FALSE]
print(dim(fundq))





oldls <- ls()
load(paste0(project.dir, "/Data/funda.Rdata")) # funda
print(setdiff(ls(), oldls))

funda$gvkey <- as.numeric(funda$gvkey)

print(dim(funda))
print(dim(unique(funda[c("gvkey", "datadate")])))



gvkeys_to_keep <- union(unique(link_data$gvkey), unique(covenant_violations$gvkey))

# only keep fundq observations that we might use
#   -- no, table 2 wants full summary statistics
# fundq <- subset(fundq, gvkey %in% gvkeys_to_keep)



# deflate facilityamt by cpi$cpi matching dealactivedate on cpi$date

cpi <- cpi %>%
  mutate(yrmo = date.to.yrmo(datadate)) %>%
  select(yrmo, cpi)

fcdata_with_yrmo <- fcdata %>%
  mutate(yrmo = date.to.yrmo(dealactivedate))
fcdata <- left_join(fcdata_with_yrmo, cpi) %>%
  mutate(facilityamt2000 = facilityamt / cpi)

nwdata_with_yrmo <- nwdata %>%
  mutate(yrmo = date.to.yrmo(dealactivedate))
nwdata <- left_join(nwdata_with_yrmo, cpi) %>%
  mutate(facilityamt2000 = facilityamt / cpi)


str(fcdata)


save(covenant_violations, link_data, fundq, funda, fcdata, nwdata, cpi, file = "Data/rawdata.Rdata")
