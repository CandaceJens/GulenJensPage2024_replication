rm(list=ls())

##Loads required packages 
library(plyr)
library(tidyverse)

##Setting directory
project.dir <- getwd()

print(Sys.getenv("USERDOMAIN"))

####Functions are in a separate file (functions.R)
source(paste(project.dir, "functions.R", sep="/"))

#### Load Data ####
oldls <- ls()

load(file = "Data/rawdata.Rdata")

print(setdiff(ls(), oldls))

rm(list = intersect(ls(), c("covenant_violations", "link_data", "fcdata", "nwdata")))
print(setdiff(ls(), oldls))



print(names(funda))
print(head(funda))
print(intersect(names(funda), names(fundq)))

funda_pared <- funda %>% dplyr::select(-datadate, -cusip) %>% dplyr::rename(fyearq = fyear)

print(names(funda_pared))
print(intersect(names(funda_pared), names(fundq)))

fundq <- left_join(fundq, funda_pared, by = c("gvkey", "fyearq"))

summary(fundq[c("sich", "pstkl")])
print(mean(is.na(fundq$pstkl)))
print(mean(fundq$pstkl < 0, na.rm=TRUE))
print(mean(fundq$pstkl == 0, na.rm=TRUE))

fundq$pstkl <- ifelse(is.na(fundq$pstkl) | fundq$pstkl < 0, 0, fundq$pstkl)




##### produce new fundq variables #####


# lagged_ppentq

fundq_with_lags <- fundq %>%
  mutate(yrq = fyearq * 4 + fqtr) %>%
  group_by(gvkey) %>%
  arrange(datadate) %>%
  mutate(lagged_yrq = lag(yrq),
         lagged_ppentq = lag(ppentq),
         lagged_capxy = lag(capxy),
         lagged_ibcy = lag(ibcy),
         lagged_dpcy = lag(dpcy)) %>%
  ungroup()

print(names(fundq_with_lags))

# capxq from capxy
# ibcy + dpcy

fundq_with_quarterly_cash_flows <- fundq_with_lags %>%
  mutate(capxq = capxy - ifelse(fqtr == 1, 0, lagged_capxy),
         ibcq = ibcy - ifelse(fqtr == 1, 0, lagged_ibcy),
         dpcq = dpcy - ifelse(fqtr == 1, 0, lagged_dpcy),
         good_lag = (yrq - lagged_yrq == 1)) %>%
  # except that if good_lag is missing or FALSE, the others are NA
  mutate(capxq = ifelse(good_lag, capxq, NA),
         ibcq = ifelse(good_lag, ibcq, NA),
         dpcq = ifelse(good_lag, dpcq, NA))

fundq_with_quarterly_cash_flows <- fundq_with_quarterly_cash_flows %>%
  mutate(capxy = NULL,
         ibcy = NULL,
         dpcy = NULL,
         good_lag = NULL)


## creating variables
fundq_extended <- fundq_with_quarterly_cash_flows %>%
  mutate(`Current Ratio` = actq / lctq,
         `Net Worth` = atq - ltq,
         `Tangible Net Worth` = actq + ppentq + aoq - ltq,
         `Log(Assets)` = log(atq),
	       `Market-to-Book` = (mkvaltq + dlttq + pstkl - txditcq) / atq,
         `Macro q` = (mkvaltq + dlttq - invtq) / lagged_ppentq,
         ROA = oibdpq / atq,
         `Capital/Assets` = ppentq / atq,
         `Investment/Capital` = capxq / lagged_ppentq,
         `Cash Flow` = (ibcq + dpcq) / lagged_ppentq,
         Leverage = dlttq / atq,
         altman_sales = saleq / atq,
         altman = altman_numerator / atq + altman_sales, # as defined in Chava & Roberts (2008) appendix
         altman_alt = altman + 0.6 * mkvaltq / ltq) # definition we use
fundq_extended$cash_assets = ifelse(fundq_extended$cheq < 0, NA, fundq_extended$cheq / fundq_extended$atq) # NA if negative cash holdings

##add some new variables
# credit rating
ratings <- read.csv(paste(project.dir, "Data/ratings.csv",sep="/"))
dim(ratings)
ratings$has.rating = ifelse(ratings$splticrm=="" & ratings$spsdrm=="" & ratings$spsticrm=="",0,1) # using whether or not the firm has any rating at all
ratings <- as_tibble(ratings[c("gvkey","datadate","has.rating")])
ratings$datadate <- ymd(ratings$datadate)
#merge credit rating with tmp
dim(fundq_extended)
fundq_extended <- left_join(x=fundq_extended, y=ratings, by=c("gvkey","datadate"))
fundq_extended$has.rating.all <- ifelse(is.na(fundq_extended$has.rating), 0, fundq_extended$has.rating) # if rating is NA, then the firm probably doesn't have a rating
dim(fundq_extended)

# deflate log of assets
fundq_extended <- left_join(x = fundq_extended, y = cpi, by = "yrmo")
fundq_extended[["Log(Assets) deflated"]] = fundq_extended$`Log(Assets)` - log(fundq_extended$cpi)

### lag variables of interest
fundq_extended <- fundq_extended[order(fundq_extended$gvkey, fundq_extended$yrmo),]
fundq_extended[["lag Macro q"]] <- df.lag(keepcolumns(fundq_extended, c("Macro q", "gvkey")), lag = 1, groups = c("gvkey"))[["Macro q"]]
fundq_extended[["lag Market-to-Book"]] <- df.lag(keepcolumns(fundq_extended, c("Market-to-Book", "gvkey")), lag = 1, groups = c("gvkey"))[["Market-to-Book"]]
fundq_extended[["lag Cash Flow"]] <- df.lag(keepcolumns(fundq_extended, c("Cash Flow", "gvkey")), lag = 1, groups = c("gvkey"))[["Cash Flow"]]
fundq_extended[["lag Log(Assets) deflated"]] <- df.lag(keepcolumns(fundq_extended, c("Log(Assets) deflated", "gvkey")), lag = 1, groups = c("gvkey"))[["Log(Assets) deflated"]]
fundq_extended[["lag Log(Sales)"]] <- df.lag(keepcolumns(fundq_extended, c("Log(Sales)", "gvkey")), lag = 1, groups = c("gvkey"))[["Log(Sales)"]]
fundq_extended[["lag ROA"]] <- df.lag(keepcolumns(fundq_extended, c("ROA", "gvkey")), lag = 1, groups = c("gvkey"))[["ROA"]]
fundq_extended[["lag Leverage"]] <- df.lag(keepcolumns(fundq_extended, c("Leverage", "gvkey")), lag = 1, groups = c("gvkey"))[["Leverage"]]
fundq_extended$lag.altman = df.lag(keepcolumns(fundq_extended, c("altman", "gvkey")), lag = 1, groups = c("gvkey"))[["altman"]]
fundq_extended$lag.altman_alt = df.lag(keepcolumns(fundq_extended, c("altman_alt", "gvkey")), lag = 1, groups = c("gvkey"))[["altman_alt"]]
fundq_extended$lag_cash_assets = df.lag(keepcolumns(fundq_extended, c("cash_assets", "gvkey")), lag = 1, groups = c("gvkey"))$cash_assets
dim(fundq_extended)

# save data
save(fundq_extended, file = "Data/extend_compustat_data.Rdata")
