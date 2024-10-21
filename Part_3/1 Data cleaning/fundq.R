rm(list=setdiff(ls(), c("wrds")))

##Setting directory
project.dir <- getwd()


# current assets
# current assets
# current liabilities
# total assets
# total liabilities
# net ppe
# "other assets" -- i.e., tangible net worth is current assets plus net ppe plus other assets minus total liabilities
# total assets deflated by the all-urban CPI
# market value of assets: market equity, total debt, and preferred stock liquidation value, minus deferred taxes and investment tax credits
# TK: macro q, etc.


myquery <- paste0("select column_name, ordinal_position, data_type",
                    " from information_schema.columns where table_schema='comp' and table_name='",
                    "fundq",
                    "'order by column_name")
res <- dbSendQuery(wrds, myquery)
data <- dbFetch(res, n=-1)
dbClearResult(res)
print(data)

needed_for_table_2 <- c("actq", "aoq", "atq", "capxy", "cheq", "dlttq", "dpcy", "ibcy", 
                        "invtq", "lctq", "ltq", "coalesce(mkvaltq, prccq * cshoq) as mkvaltq", "oibdpq", "ppentq", "saleq", "txditcq")

fundqfields <- c("gvkey", "datadate", "cusip", "fyearq", "fqtr",
		 "niq",
                 needed_for_table_2,
		 "(3.3 * piq + 1.4 * req + 1.2 * (actq - lctq) + saleq) as altman_numerator")


fundqquery <- paste("SELECT", paste(fundqfields, collapse=", "),
                    "FROM comp.fundq",
                    "WHERE DATADATE >= '01JAN1986' AND INDFMT = 'INDL' and DATAFMT='STD' and POPSRC='D' and CONSOL='C'")

print(fundqquery)
res <- dbSendQuery(wrds, fundqquery)
fundq <- dbFetch(res)
dbClearResult(res)

print(dim(fundq))
print(summary(fundq))


msfquery <- "SELECT msf.date, abs(msf.prc) * msf.shrout as mkval, link.gvkey FROM crsp.msf as msf, crsp.ccmxpf_linktable as link WHERE msf.date >= '01JAN1980' and link.lpermno = msf.permno and msf.date >= link.linkdt and (msf.date < link.linkenddt or linkenddt is null) and link.usedflag = 1"
res <- dbSendQuery(wrds, msfquery)
msf <- dbFetch(res)
dbClearResult(res)
print(summary(msf))



save(fundq, msf, file=paste(project.dir, "Data/fundq.Rdata", sep="/"))


