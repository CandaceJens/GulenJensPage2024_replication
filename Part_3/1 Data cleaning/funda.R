rm(list=setdiff(ls(), c("wrds")))

##Setting directory
project.dir <- getwd()

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
                    "funda",
                    "'order by column_name")
res <- dbSendQuery(wrds, myquery)
data <- dbFetch(res, n=-1)
dbClearResult(res)
print(data)


needed_for_table_2 <- c("COALESCE(pstkl, pstkrv, pstk, 0) AS pstkl",
                        "sich")

fundafields <- c("gvkey", "datadate", "cusip", "fyear", needed_for_table_2)




fundaquery <- paste("SELECT", paste(fundafields, collapse=", "),
#                    ", COALESCE(seq, ceq + pstk, at-lt-mib) + COALESCE(txditc, 0) - COALESCE(pstkrv, pstkl, pstk, 0) AS be",
                    "FROM comp.funda",
                    "WHERE DATADATE >= '01JAN1986' AND INDFMT = 'INDL' and DATAFMT='STD' and POPSRC='D' and CONSOL='C'")

print(fundaquery)
res <- dbSendQuery(wrds, fundaquery)
funda <- dbFetch(res)

print(dim(funda))
print(summary(funda))


save(funda, file=paste(project.dir, "Data/funda.Rdata", sep="/"))

