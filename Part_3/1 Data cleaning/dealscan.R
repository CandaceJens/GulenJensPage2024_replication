rm(list=setdiff(ls(), c("wrds")))

##Setting directory
project.dir <- getwd()


# need company, package, and facility tables
#    networthcovenant and financialcovenant

res <- dbSendQuery(wrds, "select distinct table_schema
                   from information_schema.tables
                   where table_type ='VIEW'
                   order by table_schema")
data <- dbFetch(res, n=-1)
dbClearResult(res)
print(data)



res <- dbSendQuery(wrds, "select distinct table_name
                   from information_schema.columns
                   where table_schema='dealscan'
                   order by table_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data


# helper function
getColumnsFromDealscanTable <- function(tablename){
  myquery <- paste0("select * from information_schema.columns where table_schema='dealscan' and table_name='",
                    tablename,
                    "'order by column_name")
  res <- dbSendQuery(wrds, myquery)
  data <- dbFetch(res, n=-1)
  dbClearResult(res)
  data
}

print(getColumnsFromDealscanTable("company"))
print(getColumnsFromDealscanTable("package"))
print(getColumnsFromDealscanTable("facility"))
print(getColumnsFromDealscanTable("financialcovenant"))
print(getColumnsFromDealscanTable("networthcovenant"))

res <- dbSendQuery(wrds, "select *
                          from dealscan.package")
data <- dbFetch(res, n=-1)
dbClearResult(res)
print(dim(data))
data <- data[1:100,]
print(head(data, 20))

res <- dbSendQuery(wrds, "select *
                          from dealscan.financialcovenant")
data <- dbFetch(res, n=-1)
dbClearResult(res)
print(dim(data))
data <- data[1:1000,]
print(head(data, 20))


res <- dbSendQuery(wrds, "select *
                          from dealscan.networthcovenant")
data <- dbFetch(res, n=-1)
dbClearResult(res)
print(dim(data))
data <- data[1:1000,]
print(head(data, 20))


fcfields <- c("packageid", "covenanttype", "initialratio", "trend", "final")


foo <- paste0("select c.companyid, c.company, c.country, c.primarysiccode,
                                 p.dealactivedate, p.collateralrelease, p.dealamount, p.dividendrestrictions, p.percentageofexcesscf, p.percentageofnetincome, p.refinancingindicator, p.spreadoverdefaultbase,
                                 f.conversiondate, f.countryofsyndication, f.facilityamt, f.currency, f.facilityenddate, f.facilitystartdate, f.facilityid, fc.",
			paste(fcfields, collapse = ", fc."),
                          " from dealscan.company as c, dealscan.package as p, dealscan.facility as f, dealscan.financialcovenant as fc
                          where c.Companyid = p.BorrowerCompanyID and p.packageid = f.packageid and p.packageid = fc.packageid")
print(foo)

res <- dbSendQuery(wrds, paste0("select c.companyid, c.company, c.country, c.primarysiccode,
                                 p.defaultbaserate, p.dealactivedate, p.collateralrelease, p.dealamount, p.dividendrestrictions, p.percentageofexcesscf, p.percentageofnetincome, p.refinancingindicator, p.spreadoverdefaultbase,
                                 f.conversiondate, f.countryofsyndication, f.facilityamt, f.maturity, f.currency, f.facilityenddate, f.facilitystartdate, f.facilityid, fc.",
			paste(fcfields, collapse = ", fc."),
                          " from dealscan.company as c, dealscan.package as p, dealscan.facility as f, dealscan.financialcovenant as fc 
                          where c.Companyid = p.BorrowerCompanyID and p.packageid = f.packageid and p.packageid = fc.packageid"))
fcdata <- dbFetch(res, n=-1)
dbClearResult(res)
print(dim(fcdata))
print(names(fcdata))



res <- dbSendQuery(wrds, "select c.companyid, c.company, c.country, c.primarysiccode,
                                 p.defaultbaserate, p.dealactivedate, p.collateralrelease, p.dealamount, p.dividendrestrictions, p.percentageofexcesscf, p.percentageofnetincome, p.refinancingindicator, p.spreadoverdefaultbase,
                                 f.conversiondate, f.countryofsyndication, f.facilityamt, f.currency, f.maturity, f.facilityenddate, f.facilitystartdate, f.facilityid,
                                 fc.*
                          from dealscan.company as c, dealscan.package as p, dealscan.facility as f, dealscan.networthcovenant as fc
                          where c.Companyid = p.BorrowerCompanyID and p.packageid = f.packageid and p.packageid = fc.packageid")
nwdata <- dbFetch(res, n=-1)
dbClearResult(res)
print(dim(nwdata))
print(names(nwdata))

# save financial covenant and net worth covenant tables
save(fcdata, nwdata, file=paste(project.dir, "Data/dealscan.Rdata", sep="/"))
