###### Functions ######
##these are functions I commonly use

make.indicator <- function(variable,
                           values = sort(unique(variable))[2:length(unique(variable))]){
  rv <- 1 * (variable == values[1])
  for(i in 2:length(values)){
    rv <- cbind(rv, as.numeric(variable == values[i]))
  }
  rv
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

dropcolumns <- function(df, colname){
  newcolnames <- setdiff(names(df), colname)
  subset(df, select=newcolnames)
}

keepcolumns <- function(df, colname){
  newcolnames <- intersect(names(df), colname)
  subset(df, select=newcolnames)
}

vec.lag <- function(vec, lag=1){
  if(lag == 0){
    vec
  } else if(lag > 0){
    filler <- rep(NA, lag)
    rv <- c(filler, vec)[1:length(vec)]
    rv
  } else {
    lag <- -lag
    filler <- rep(NA, lag)
    rv <- c(vec, filler)
    rv[(lag+1):length(rv)]
  }
}

df.lag <- function(df, lag=1, groups=NULL){
    if(is.null(groups)){
        for(nm in names(df)){
            df[[nm]] <- vec.lag(df[[nm]], lag)
        }
	  df
    } else {
        rv <- df.lag(df, lag, NULL)
        groups <- intersect(groups, names(rv))
        match.matr <- as.matrix(subset(df, select=groups)) == as.matrix(subset(rv, select=groups))
        match.vec <- (apply(match.matr * 1, 1, prod) == 1)
        match.vec <- ifelse(is.na(match.vec), FALSE, match.vec)

        for(nm in names(rv)){
            rv[[nm]] <- ifelse(match.vec, rv[[nm]], NA)
        }
        rv
    }
}

# expects vectors of the same length, returning non-missing
# elements of the first vector, with missing elements filled
# in with subsequent vectors
# 
# print(coalesce(c(1,2,3))) # c(1,2,3)
# print(coalesce(c(1,2,3), c(3,4,5))) # c(1,2,3)
# print(coalesce(c(1, NA, 3), c(6, 2, 3))) # c(1,2,3)
# print(coalesce(c(1, NA, NA), c(NA, 2, NA), c(NA, NA, 3))) # c(1,2,3)
coalesce <- function(...){
   args <- list(...)
   if(length(args) == 1){
      args[[1]]
   } else {
      rv <- args[[1]]
      for(idx in 2:length(args)){
         rv <- ifelse(is.na(rv), args[[idx]], rv)
      }
      rv
   }
}

date.to.yrmo <- function(date){
  12 * as.numeric(format(date, "%Y")) + as.numeric(format(date, "%m"))
}


date.to.yrqtr <- function(date){
	(date.to.yrmo(date) + 2) %/% 3	
}





drop.ambiguous.observations <- function(df, keys, verbose=TRUE){
  dupkeys <- keepcolumns(df, keys)
  allkeys <- unique(dupkeys)
  dupkeys <- dupkeys[duplicated(dupkeys),, drop = FALSE]
  dupkeys <- unique(dupkeys)
  dupkeys[["duplicated_"]] <- 1
  allkeys <- merge(allkeys, dupkeys, all=TRUE)
#print(dim(allkeys))
  nondupkeys <- allkeys[is.na(allkeys[["duplicated_"]]),,drop = FALSE]
#print(dim(nondupkeys))
  if(verbose && nrow(dupkeys)>0){
    for(idx in 1:nrow(dupkeys)){
      print(paste(c("dropping repeated keys:", dupkeys[idx,]), collapse=" "))
    }
  }
#print(dim(nondupkeys))
  rv <- merge(dropcolumns(nondupkeys, "duplicated_"), df)
#print(dim(rv))
  rv
}

# returns df with observations not within the window taken out
# timevar is the time variable name as a string
# window.start and window.end dates are inclusive; I don't care
# whether dates are given as Date or numeric or any other particular
# type, so long as they can be compared with < and >
# e.g. within.window(df, "yearmo", 23800, 24100)

observations.within.window <- function(df, timevar, window.start, window.end){
  df[df[[timevar]] >= window.start & df[[timevar]] <= window.end,]
}

# takes a data frame with information about when securities are added to a
# portfolio and returns a data frame with information about holdings each period
# i.e., if securities are held for 3 months, and timevar is monthly, the output
# data frame has three observations for each observation in the input data frame --
# one for the month the security is added, plus a new observation for each of the
# next two months.  "timevar" should be an integer and consecutive (i.e., if
# holding.period is 3, the new observations will have timevar incremented by 1 and
# by 2)

extend.portfolio.holdings <- function(df, timevar, holding.period){
  do.call(rbind,
          lapply(0:(holding.period-1), function(offset){
            rv <- df
            rv[[timevar]] <- rv[[timevar]] + offset
            rv
          }))
}

trim <- function(string) {
  ifelse(grepl("^\\s*$", string, perl=TRUE),"",
         gsub("^\\s*(.*?)\\s*$","\\1",string,perl=TRUE))
}




censor <- function(vec, range, truncate=FALSE){
  if(truncate){
    ifelse(vec < range[1], NA, ifelse(vec > range[2], NA, vec))
  } else {
    ifelse(is.na(vec),
           NA,
           ifelse(vec < range[1],
                  range[1],
                  ifelse(vec > range[2],
                         range[2],
                         vec)))
  }
}

# if groups isn't NULL, winsorization takes place within each group
winsorize <- function(vec, probs=c(0.05, 0.95), groups=NULL, truncate=FALSE){
  if(is.null(groups)){
    range <- quantile(vec, probs=probs, na.rm=TRUE)    
    censor(vec, range, truncate)
  } else {
    rv <- rep(NA, length(vec))
    groupvals <- unique(groups)
    for(date in groupvals){
      rv[groups == date] <-
        winsorize(vec[groups == date],
                  probs=probs,
                  groups=NULL,
                  truncate=truncate)
    }
    rv
  }
}




addFamaFrenchIndustries <- function(df, sicname="SIC", ffname="FFIC"){
  SIC <- df[[sicname]]

  if(is.character(class(SIC))){
    SIC <- as.numeric(substr(SIC, 1, 4))
  }

  rv <- rep(0, length(df[[sicname]]))

# Consumer NonDurables -- Food, Tobacco, Textiles, Apparel, Leather, Toys
  rv <- rv + 1 * ((SIC >= 0100 & SIC <= 0999) | (SIC >= 2000 & SIC <= 2399) | (SIC >= 2700 & SIC <= 2749) | (SIC >= 2770 & SIC <= 2799) | (SIC >= 3100 & SIC <= 3199) | (SIC >= 3940 & SIC <= 3989))

# Consumer Durables -- Cars, TV's, Furniture, Household Appliances
  rv <- rv + 2 * ((SIC >= 2500 & SIC <= 2519) | (SIC >= 2590 & SIC <= 2599) | (SIC >= 3630 & SIC <= 3659) | (SIC >= 3710 & SIC <= 3711) | (SIC >= 3714 & SIC <= 3714) | (SIC >= 3716 & SIC <= 3716) | (SIC >= 3750 & SIC <= 3751) | (SIC >= 3792 & SIC <= 3792) | (SIC >= 3900 & SIC <= 3939) | (SIC >= 3990 & SIC <= 3999))

# Manufacturing -- Machinery, Trucks, Planes, Off Furn, Paper, Com Printing
  rv <- rv + 3 * ((SIC >= 2520 & SIC <= 2589) | (SIC >= 2600 & SIC <= 2699) | (SIC >= 2750 & SIC <= 2769) | (SIC >= 3000 & SIC <= 3099) | (SIC >= 3200 & SIC <= 3569) | (SIC >= 3580 & SIC <= 3629) | (SIC >= 3700 & SIC <= 3709) | (SIC >= 3712 & SIC <= 3713) | (SIC >= 3715 & SIC <= 3715) | (SIC >= 3717 & SIC <= 3749) | (SIC >= 3752 & SIC <= 3791) | (SIC >= 3793 & SIC <= 3799) | (SIC >= 3830 & SIC <= 3839) | (SIC >= 3860 & SIC <= 3899))

# Oil, Gas, and Coal Extraction and Products
  rv <- rv + 4 * ((SIC >= 1200 & SIC <= 1399) | (SIC >= 2900 & SIC <= 2999))

# Chemicals and Allied Products
  rv <- rv + 5 * ((SIC >= 2800 & SIC <= 2829) | (SIC >= 2840 & SIC <= 2899))

# Business Equipment -- Computers, Software, and Electronic Equipment
  rv <- rv + 6 * ((SIC >= 3570 & SIC <= 3579) | (SIC >= 3660 & SIC <= 3692) | (SIC >= 3694 & SIC <= 3699) | (SIC >= 3810 & SIC <= 3829) | (SIC >= 7370 & SIC <= 7379))

# Telephone and Television Transmission
  rv <- rv + 7 * ((SIC >= 4800 & SIC <= 4899))

# Utilities
  rv <- rv + 8 * ((SIC >= 4900 & SIC <= 4949))

# Wholesale, Retail, and Some Services (Laundries, Repair Shops)
  rv <- rv + 9 * ((SIC >= 5000 & SIC <= 5999) | (SIC >= 7200 & SIC <= 7299) | (SIC >= 7600 & SIC <= 7699))

# Healthcare, Medical Equipment, and Drugs
  rv <- rv + 10 * ((SIC >= 2830 & SIC <= 2839) | (SIC >= 3693 & SIC <= 3693) | (SIC >= 3840 & SIC <= 3859) | (SIC >= 8000 & SIC <= 8099))

# Finance
  rv <- rv + 11 * ((SIC >= 6000 & SIC <= 6999))

  rv <- ifelse(rv == 0, 12, rv)

  df[[ffname]] <- rv
  df
}

