rm(list=ls())


##Loads required packages 
require(ggplot2)
require(stargazer)
require(lmtest)
require(rdd)
require(tidyverse)
require(dplyr)
require(xtable)

# which dataset?
data.set = "NWallnoloan"

### directories
#setwd(..)
project.dir = getwd()
data.dir = paste(project.dir, "Samples",sep="/")
output.dir = paste(project.dir,"output",sep="/")

####Functions are in a separate file (functions.R)
source(paste(project.dir, "functions.R", sep="/"))
load(paste(data.dir, "NWallnoloan.Rdata", sep="/"))
assign("NWallnoloan", tmp, envir = .GlobalEnv)

# only presenting summary statistics for the All/No loan samples
networth.data <- NWallnoloan

make_column <- function(this.tibble){
  this.tibble <- this.tibble %>%
    select(`Current Ratio`,
           `Net Worth`,
           `Tangible Net Worth`,
           `Log(Assets) win`,
           `Market-to-Book`,
           `Macro q win`,
           `ROA win`,
           `Capital/Assets win`,
           `Investment/Capital win`,
           `Cash Flow win`,
           `Leverage win`)
  
  means <- as.data.frame(lapply(this.tibble, mean, na.rm=TRUE))
  ses <- as.data.frame(lapply(this.tibble,
             function(myvec){
	       myvec <- na.omit(myvec)
	       sd(myvec) / sqrt(length(myvec))
	     }))
  medians <- as.data.frame(lapply(this.tibble, median, na.rm=TRUE))
  rv <- rbind(means, ses, medians)
  rownames(rv) <- c("mean", "se", "median")
  rv
}

print(t(round(make_column(networth.data),2)))
dim(networth.data)
length(unique(networth.data$gvkey))

print(xtable(t(round(make_column(networth.data),2)), align=c("l","c","c","c"), digits = c(0,2,2,2),
             caption = paste("Summary statistics using data: ", data.set, sep=""),
             label = "tab:sumstats"),
      file=paste(output.dir, "Table_IA_5.tex", sep="/"),
      caption.placement = "top",
      size = "\\footnotesize")
