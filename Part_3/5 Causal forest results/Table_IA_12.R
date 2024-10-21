rm(list=ls())



##Loads required packages 
require(ggplot2)
require(stargazer)
require(rdd)
#require(installr)
require(rdrobust)
require(datawizard)
require(tidyverse)
require(lmtest)
require(twang)
require(xtable)

### directories
#setwd(..)
project.dir = getwd()
data.dir = paste(project.dir, "Samples",sep="/")
output.dir = paste(project.dir,"output",sep="/")

####Functions are in a separate file (functions.R)
source(paste(project.dir, "functions.R", sep="/"))
load(paste(data.dir, "NWallnoloan.Rdata", sep="/"))
assign("NWallnoloan", tmp, envir = .GlobalEnv)


tmp$slack.final <- tmp[["min_slack win"]]

summary(tmp$slack.final)

data <- subset(tmp, !is.na(tmp$slack.final))

#####################


data <- data %>%
  mutate(invest = data$'Investment/Capital win',
         macro_Q = data$'lag Macro q win',
         size = data$'Log(Assets) win',
         ROA = data$'lag ROA win',
         lag_cash_flow = data$'lag Cash Flow win',
         cash_flow = data$'Cash Flow win',
         lev = data$'lag Leverage win',
         altman_Z = data$'altman win'
         ) %>%
  mutate(in_default = ifelse(slack.final < 0, 1, 0)) %>%
  select(invest,  macro_Q, initial_slack, in_default, size, ROA, cash_flow, lag_cash_flow, slack.final, lev, altman_Z) %>%
  filter(!is.na(invest), !is.na(macro_Q), !is.na(initial_slack), !is.na(in_default), !is.na(size), !is.na(ROA), !is.na(cash_flow),
         !is.na(lev), !is.na(altman_Z), !is.na(lag_cash_flow), !is.na(slack.final))



head(data)
sort(names(data))

data <- data.frame(data)

# random seed for use below
my.seed = 467062


first_ps <- ps(in_default ~ macro_Q  + size + initial_slack + ROA + cash_flow + lag_cash_flow  + altman_Z, data = data)
#Note:  the above will print results to 10,000 before finishing

balance_table <- as.data.frame(bal.table(first_ps, digits = 2)$unw$ks)

rownames(balance_table) <- c("macro Q", "size", "initial_slack", "ROA", "cash_flow", "lag_cash_flow", "altman_Z")

print(xtable(balance_table, align=c("l","c"), digits = c(0,2),
             caption = "Balance test",
             label = "tab:balance"),
      file=paste(output.dir, "Table_IA_12.tex", sep="/"),
      caption.placement = "top",
      size = "\\footnotesize")
