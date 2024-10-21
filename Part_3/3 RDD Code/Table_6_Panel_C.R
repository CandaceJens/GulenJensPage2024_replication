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
require(xtable)

### directories
project.dir = getwd()
data.dir = paste(project.dir, "Samples",sep="/")

####Functions are in a separate file (functions.R)
source(paste(project.dir, "functions.R", sep="/"))


# load in net worth sample
load(paste(data.dir, "NWallnoloan.Rdata", sep="/"))
assign("NWallnoloan", tmp, envir = .GlobalEnv)

data <- NWallnoloan[!is.na(NWallnoloan[["Investment/Capital win"]]) & !is.na(NWallnoloan[["slack_ratio win"]]),]
data$slack_ratio_win <- data[["slack_ratio win"]]


################
###Subsample definition:
data <- subset(data, data$`Cash Flow win` > -0.3)
data <- subset(data, data$`altman win` > 0)
data <- data %>%
  filter(slack_ratio_win < 1 & slack_ratio_win > -1)

# scale up investment by 100
data[["Investment/Capital win"]] <- 100 * data[["Investment/Capital win"]]

##Note:  flipping the slack ratio negative for consistency with bind.  
data$neg_slack_ratio <- -data$slack_ratio_win 

##Note:  cutoff is 0:
###This is the p-value for the table:
density.test <- DCdensity(data$slack_ratio_win, cutpoint = 0, ext.out = TRUE)
density.test$p


title(xlab = 'Forcing variable (as ratio)', ylab = 'Density', main = 'McCrary density test \n high ability to manipulate')

dev.print(pdf, "output/Figure_IA_6_Panel_B.pdf")



#################
#################

##RDD
# note that bind = 1 when slack is negative, so use negative slack ratio to correct estimate sign
RDD.all <- RDestimate(`Investment/Capital win` ~ neg_slack_ratio, cutpoint = 0, data = data)
RDD.all
summary(RDD.all)

#########################
#########################
############Using RDDrobust:

####vary p = 1, 2, 3, 4 for polynomials...
RDrobust.results.1 <- rdrobust(data$`Investment/Capital win`, data$neg_slack_ratio, 0, p = 1)

summary(RDrobust.results.1)

###For more decimals:
RDrobust.results.1$coef
RDrobust.results.1$se

###Please report in table:
###p = 2
RDrobust.results.2 <- rdrobust(data$`Investment/Capital win`, data$neg_slack_ratio, 0, p = 2)

summary(RDrobust.results.2)

###For more decimals:
RDrobust.results.2$coef
RDrobust.results.2$se


###p = 3
RDrobust.results.3 <- rdrobust(data$`Investment/Capital win`, data$neg_slack_ratio, 0, p = 3)

summary(RDrobust.results.3)

###For more decimals:
RDrobust.results.3$coef
RDrobust.results.3$se

###p = 4
RDrobust.results.4 <- rdrobust(data$`Investment/Capital win`, data$neg_slack_ratio, 0, p = 4)

summary(RDrobust.results.4)

###For more decimals:
RDrobust.results.4$coef
RDrobust.results.4$se

# make table -- note that treatment is defined by slack < 0, so the above RDD functions estimate
# the LATE with the incorrect sign, thus we flip the sign in producing the below table.
RDD.obs = c(sum(between(data$neg_slack_ratio, -RDD.all$bw[1], 0)),sum(between(data$neg_slack_ratio, 0, RDD.all$bw[1])))

RDD_panel <- data.frame(Model = c("IK Bandwidth", "Linear", "Quadratic", "Cubic", "Quartic"),
                        Coefficient = round(c(RDD.all$est[1],RDrobust.results.1$coef[1],RDrobust.results.2$coef[1],RDrobust.results.3$coef[1],RDrobust.results.4$coef[1]),4),
                        Std.Err = paste0("(",round(c(RDD.all$se[1],RDrobust.results.1$se[1],RDrobust.results.2$se[1],RDrobust.results.3$se[1],RDrobust.results.4$se[1]),4),")"),
                        Bandwidth = round(c(RDD.all$bw[1],RDrobust.results.1$bws[1],RDrobust.results.2$bws[1],RDrobust.results.3$bws[1],RDrobust.results.4$bws[1]),4),
                        Obs.Below = c(RDD.obs[1],RDrobust.results.1$N_h[2],RDrobust.results.2$N_h[2],RDrobust.results.3$N_h[2],RDrobust.results.4$N_h[2]),
                        Obs.Above = c(RDD.obs[2],RDrobust.results.1$N_h[1],RDrobust.results.2$N_h[1],RDrobust.results.3$N_h[1],RDrobust.results.4$N_h[1]))
RDD_panel

print(xtable(RDD_panel,digits=c(0, 2, 2, 2, 4, 0, 0)), include.rownames = FALSE,
      file = "output/Table_6_Panel_C.txt")

###One more time so this doesn't get lost:
###This is the p-value for the table:
density.test$p
