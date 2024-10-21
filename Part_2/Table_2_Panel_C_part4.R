rm(list=ls())


##Loads required packages 
require(tidyverse)
library(xtable)

project.dir <- "."

OUTPUT_DIRECTORY <- "output"


###########LATE function:
compute_LATE <- function(het.slope = 0.0, zero.te.percentile = 0.0) {
  mu = as.matrix(c(1.4,5.5,0.1,0.2,0.3)) # variable means
  sig = as.matrix(c(1.00, 1.50, 0.30, 0.30, 0.80)) # variable standard deviations
  rho.mat = matrix(c(  1.00,-0.05,-0.30, 0.15, 0.00,
                       -0.05, 1.00, 0.20, 0.35, 0.00,
                       -0.30, 0.20, 1.00, 0.50, 0.00,
                       0.15, 0.35, 0.50, 1.00, 0.00,
                       0.00, 0.00, 0.00, 0.00, 1.00),
                   5,5) # correlation matrix
  sigma = rho.mat * (sig %*% t(sig)) # covariance matrix
  
  coeffs = as.matrix(c(0.05, -0.005, 0.01, 0.02, # linear coefficients in determining the y variable
                       0.025, -0.01, 0.015)) # 2d order coefficients in determining the y variable
  y.sig = 0.065 # standard deviation of y error
  te = 0.02
  
  # find appropriate cutoff for data in meeting zero.te.percentile and the impact on mu_2|x3>cutoff
  x3_cutoff <- ifelse(zero.te.percentile == 0, mu[3] - 15 * sig[3],
                      ifelse(zero.te.percentile == 1, mu[3] + 15 * sig[3],
                             qnorm(p = zero.te.percentile, mean = mu[3], sd = sig[3])))
  alph <- (x3_cutoff - mu[3]) / sig[3]
  mu_2_3 <- mu[2] + sigma[2,3] / sig[3] * dnorm(alph) / (1 - pnorm(alph))
  te_cond <- te / (1 - zero.te.percentile)
  
  ## what is the LATE?
  sig_3_d <- sqrt(sig[3]^2 - sigma[3,4]^2 / sigma[4,4])
  mu_3_d <- mu[3] - sigma[3,4] / sigma[4,4] * mu[4]
  mu_3_3d <- sig_3_d * dnorm((x3_cutoff - mu_3_d) / sig_3_d) / (1 - pnorm((x3_cutoff - mu_3_d) / sig_3_d))
  mu_2_3d <- mu[2] + sigma[2,3:4] %*% solve(sigma[3:4,3:4]) %*% matrix(c(mu_3_3d - mu[3], -mu[4]))
  P_3_d <- pnorm(-x3_cutoff / sig_3_d)
  P_3_d * (te_cond + het.slope * (mu_2_3d - mu_2_3))
  
}


###For the whole panel:
manip.perc = 40

###### Load Data ######
##5% manip, slope 0.001:
data.pman.4.slope.001 <- read.csv(paste(project.dir, "pmanhetero/pman0.4_slope0.001_10000_obs_sims_alt.csv", sep="/"))

het.spec.4.001 = 0.001
het.perc.4.001 = 0

late.4.001 <- compute_LATE(het.spec.4.001, het.perc.4.001)
late.4.001


data.4.001.ate <- data.pman.4.slope.001 %>% 
  select(forest.ate, ols.ate) %>%
  mutate(forest.diff = (forest.ate - 0.02)/0.02) %>%
  mutate(ols.diff = (ols.ate - 0.02)/0.02) %>%
  select(-forest.ate, -ols.ate)

ate.4.001 <- round(apply(data.4.001.ate, 2, mean, na.rm = TRUE)*100, digits = 3)


data.4.001.late <- data.pman.4.slope.001 %>% 
  select(rd.late, forest.close.ate) %>%
  mutate(forest.diff = (forest.close.ate - as.vector(late.4.001))/as.vector(late.4.001)) %>%
  mutate(rd.diff = (rd.late - as.vector(late.4.001))/as.vector(late.4.001)) %>%
  select(-rd.late, -forest.close.ate)

late.cate.4.001 <- round(apply(data.4.001.late, 2, mean, na.rm = TRUE)*100, digits = 3)

###RMSE:
rmse.forest.ate.4.001 <- round(100 * sqrt(mean((data.pman.4.slope.001$forest.ate - 0.02)^2, na.rm = TRUE)) / 0.02, digits = 2)
rmse.rdd.late.4.001 <- round(100 * sqrt(mean((data.pman.4.slope.001$rd.late - as.vector(late.4.001))^2, na.rm = TRUE)) / late.4.001, digits = 2)
rmse.forest.cate.4.001 <- round(100 * sqrt(mean((data.pman.4.slope.001$forest.close.ate - as.vector(late.4.001))^2, na.rm = TRUE)) / late.4.001, digits = 2)

###For table wrap-up:
spec.details.4.001 <- c( manip.perc, het.spec.4.001,2, late.4.001*100)
spec.details.4.001

cf.ATE.4.001 <- c(ate.4.001[[1]], rmse.forest.ate.4.001)
cf.ATE.4.001

cf.CATE.4.001 <- c(late.cate.4.001[[1]], rmse.forest.cate.4.001)
cf.CATE.4.001

RDD.LATE.4.001 <- c(late.cate.4.001[[2]], rmse.rdd.late.4.001)
RDD.LATE.4.001

row.4.001 <- c(spec.details.4.001, cf.ATE.4.001, cf.CATE.4.001, RDD.LATE.4.001)
row.4.001



###### Load Data ######
##5% manip, slope 0.003:
data.pman.4.slope.003 <- read.csv(paste(project.dir, "pmanhetero/pman0.4_slope0.003_10000_obs_sims_alt.csv", sep="/"))


het.spec.4.003 = 0.003
het.perc.4.003 = 0

late.4.003 <- compute_LATE(het.spec.4.003, het.perc.4.003)
late.4.003


data.4.003.ate <- data.pman.4.slope.003 %>% 
  select(forest.ate, ols.ate) %>%
  mutate(forest.diff = (forest.ate - 0.02)/0.02) %>%
  mutate(ols.diff = (ols.ate - 0.02)/0.02) %>%
  select(-forest.ate, -ols.ate)

ate.4.003 <- round(apply(data.4.003.ate, 2, mean, na.rm = TRUE)*100, digits = 3)


data.4.003.late <- data.pman.4.slope.003 %>% 
  select(rd.late, forest.close.ate) %>%
  mutate(forest.diff = (forest.close.ate - as.vector(late.4.003))/as.vector(late.4.003)) %>%
  mutate(rd.diff = (rd.late - as.vector(late.4.003))/as.vector(late.4.003)) %>%
  select(-rd.late, -forest.close.ate)

late.cate.4.003 <- round(apply(data.4.003.late, 2, mean, na.rm = TRUE)*100, digits = 3)

###RMSE:
rmse.forest.ate.4.003 <- round(100 * sqrt(mean((data.pman.4.slope.003$forest.ate - 0.02)^2, na.rm = TRUE)) / 0.02, digits = 2)
rmse.rdd.late.4.003 <- round(100 * sqrt(mean((data.pman.4.slope.003$rd.late - as.vector(late.4.003))^2, na.rm = TRUE)) / late.4.003, digits = 2)
rmse.forest.cate.4.003 <- round(100 * sqrt(mean((data.pman.4.slope.003$forest.close.ate - as.vector(late.4.003))^2, na.rm = TRUE)) / late.4.003, digits = 2)

###For table wrap-up:
spec.details.4.003 <- c(manip.perc, het.spec.4.003, 2, late.4.003*100)
spec.details.4.003

cf.ATE.4.003 <- c(ate.4.003[[1]], rmse.forest.ate.4.003)
cf.ATE.4.003

cf.CATE.4.003 <- c(late.cate.4.003[[1]], rmse.forest.cate.4.003)
cf.CATE.4.003

RDD.LATE.4.003 <- c(late.cate.4.003[[2]], rmse.rdd.late.4.003)
RDD.LATE.4.003

row.4.003 <- c(spec.details.4.003, cf.ATE.4.003, cf.CATE.4.003, RDD.LATE.4.003)
row.4.003




###### Load Data ######
##5% manip, slope 0.005:
data.pman.4.slope.005 <- read.csv(paste(project.dir, "pmanhetero/pman0.4_slope0.005_10000_obs_sims_alt.csv", sep="/"))


het.spec.4.005 = 0.005
het.perc.4.005 = 0

late.4.005 <- compute_LATE(het.spec.4.005, het.perc.4.005)
late.4.005


data.4.005.ate <- data.pman.4.slope.005 %>% 
  select(forest.ate, ols.ate) %>%
  mutate(forest.diff = (forest.ate - 0.02)/0.02) %>%
  mutate(ols.diff = (ols.ate - 0.02)/0.02) %>%
  select(-forest.ate, -ols.ate)

ate.4.005 <- round(apply(data.4.005.ate, 2, mean, na.rm = TRUE)*100, digits = 3)


data.4.005.late <- data.pman.4.slope.005 %>% 
  select(rd.late, forest.close.ate) %>%
  mutate(forest.diff = (forest.close.ate - as.vector(late.4.005))/as.vector(late.4.005)) %>%
  mutate(rd.diff = (rd.late - as.vector(late.4.005))/as.vector(late.4.005)) %>%
  select(-rd.late, -forest.close.ate)

late.cate.4.005 <- round(apply(data.4.005.late, 2, mean, na.rm = TRUE)*100, digits = 3)

###RMSE:
rmse.forest.ate.4.005 <- round(100 * sqrt(mean((data.pman.4.slope.005$forest.ate - 0.02)^2, na.rm = TRUE)) / 0.02, digits = 2)
rmse.rdd.late.4.005 <- round(100 * sqrt(mean((data.pman.4.slope.005$rd.late - as.vector(late.4.005))^2, na.rm = TRUE)) / late.4.005, digits = 2)
rmse.forest.cate.4.005 <- round(100 * sqrt(mean((data.pman.4.slope.005$forest.close.ate - as.vector(late.4.005))^2, na.rm = TRUE)) / late.4.005, digits = 2)

###For table wrap-up:
spec.details.4.005 <- c(manip.perc,het.spec.4.005,  2, late.4.005*100)
spec.details.4.005

cf.ATE.4.005 <- c(ate.4.005[[1]], rmse.forest.ate.4.005)
cf.ATE.4.005

cf.CATE.4.005 <- c(late.cate.4.005[[1]], rmse.forest.cate.4.005)
cf.CATE.4.005

RDD.LATE.4.005 <- c(late.cate.4.005[[2]], rmse.rdd.late.4.005)
RDD.LATE.4.005

row.4.005 <- c(spec.details.4.005, cf.ATE.4.005, cf.CATE.4.005, RDD.LATE.4.005)
row.4.005





###### Load Data ######
##5% manip, slope 0.007:
data.pman.4.slope.007 <- read.csv(paste(project.dir, "pmanhetero/pman0.4_slope0.007_10000_obs_sims_alt.csv", sep="/"))


het.spec.4.007 = 0.007
het.perc.4.007 = 0

late.4.007 <- compute_LATE(het.spec.4.007, het.perc.4.007)
late.4.007


data.4.007.ate <- data.pman.4.slope.007 %>% 
  select(forest.ate, ols.ate) %>%
  mutate(forest.diff = (forest.ate - 0.02)/0.02) %>%
  mutate(ols.diff = (ols.ate - 0.02)/0.02) %>%
  select(-forest.ate, -ols.ate)

ate.4.007 <- round(apply(data.4.007.ate, 2, mean, na.rm = TRUE)*100, digits = 3)


data.4.007.late <- data.pman.4.slope.007 %>% 
  select(rd.late, forest.close.ate) %>%
  mutate(forest.diff = (forest.close.ate - as.vector(late.4.007))/as.vector(late.4.007)) %>%
  mutate(rd.diff = (rd.late - as.vector(late.4.007))/as.vector(late.4.007)) %>%
  select(-rd.late, -forest.close.ate)

late.cate.4.007 <- round(apply(data.4.007.late, 2, mean, na.rm = TRUE)*100, digits = 3)

###RMSE:
rmse.forest.ate.4.007 <- round(100 * sqrt(mean((data.pman.4.slope.007$forest.ate - 0.02)^2, na.rm = TRUE)) / 0.02, digits = 2)
rmse.rdd.late.4.007 <- round(100 * sqrt(mean((data.pman.4.slope.007$rd.late - as.vector(late.4.007))^2, na.rm = TRUE)) / late.4.007, digits = 2)
rmse.forest.cate.4.007 <- round(100 * sqrt(mean((data.pman.4.slope.007$forest.close.ate - as.vector(late.4.007))^2, na.rm = TRUE)) / late.4.007, digits = 2)

###For table wrap-up:
spec.details.4.007 <- c(manip.perc, het.spec.4.007, 2, late.4.007*100)
spec.details.4.007

cf.ATE.4.007 <- c(ate.4.007[[1]], rmse.forest.ate.4.007)
cf.ATE.4.007

cf.CATE.4.007 <- c(late.cate.4.007[[1]], rmse.forest.cate.4.007)
cf.CATE.4.007

RDD.LATE.4.007 <- c(late.cate.4.007[[2]], rmse.rdd.late.4.007)
RDD.LATE.4.007

row.4.007 <- c(spec.details.4.007, cf.ATE.4.007, cf.CATE.4.007, RDD.LATE.4.007)
row.4.007


###########
###Wrap this up as a Panel:
final.table <- t(cbind(row.4.001,
                       row.4.003,
                       row.4.005,
                       row.4.007
))

##manip percent, slope, ATE, LATE, cf LATE bias, cf RMSE, cf CATE bias, cf CATE rmse, RDD LATE bias, RDD RMSE
print(xtable(final.table,digits=c(3, 0, 3,  0, 3, 3, 2, 3, 2, 3, 2)), include.rownames = FALSE,
      file = "output/Table_2_Panel_C_part4.txt")


#40 & 0.001 & 2 & 1.965 & 0.890 & 18.37 & -0.595 & 35.70 & -62.753 & 69.79 \\ 
#40 & 0.003 & 2 & 1.895 & 0.690 & 18.02 & 2.288 & 35.93 & -62.172 & 69.69 \\ 
#40 & 0.005 & 2 & 1.825 & 0.448 & 18.14 & 4.806 & 37.62 & -61.561 & 69.62 \\ 
#40 & 0.007 & 2 & 1.755 & 0.129 & 18.31 & 6.718 & 39.61 & -60.945 & 69.59 \\ 
