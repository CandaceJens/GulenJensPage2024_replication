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
manip.perc = 20

###### Load Data ######
##5% manip, slope 0.001:
data.pman.2.slope.001 <- read.csv(paste(project.dir, "pmanhetero/pman0.2_slope0.001_10000_obs_sims_alt.csv", sep="/"))

het.spec.2.001 = 0.001
het.perc.2.001 = 0

late.2.001 <- compute_LATE(het.spec.2.001, het.perc.2.001)
late.2.001


data.2.001.ate <- data.pman.2.slope.001 %>% 
  select(forest.ate, ols.ate) %>%
  mutate(forest.diff = (forest.ate - 0.02)/0.02) %>%
  mutate(ols.diff = (ols.ate - 0.02)/0.02) %>%
  select(-forest.ate, -ols.ate)

ate.2.001 <- round(apply(data.2.001.ate, 2, mean, na.rm = TRUE)*100, digits = 3)


data.2.001.late <- data.pman.2.slope.001 %>% 
  select(rd.late, forest.close.ate) %>%
  mutate(forest.diff = (forest.close.ate - as.vector(late.2.001))/as.vector(late.2.001)) %>%
  mutate(rd.diff = (rd.late - as.vector(late.2.001))/as.vector(late.2.001)) %>%
  select(-rd.late, -forest.close.ate)

late.cate.2.001 <- round(apply(data.2.001.late, 2, mean, na.rm = TRUE)*100, digits = 3)

###RMSE:
rmse.forest.ate.2.001 <- round(100 * sqrt(mean((data.pman.2.slope.001$forest.ate - 0.02)^2, na.rm = TRUE)) / 0.02, digits = 2)
rmse.rdd.late.2.001 <- round(100 * sqrt(mean((data.pman.2.slope.001$rd.late - as.vector(late.2.001))^2, na.rm = TRUE)) / late.2.001, digits = 2)
rmse.forest.cate.2.001 <- round(100 * sqrt(mean((data.pman.2.slope.001$forest.close.ate - as.vector(late.2.001))^2, na.rm = TRUE)) / late.2.001, digits = 2)

###For table wrap-up:
spec.details.2.001 <- c( manip.perc, het.spec.2.001,2, late.2.001*100)
spec.details.2.001

cf.ATE.2.001 <- c(ate.2.001[[1]], rmse.forest.ate.2.001)
cf.ATE.2.001

cf.CATE.2.001 <- c(late.cate.2.001[[1]], rmse.forest.cate.2.001)
cf.CATE.2.001

RDD.LATE.2.001 <- c(late.cate.2.001[[2]], rmse.rdd.late.2.001)
RDD.LATE.2.001

row.2.001 <- c(spec.details.2.001, cf.ATE.2.001, cf.CATE.2.001, RDD.LATE.2.001)
row.2.001



###### Load Data ######
##5% manip, slope 0.003:
data.pman.2.slope.003 <- read.csv(paste(project.dir, "pmanhetero/pman0.2_slope0.003_10000_obs_sims_alt.csv", sep="/"))


het.spec.2.003 = 0.003
het.perc.2.003 = 0

late.2.003 <- compute_LATE(het.spec.2.003, het.perc.2.003)
late.2.003


data.2.003.ate <- data.pman.2.slope.003 %>% 
  select(forest.ate, ols.ate) %>%
  mutate(forest.diff = (forest.ate - 0.02)/0.02) %>%
  mutate(ols.diff = (ols.ate - 0.02)/0.02) %>%
  select(-forest.ate, -ols.ate)

ate.2.003 <- round(apply(data.2.003.ate, 2, mean, na.rm = TRUE)*100, digits = 3)


data.2.003.late <- data.pman.2.slope.003 %>% 
  select(rd.late, forest.close.ate) %>%
  mutate(forest.diff = (forest.close.ate - as.vector(late.2.003))/as.vector(late.2.003)) %>%
  mutate(rd.diff = (rd.late - as.vector(late.2.003))/as.vector(late.2.003)) %>%
  select(-rd.late, -forest.close.ate)

late.cate.2.003 <- round(apply(data.2.003.late, 2, mean, na.rm = TRUE)*100, digits = 3)

###RMSE:
rmse.forest.ate.2.003 <- round(100 * sqrt(mean((data.pman.2.slope.003$forest.ate - 0.02)^2, na.rm = TRUE)) / 0.02, digits = 2)
rmse.rdd.late.2.003 <- round(100 * sqrt(mean((data.pman.2.slope.003$rd.late - as.vector(late.2.003))^2, na.rm = TRUE)) / late.2.003, digits = 2)
rmse.forest.cate.2.003 <- round(100 * sqrt(mean((data.pman.2.slope.003$forest.close.ate - as.vector(late.2.003))^2, na.rm = TRUE)) / late.2.003, digits = 2)

###For table wrap-up:
spec.details.2.003 <- c(manip.perc, het.spec.2.003, 2, late.2.003*100)
spec.details.2.003

cf.ATE.2.003 <- c(ate.2.003[[1]], rmse.forest.ate.2.003)
cf.ATE.2.003

cf.CATE.2.003 <- c(late.cate.2.003[[1]], rmse.forest.cate.2.003)
cf.CATE.2.003

RDD.LATE.2.003 <- c(late.cate.2.003[[2]], rmse.rdd.late.2.003)
RDD.LATE.2.003

row.2.003 <- c(spec.details.2.003, cf.ATE.2.003, cf.CATE.2.003, RDD.LATE.2.003)
row.2.003




###### Load Data ######
##5% manip, slope 0.005:
data.pman.2.slope.005 <- read.csv(paste(project.dir, "pmanhetero/pman0.2_slope0.005_10000_obs_sims_alt.csv", sep="/"))


het.spec.2.005 = 0.005
het.perc.2.005 = 0

late.2.005 <- compute_LATE(het.spec.2.005, het.perc.2.005)
late.2.005


data.2.005.ate <- data.pman.2.slope.005 %>% 
  select(forest.ate, ols.ate) %>%
  mutate(forest.diff = (forest.ate - 0.02)/0.02) %>%
  mutate(ols.diff = (ols.ate - 0.02)/0.02) %>%
  select(-forest.ate, -ols.ate)

ate.2.005 <- round(apply(data.2.005.ate, 2, mean, na.rm = TRUE)*100, digits = 3)


data.2.005.late <- data.pman.2.slope.005 %>% 
  select(rd.late, forest.close.ate) %>%
  mutate(forest.diff = (forest.close.ate - as.vector(late.2.005))/as.vector(late.2.005)) %>%
  mutate(rd.diff = (rd.late - as.vector(late.2.005))/as.vector(late.2.005)) %>%
  select(-rd.late, -forest.close.ate)

late.cate.2.005 <- round(apply(data.2.005.late, 2, mean, na.rm = TRUE)*100, digits = 3)

###RMSE:
rmse.forest.ate.2.005 <- round(100 * sqrt(mean((data.pman.2.slope.005$forest.ate - 0.02)^2, na.rm = TRUE)) / 0.02, digits = 2)
rmse.rdd.late.2.005 <- round(100 * sqrt(mean((data.pman.2.slope.005$rd.late - as.vector(late.2.005))^2, na.rm = TRUE)) / late.2.005, digits = 2)
rmse.forest.cate.2.005 <- round(100 * sqrt(mean((data.pman.2.slope.005$forest.close.ate - as.vector(late.2.005))^2, na.rm = TRUE)) / late.2.005, digits = 2)

###For table wrap-up:
spec.details.2.005 <- c(manip.perc,het.spec.2.005,  2, late.2.005*100)
spec.details.2.005

cf.ATE.2.005 <- c(ate.2.005[[1]], rmse.forest.ate.2.005)
cf.ATE.2.005

cf.CATE.2.005 <- c(late.cate.2.005[[1]], rmse.forest.cate.2.005)
cf.CATE.2.005

RDD.LATE.2.005 <- c(late.cate.2.005[[2]], rmse.rdd.late.2.005)
RDD.LATE.2.005

row.2.005 <- c(spec.details.2.005, cf.ATE.2.005, cf.CATE.2.005, RDD.LATE.2.005)
row.2.005





###### Load Data ######
##5% manip, slope 0.007:
data.pman.2.slope.007 <- read.csv(paste(project.dir, "pmanhetero/pman0.2_slope0.007_10000_obs_sims_alt.csv", sep="/"))


het.spec.2.007 = 0.007
het.perc.2.007 = 0

late.2.007 <- compute_LATE(het.spec.2.007, het.perc.2.007)
late.2.007


data.2.007.ate <- data.pman.2.slope.007 %>% 
  select(forest.ate, ols.ate) %>%
  mutate(forest.diff = (forest.ate - 0.02)/0.02) %>%
  mutate(ols.diff = (ols.ate - 0.02)/0.02) %>%
  select(-forest.ate, -ols.ate)

ate.2.007 <- round(apply(data.2.007.ate, 2, mean, na.rm = TRUE)*100, digits = 3)


data.2.007.late <- data.pman.2.slope.007 %>% 
  select(rd.late, forest.close.ate) %>%
  mutate(forest.diff = (forest.close.ate - as.vector(late.2.007))/as.vector(late.2.007)) %>%
  mutate(rd.diff = (rd.late - as.vector(late.2.007))/as.vector(late.2.007)) %>%
  select(-rd.late, -forest.close.ate)

late.cate.2.007 <- round(apply(data.2.007.late, 2, mean, na.rm = TRUE)*100, digits = 3)

###RMSE:
rmse.forest.ate.2.007 <- round(100 * sqrt(mean((data.pman.2.slope.007$forest.ate - 0.02)^2, na.rm = TRUE)) / 0.02, digits = 2)
rmse.rdd.late.2.007 <- round(100 * sqrt(mean((data.pman.2.slope.007$rd.late - as.vector(late.2.007))^2, na.rm = TRUE)) / late.2.007, digits = 2)
rmse.forest.cate.2.007 <- round(100 * sqrt(mean((data.pman.2.slope.007$forest.close.ate - as.vector(late.2.007))^2, na.rm = TRUE)) / late.2.007, digits = 2)

###For table wrap-up:
spec.details.2.007 <- c(manip.perc, het.spec.2.007, 2, late.2.007*100)
spec.details.2.007

cf.ATE.2.007 <- c(ate.2.007[[1]], rmse.forest.ate.2.007)
cf.ATE.2.007

cf.CATE.2.007 <- c(late.cate.2.007[[1]], rmse.forest.cate.2.007)
cf.CATE.2.007

RDD.LATE.2.007 <- c(late.cate.2.007[[2]], rmse.rdd.late.2.007)
RDD.LATE.2.007

row.2.007 <- c(spec.details.2.007, cf.ATE.2.007, cf.CATE.2.007, RDD.LATE.2.007)
row.2.007


###########
###Wrap this up as a Panel:
final.table <- t(cbind(row.2.001,
                       row.2.003,
                       row.2.005,
                       row.2.007
))

##manip percent, slope, ATE, LATE, cf LATE bias, cf RMSE, cf CATE bias, cf CATE rmse, RDD LATE bias, RDD RMSE
print(xtable(final.table,digits=c(3, 0, 3,  0, 3, 3, 2, 3, 2, 3, 2)), include.rownames = FALSE,
      file = "output/Table_2_Panel_C_part3.txt")


#20 & 0.001 & 2 & 1.965 & 0.335 & 18.23 & -0.677 & 32.74 & -43.217 & 51.32 \\ 
#20 & 0.003 & 2 & 1.895 & 0.113 & 18.16 & 1.935 & 34.10 & -42.819 & 51.43 \\ 
#20 & 0.005 & 2 & 1.825 & -0.137 & 18.29 & 4.500 & 35.44 & -42.396 & 51.59 \\ 
#20 & 0.007 & 2 & 1.755 & -0.461 & 18.15 & 6.407 & 36.90 & -41.950 & 51.81 \\ 

