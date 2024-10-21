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
manip.perc = 0

###### Load Data ######
##0% manip, slope 0.001:
data.pman.05.slope.001 <- read.csv(paste(project.dir, "hetero/pman0_slope1_10000_obs_ps13_sims_alt.csv", sep="/"))


het.spec.05.001 = 0.001
het.perc.05.001 = 0

late.05.001 <- compute_LATE(het.spec.05.001, het.perc.05.001)
late.05.001


data.05.001.ate <- data.pman.05.slope.001 %>% 
  select(forest.ate, ols.ate) %>%
  mutate(forest.diff = (forest.ate - 0.02)/0.02) %>%
  mutate(ols.diff = (ols.ate - 0.02)/0.02) %>%
  select(-forest.ate, -ols.ate)

ate.05.001 <- round(apply(data.05.001.ate, 2, mean, na.rm = TRUE)*100, digits = 3)


data.05.001.late <- data.pman.05.slope.001 %>% 
  select(rd.late, forest.close.ate) %>%
  mutate(forest.diff = (forest.close.ate - as.vector(late.05.001))/as.vector(late.05.001)) %>%
  mutate(rd.diff = (rd.late - as.vector(late.05.001))/as.vector(late.05.001)) %>%
  select(-rd.late, -forest.close.ate)

late.cate.05.001 <- round(apply(data.05.001.late, 2, mean, na.rm = TRUE)*100, digits = 3)

###RMSE:
rmse.forest.ate.05.001 <- round(100 * sqrt(mean((data.pman.05.slope.001$forest.ate - 0.02)^2, na.rm = TRUE)) / 0.02, digits = 2)
rmse.rdd.late.05.001 <- round(100 * sqrt(mean((data.pman.05.slope.001$rd.late - as.vector(late.05.001))^2, na.rm = TRUE)) / late.05.001, digits = 2)
rmse.forest.cate.05.001 <- round(100 * sqrt(mean((data.pman.05.slope.001$forest.close.ate - as.vector(late.05.001))^2, na.rm = TRUE)) / late.05.001, digits = 2)

###For table wrap-up:
spec.details.05.001 <- c(0, het.spec.05.001,2, late.05.001*100)
spec.details.05.001

cf.ATE.05.001 <- c(ate.05.001[[1]], rmse.forest.ate.05.001)
cf.ATE.05.001

cf.CATE.05.001 <- c(late.cate.05.001[[1]], rmse.forest.cate.05.001)
cf.CATE.05.001

RDD.LATE.05.001 <- c(late.cate.05.001[[2]], rmse.rdd.late.05.001)
RDD.LATE.05.001

row.05.001 <- c(spec.details.05.001, cf.ATE.05.001, cf.CATE.05.001, RDD.LATE.05.001)
row.05.001





###### Load Data ######
##0% manip, slope 0.003:
data.pman.0.slope.003 <- read.csv(paste(project.dir, "hetero/pman0_slope3_10000_obs_ps13_sims_alt.csv", sep="/"))


het.spec.05.003 = 0.003
het.perc.05.003 = 0

late.05.003 <- compute_LATE(het.spec.05.003, het.perc.05.003)
late.05.003


data.05.003.ate <- data.pman.0.slope.003 %>% 
  select(forest.ate, ols.ate) %>%
  mutate(forest.diff = (forest.ate - 0.02)/0.02) %>%
  mutate(ols.diff = (ols.ate - 0.02)/0.02) %>%
  select(-forest.ate, -ols.ate)

ate.05.003 <- round(apply(data.05.003.ate, 2, mean, na.rm = TRUE)*100, digits = 3)


data.05.003.late <- data.pman.0.slope.003 %>% 
  select(rd.late, forest.close.ate) %>%
  mutate(forest.diff = (forest.close.ate - as.vector(late.05.003))/as.vector(late.05.003)) %>%
  mutate(rd.diff = (rd.late - as.vector(late.05.003))/as.vector(late.05.003)) %>%
  select(-rd.late, -forest.close.ate)

late.cate.05.003 <- round(apply(data.05.003.late, 2, mean, na.rm = TRUE)*100, digits = 3)

###RMSE:
rmse.forest.ate.05.003 <- round(100 * sqrt(mean((data.pman.0.slope.003$forest.ate - 0.02)^2, na.rm = TRUE)) / 0.02, digits = 2)
rmse.rdd.late.05.003 <- round(100 * sqrt(mean((data.pman.0.slope.003$rd.late - as.vector(late.05.003))^2, na.rm = TRUE)) / late.05.003, digits = 2)
rmse.forest.cate.05.003 <- round(100 * sqrt(mean((data.pman.0.slope.003$forest.close.ate - as.vector(late.05.003))^2, na.rm = TRUE)) / late.05.003, digits = 2)

###For table wrap-up:
spec.details.05.003 <- c(0, het.spec.05.003,2, late.05.003*100)
spec.details.05.003

cf.ATE.05.003 <- c(ate.05.003[[1]], rmse.forest.ate.05.003)
cf.ATE.05.003

cf.CATE.05.003 <- c(late.cate.05.003[[1]], rmse.forest.cate.05.003)
cf.CATE.05.003

RDD.LATE.05.003 <- c(late.cate.05.003[[2]], rmse.rdd.late.05.003)
RDD.LATE.05.003

row.05.003 <- c(spec.details.05.003, cf.ATE.05.003, cf.CATE.05.003, RDD.LATE.05.003)
row.05.003


##0% manip, slope 0.005:
data.pman.0.slope.005 <- read.csv(paste(project.dir, "hetero/pman0_slope5_10000_obs_ps13_sims_alt.csv", sep="/"))


het.spec.05.005 = 0.005
het.perc.05.005 = 0

late.05.005 <- compute_LATE(het.spec.05.005, het.perc.05.005)
late.05.005


data.05.005.ate <- data.pman.0.slope.005 %>% 
  select(forest.ate, ols.ate) %>%
  mutate(forest.diff = (forest.ate - 0.02)/0.02) %>%
  mutate(ols.diff = (ols.ate - 0.02)/0.02) %>%
  select(-forest.ate, -ols.ate)

ate.05.005 <- round(apply(data.05.005.ate, 2, mean, na.rm = TRUE)*100, digits = 3)


data.05.005.late <- data.pman.0.slope.005 %>% 
  select(rd.late, forest.close.ate) %>%
  mutate(forest.diff = (forest.close.ate - as.vector(late.05.005))/as.vector(late.05.005)) %>%
  mutate(rd.diff = (rd.late - as.vector(late.05.005))/as.vector(late.05.005)) %>%
  select(-rd.late, -forest.close.ate)

late.cate.05.005 <- round(apply(data.05.005.late, 2, mean, na.rm = TRUE)*100, digits = 3)

###RMSE:
rmse.forest.ate.05.005 <- round(100 * sqrt(mean((data.pman.0.slope.005$forest.ate - 0.02)^2, na.rm = TRUE)) / 0.02, digits = 2)
rmse.rdd.late.05.005 <- round(100 * sqrt(mean((data.pman.0.slope.005$rd.late - as.vector(late.05.005))^2, na.rm = TRUE)) / late.05.005, digits = 2)
rmse.forest.cate.05.005 <- round(100 * sqrt(mean((data.pman.0.slope.005$forest.close.ate - as.vector(late.05.005))^2, na.rm = TRUE)) / late.05.005, digits = 2)

###For table wrap-up:
spec.details.05.005 <- c(0, het.spec.05.005,2, late.05.005*100)
spec.details.05.005

cf.ATE.05.005 <- c(ate.05.005[[1]], rmse.forest.ate.05.005)
cf.ATE.05.005

cf.CATE.05.005 <- c(late.cate.05.005[[1]], rmse.forest.cate.05.005)
cf.CATE.05.005

RDD.LATE.05.005 <- c(late.cate.05.005[[2]], rmse.rdd.late.05.005)
RDD.LATE.05.005

row.05.005 <- c(spec.details.05.005, cf.ATE.05.005, cf.CATE.05.005, RDD.LATE.05.005)
row.05.005




##0% manip, slope 0.007:
data.pman.0.slope.007 <- read.csv(paste(project.dir, "hetero/pman0_slope7_10000_obs_ps13_sims_alt.csv", sep="/"))


het.spec.05.007 = 0.007
het.perc.05.007 = 0

late.05.007 <- compute_LATE(het.spec.05.007, het.perc.05.007)
late.05.007


data.05.007.ate <- data.pman.0.slope.007 %>% 
  select(forest.ate, ols.ate) %>%
  mutate(forest.diff = (forest.ate - 0.02)/0.02) %>%
  mutate(ols.diff = (ols.ate - 0.02)/0.02) %>%
  select(-forest.ate, -ols.ate)

ate.05.007 <- round(apply(data.05.007.ate, 2, mean, na.rm = TRUE)*100, digits = 3)


data.05.007.late <- data.pman.0.slope.007 %>% 
  select(rd.late, forest.close.ate) %>%
  mutate(forest.diff = (forest.close.ate - as.vector(late.05.007))/as.vector(late.05.007)) %>%
  mutate(rd.diff = (rd.late - as.vector(late.05.007))/as.vector(late.05.007)) %>%
  select(-rd.late, -forest.close.ate)

late.cate.05.007 <- round(apply(data.05.007.late, 2, mean, na.rm = TRUE)*100, digits = 3)

###RMSE:
rmse.forest.ate.05.007 <- round(100 * sqrt(mean((data.pman.0.slope.007$forest.ate - 0.02)^2, na.rm = TRUE)) / 0.02, digits = 2)
rmse.rdd.late.05.007 <- round(100 * sqrt(mean((data.pman.0.slope.007$rd.late - as.vector(late.05.007))^2, na.rm = TRUE)) / late.05.007, digits = 2)
rmse.forest.cate.05.007 <- round(100 * sqrt(mean((data.pman.0.slope.007$forest.close.ate - as.vector(late.05.007))^2, na.rm = TRUE)) / late.05.007, digits = 2)

###For table wrap-up:
spec.details.05.007 <- c(0, het.spec.05.007,2, late.05.007*100)
spec.details.05.007

cf.ATE.05.007 <- c(ate.05.007[[1]], rmse.forest.ate.05.007)
cf.ATE.05.007

cf.CATE.05.007 <- c(late.cate.05.007[[1]], rmse.forest.cate.05.007)
cf.CATE.05.007

RDD.LATE.05.007 <- c(late.cate.05.007[[2]], rmse.rdd.late.05.007)
RDD.LATE.05.007

row.05.007 <- c(spec.details.05.007, cf.ATE.05.007, cf.CATE.05.007, RDD.LATE.05.007)
row.05.007



##0% manip, slope 0.01:
data.pman.0.slope.01 <- read.csv(paste(project.dir, "hetero/pman0_slope10_10000_obs_ps13_sims_alt.csv", sep="/"))


het.spec.05.01 = 0.01
het.perc.05.01 = 0

late.05.01 <- compute_LATE(het.spec.05.01, het.perc.05.01)
late.05.01


data.05.01.ate <- data.pman.0.slope.01 %>% 
  select(forest.ate, ols.ate) %>%
  mutate(forest.diff = (forest.ate - 0.02)/0.02) %>%
  mutate(ols.diff = (ols.ate - 0.02)/0.02) %>%
  select(-forest.ate, -ols.ate)

ate.05.01 <- round(apply(data.05.01.ate, 2, mean, na.rm = TRUE)*100, digits = 3)


data.05.01.late <- data.pman.0.slope.01 %>% 
  select(rd.late, forest.close.ate) %>%
  mutate(forest.diff = (forest.close.ate - as.vector(late.05.01))/as.vector(late.05.01)) %>%
  mutate(rd.diff = (rd.late - as.vector(late.05.01))/as.vector(late.05.01)) %>%
  select(-rd.late, -forest.close.ate)

late.cate.05.01 <- round(apply(data.05.01.late, 2, mean, na.rm = TRUE)*100, digits = 3)

###RMSE:
rmse.forest.ate.05.01 <- round(100 * sqrt(mean((data.pman.0.slope.01$forest.ate - 0.02)^2, na.rm = TRUE)) / 0.02, digits = 2)
rmse.rdd.late.05.01 <- round(100 * sqrt(mean((data.pman.0.slope.01$rd.late - as.vector(late.05.01))^2, na.rm = TRUE)) / late.05.01, digits = 2)
rmse.forest.cate.05.01 <- round(100 * sqrt(mean((data.pman.0.slope.01$forest.close.ate - as.vector(late.05.01))^2, na.rm = TRUE)) / late.05.01, digits = 2)

###For table wrap-up:
spec.details.05.01 <- c(0, het.spec.05.01,2, late.05.01*100)
spec.details.05.01

cf.ATE.05.01 <- c(ate.05.01[[1]], rmse.forest.ate.05.01)
cf.ATE.05.01

cf.CATE.05.01 <- c(late.cate.05.01[[1]], rmse.forest.cate.05.01)
cf.CATE.05.01

RDD.LATE.05.01 <- c(late.cate.05.01[[2]], rmse.rdd.late.05.01)
RDD.LATE.05.01

row.05.01 <- c(spec.details.05.01, cf.ATE.05.01, cf.CATE.05.01, RDD.LATE.05.01)
row.05.01

##################
###Final table rows:

final.table <- t(cbind(row.05.001,
                       row.05.003,
                       row.05.005,
                       row.05.007,
                       row.05.01))

final.table

print(xtable(final.table,digits=c(0, 0, 3, 0, 3, 3, 2, 3, 2, 3, 2)), include.rownames = FALSE,
      file = "output/Table_2_Panel_B.txt")


#0 & 0.001 & 2 & 1.965 & -0.440 & 17.79 & -1.203 & 27.19 & 0.522 & 24.15 \\ 
#0 & 0.003 & 2 & 1.895 & -0.648 & 17.89 & 1.426 & 28.39 & 0.489 & 25.02 \\ 
#0 & 0.005 & 2 & 1.825 & -0.891 & 17.98 & 3.901 & 29.93 & 0.447 & 25.97 \\ 
#0 & 0.007 & 2 & 1.755 & -1.137 & 18.21 & 6.130 & 32.28 & 0.413 & 27.03 \\ 
#0 & 0.010 & 2 & 1.650 & -1.498 & 18.41 & 9.299 & 35.41 & 0.343 & 28.76 \\ 



