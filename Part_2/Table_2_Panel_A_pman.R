rm(list=ls())


##Loads required packages 
#require(ggplot2)
require(tidyverse)
library(xtable)

project.dir <- "."

OUTPUT_DIRECTORY <- "output"


###### Load Data ######

data.0.05 <- read.csv(paste(project.dir, "pman/pman0.05_slope_10000_obs_sims_alt.csv", sep="/"))

manip.perc = 5

late.05 <- 0.02

data.05.ate <- data.0.05 %>% 
  select(forest.ate, ols.ate) %>%
  mutate(forest.diff = (forest.ate - 0.02)/0.02) %>%
  mutate(ols.diff = (ols.ate - 0.02)/0.02) %>%
  select(-forest.ate, -ols.ate)

ate.05 <- round(apply(data.05.ate, 2, mean, na.rm = TRUE)*100, digits = 3)


data.05.late <- data.0.05 %>% 
  select(rd.late, forest.close.ate) %>%
  mutate(forest.diff = (forest.close.ate - as.vector(late.05))/as.vector(late.05)) %>%
  mutate(rd.diff = (rd.late - as.vector(late.05))/as.vector(late.05)) %>%
  select(-rd.late, -forest.close.ate)

late.cate.05 <- round(apply(data.05.late, 2, mean, na.rm = TRUE)*100, digits = 3)

###RMSE:
rmse.forest.ate.05 <- round(100 * sqrt(mean((data.0.05$forest.ate - 0.02)^2, na.rm = TRUE)) / 0.02, digits = 2)
rmse.rdd.late.05 <- round(100 * sqrt(mean((data.0.05$rd.late - as.vector(late.05))^2, na.rm = TRUE)) / late.05, digits = 2)
rmse.forest.cate.05 <- round(100 * sqrt(mean((data.0.05$forest.close.ate - as.vector(late.05))^2, na.rm = TRUE)) / late.05, digits = 2)

###For table wrap-up:
spec.details.05 <- c(manip.perc, 0, 2, late.05*100)
spec.details.05

cf.ATE.05 <- c(ate.05[[1]], rmse.forest.ate.05)
cf.ATE.05

cf.CATE.05 <- c(late.cate.05[[1]], rmse.forest.cate.05)
cf.CATE.05

RDD.LATE.05 <- c(late.cate.05[[2]], rmse.rdd.late.05)
RDD.LATE.05

###This gives manip perc, ATE, LATE, cf LATE bias, cf RMSE, cf CATE bias, cf CATE rmse, RDD LATE bias, RDD RMSE
row.05 <- c(spec.details.05, cf.ATE.05, cf.CATE.05, RDD.LATE.05)
row.05



###### Load Data ######

data.0.10 <- read.csv(paste(project.dir, "pman/pman0.1_slope_10000_obs_sims_alt.csv", sep="/"))

manip.perc = 10

late.10 <- 0.02

data.10.ate <- data.0.10 %>% 
  select(forest.ate, ols.ate) %>%
  mutate(forest.diff = (forest.ate - 0.02)/0.02) %>%
  mutate(ols.diff = (ols.ate - 0.02)/0.02) %>%
  select(-forest.ate, -ols.ate)

ate.10 <- round(apply(data.10.ate, 2, mean, na.rm = TRUE)*100, digits = 3)


data.10.late <- data.0.10 %>% 
  select(rd.late, forest.close.ate) %>%
  mutate(forest.diff = (forest.close.ate - as.vector(late.10))/as.vector(late.10)) %>%
  mutate(rd.diff = (rd.late - as.vector(late.10))/as.vector(late.10)) %>%
  select(-rd.late, -forest.close.ate)

late.cate.10 <- round(apply(data.10.late, 2, mean, na.rm = TRUE)*100, digits = 3)

###RMSE:
rmse.forest.ate.10 <- round(100 * sqrt(mean((data.0.10$forest.ate - 0.02)^2, na.rm = TRUE)) / 0.02, digits = 2)
rmse.rdd.late.10 <- round(100 * sqrt(mean((data.0.10$rd.late - as.vector(late.10))^2, na.rm = TRUE)) / late.10, digits = 2)
rmse.forest.cate.10 <- round(100 * sqrt(mean((data.0.10$forest.close.ate - as.vector(late.10))^2, na.rm = TRUE)) / late.10, digits = 2)

###For table wrap-up:
spec.details.10 <- c(manip.perc, 0, 2, late.10*100)
spec.details.10

cf.ATE.10 <- c(ate.10[[1]], rmse.forest.ate.10)
cf.ATE.10

cf.CATE.10 <- c(late.cate.10[[1]], rmse.forest.cate.10)
cf.CATE.10

RDD.LATE.10 <- c(late.cate.10[[2]], rmse.rdd.late.10)
RDD.LATE.10

###This gives manip perc, ATE, LATE, cf LATE bias, cf RMSE, cf CATE bias, cf CATE rmse, RDD LATE bias, RDD RMSE
row.10 <- c(spec.details.10, cf.ATE.10, cf.CATE.10, RDD.LATE.10)
row.10



###### Load Data ######

data.0.15 <- read.csv(paste(project.dir, "pman/pman0.15_slope_10000_obs_sims_alt.csv", sep="/"))

manip.perc = 15

late.15 <- 0.02

data.15.ate <- data.0.15 %>% 
  select(forest.ate, ols.ate) %>%
  mutate(forest.diff = (forest.ate - 0.02)/0.02) %>%
  mutate(ols.diff = (ols.ate - 0.02)/0.02) %>%
  select(-forest.ate, -ols.ate)

ate.15 <- round(apply(data.15.ate, 2, mean, na.rm = TRUE)*100, digits = 3)


data.15.late <- data.0.15 %>% 
  select(rd.late, forest.close.ate) %>%
  mutate(forest.diff = (forest.close.ate - as.vector(late.15))/as.vector(late.15)) %>%
  mutate(rd.diff = (rd.late - as.vector(late.15))/as.vector(late.15)) %>%
  select(-rd.late, -forest.close.ate)

late.cate.15 <- round(apply(data.15.late, 2, mean, na.rm = TRUE)*100, digits = 3)

###RMSE:
rmse.forest.ate.15 <- round(100 * sqrt(mean((data.0.15$forest.ate - 0.02)^2, na.rm = TRUE)) / 0.02, digits = 2)
rmse.rdd.late.15 <- round(100 * sqrt(mean((data.0.15$rd.late - as.vector(late.15))^2, na.rm = TRUE)) / late.15, digits = 2)
rmse.forest.cate.15 <- round(100 * sqrt(mean((data.0.15$forest.close.ate - as.vector(late.15))^2, na.rm = TRUE)) / late.15, digits = 2)

###For table wrap-up:
spec.details.15 <- c(manip.perc, 0, 2, late.15*100)
spec.details.15

cf.ATE.15 <- c(ate.15[[1]], rmse.forest.ate.15)
cf.ATE.15

cf.CATE.15 <- c(late.cate.15[[1]], rmse.forest.cate.15)
cf.CATE.15

RDD.LATE.15 <- c(late.cate.15[[2]], rmse.rdd.late.15)
RDD.LATE.15

###This gives manip perc, ATE, LATE, cf LATE bias, cf RMSE, cf CATE bias, cf CATE rmse, RDD LATE bias, RDD RMSE
row.15 <- c(spec.details.15, cf.ATE.15, cf.CATE.15, RDD.LATE.15)
row.15



###### Load Data ######

data.0.20 <- read.csv(paste(project.dir, "pman/pman0.2_slope_10000_obs_sims_alt.csv", sep="/"))

manip.perc = 20

late.20 <- 0.02

data.20.ate <- data.0.20 %>% 
  select(forest.ate, ols.ate) %>%
  mutate(forest.diff = (forest.ate - 0.02)/0.02) %>%
  mutate(ols.diff = (ols.ate - 0.02)/0.02) %>%
  select(-forest.ate, -ols.ate)

ate.20 <- round(apply(data.20.ate, 2, mean, na.rm = TRUE)*100, digits = 3)


data.20.late <- data.0.20 %>% 
  select(rd.late, forest.close.ate) %>%
  mutate(forest.diff = (forest.close.ate - as.vector(late.20))/as.vector(late.20)) %>%
  mutate(rd.diff = (rd.late - as.vector(late.20))/as.vector(late.20)) %>%
  select(-rd.late, -forest.close.ate)

late.cate.20 <- round(apply(data.20.late, 2, mean, na.rm = TRUE)*100, digits = 3)

###RMSE:
rmse.forest.ate.20 <- round(100 * sqrt(mean((data.0.20$forest.ate - 0.02)^2, na.rm = TRUE)) / 0.02, digits = 2)
rmse.rdd.late.20 <- round(100 * sqrt(mean((data.0.20$rd.late - as.vector(late.20))^2, na.rm = TRUE)) / late.20, digits = 2)
rmse.forest.cate.20 <- round(100 * sqrt(mean((data.0.20$forest.close.ate - as.vector(late.20))^2, na.rm = TRUE)) / late.20, digits = 2)

###For table wrap-up:
spec.details.20 <- c(manip.perc, 0, 2, late.20*100)
spec.details.20

cf.ATE.20 <- c(ate.20[[1]], rmse.forest.ate.20)
cf.ATE.20

cf.CATE.20 <- c(late.cate.20[[1]], rmse.forest.cate.20)
cf.CATE.20

RDD.LATE.20 <- c(late.cate.20[[2]], rmse.rdd.late.20)
RDD.LATE.20

###This gives manip perc, ATE, LATE, cf LATE bias, cf RMSE, cf CATE bias, cf CATE rmse, RDD LATE bias, RDD RMSE
row.20 <- c(spec.details.20, cf.ATE.20, cf.CATE.20, RDD.LATE.20)
row.20


###### Load Data ######

data.0.25 <- read.csv(paste(project.dir, "pman/pman0.25_slope_10000_obs_sims_alt.csv", sep="/"))

manip.perc = 25

late.25 <- 0.02

data.25.ate <- data.0.25 %>% 
  select(forest.ate, ols.ate) %>%
  mutate(forest.diff = (forest.ate - 0.02)/0.02) %>%
  mutate(ols.diff = (ols.ate - 0.02)/0.02) %>%
  select(-forest.ate, -ols.ate)

ate.25 <- round(apply(data.25.ate, 2, mean, na.rm = TRUE)*100, digits = 3)


data.25.late <- data.0.25 %>% 
  select(rd.late, forest.close.ate) %>%
  mutate(forest.diff = (forest.close.ate - as.vector(late.25))/as.vector(late.25)) %>%
  mutate(rd.diff = (rd.late - as.vector(late.25))/as.vector(late.25)) %>%
  select(-rd.late, -forest.close.ate)

late.cate.25 <- round(apply(data.25.late, 2, mean, na.rm = TRUE)*100, digits = 3)

###RMSE:
rmse.forest.ate.25 <- round(100 * sqrt(mean((data.0.25$forest.ate - 0.02)^2, na.rm = TRUE)) / 0.02, digits = 2)
rmse.rdd.late.25 <- round(100 * sqrt(mean((data.0.25$rd.late - as.vector(late.25))^2, na.rm = TRUE)) / late.25, digits = 2)
rmse.forest.cate.25 <- round(100 * sqrt(mean((data.0.25$forest.close.ate - as.vector(late.25))^2, na.rm = TRUE)) / late.25, digits = 2)

###For table wrap-up:
spec.details.25 <- c(manip.perc, 0, 2, late.25*100)
spec.details.25

cf.ATE.25 <- c(ate.25[[1]], rmse.forest.ate.25)
cf.ATE.25

cf.CATE.25 <- c(late.cate.25[[1]], rmse.forest.cate.25)
cf.CATE.25

RDD.LATE.25 <- c(late.cate.25[[2]], rmse.rdd.late.25)
RDD.LATE.25

###This gives manip perc, ATE, LATE, cf LATE bias, cf RMSE, cf CATE bias, cf CATE rmse, RDD LATE bias, RDD RMSE
row.25 <- c(spec.details.25, cf.ATE.25, cf.CATE.25, RDD.LATE.25)
row.25


#################
###0.40
data.0.40 <- read.csv(paste(project.dir, "pman/manipulation_40_10000_obs_sims_alt.csv", sep="/"))

manip.perc = 40

late.40 <- 0.02

data.40.ate <- data.0.40 %>% 
  select(forest.ate, ols.ate) %>%
  mutate(forest.diff = (forest.ate - 0.02)/0.02) %>%
  mutate(ols.diff = (ols.ate - 0.02)/0.02) %>%
  select(-forest.ate, -ols.ate)

ate.40 <- round(apply(data.40.ate, 2, mean, na.rm = TRUE)*100, digits = 3)


data.40.late <- data.0.40 %>% 
  select(rd.late, forest.close.ate) %>%
  mutate(forest.diff = (forest.close.ate - as.vector(late.40))/as.vector(late.40)) %>%
  mutate(rd.diff = (rd.late - as.vector(late.40))/as.vector(late.40)) %>%
  select(-rd.late, -forest.close.ate)

late.cate.40 <- round(apply(data.40.late, 2, mean, na.rm = TRUE)*100, digits = 3)

###RMSE:
rmse.forest.ate.40 <- round(100 * sqrt(mean((data.0.40$forest.ate - 0.02)^2, na.rm = TRUE)) / 0.02, digits = 2)
rmse.rdd.late.40 <- round(100 * sqrt(mean((data.0.40$rd.late - as.vector(late.40))^2, na.rm = TRUE)) / late.40, digits = 2)
rmse.forest.cate.40 <- round(100 * sqrt(mean((data.0.40$forest.close.ate - as.vector(late.40))^2, na.rm = TRUE)) / late.40, digits = 2)

###For table wrap-up:
spec.details.40 <- c(manip.perc, 0, 2, late.40*100)
spec.details.40

cf.ATE.40 <- c(ate.40[[1]], rmse.forest.ate.40)
cf.ATE.40

cf.CATE.40 <- c(late.cate.40[[1]], rmse.forest.cate.40)
cf.CATE.40

RDD.LATE.40 <- c(late.cate.40[[2]], rmse.rdd.late.40)
RDD.LATE.40

###This gives manip perc, ATE, LATE, cf LATE bias, cf RMSE, cf CATE bias, cf CATE rmse, RDD LATE bias, RDD RMSE
row.40 <- c(spec.details.40, cf.ATE.40, cf.CATE.40, RDD.LATE.40)
row.40


#################
###0.55
data.0.55 <- read.csv(paste(project.dir, "pman/manipulation_55_10000_obs_sims_alt.csv", sep="/"))

manip.perc = 55

late.55 <- 0.02

data.55.ate <- data.0.55 %>% 
  select(forest.ate, ols.ate) %>%
  mutate(forest.diff = (forest.ate - 0.02)/0.02) %>%
  mutate(ols.diff = (ols.ate - 0.02)/0.02) %>%
  select(-forest.ate, -ols.ate)

ate.55 <- round(apply(data.55.ate, 2, mean, na.rm = TRUE)*100, digits = 3)


data.55.late <- data.0.55 %>% 
  select(rd.late, forest.close.ate) %>%
  mutate(forest.diff = (forest.close.ate - as.vector(late.55))/as.vector(late.55)) %>%
  mutate(rd.diff = (rd.late - as.vector(late.55))/as.vector(late.55)) %>%
  select(-rd.late, -forest.close.ate)

late.cate.55 <- round(apply(data.55.late, 2, mean, na.rm = TRUE)*100, digits = 3)

###RMSE:
rmse.forest.ate.55 <- round(100 * sqrt(mean((data.0.55$forest.ate - 0.02)^2, na.rm = TRUE)) / 0.02, digits = 2)
rmse.rdd.late.55 <- round(100 * sqrt(mean((data.0.55$rd.late - as.vector(late.55))^2, na.rm = TRUE)) / late.55, digits = 2)
rmse.forest.cate.55 <- round(100 * sqrt(mean((data.0.55$forest.close.ate - as.vector(late.55))^2, na.rm = TRUE)) / late.55, digits = 2)

###For table wrap-up:
spec.details.55 <- c(manip.perc, 0, 2, late.55*100)
spec.details.55

cf.ATE.55 <- c(ate.55[[1]], rmse.forest.ate.55)
cf.ATE.55

cf.CATE.55 <- c(late.cate.55[[1]], rmse.forest.cate.55)
cf.CATE.55

RDD.LATE.55 <- c(late.cate.55[[2]], rmse.rdd.late.55)
RDD.LATE.55

###This gives manip perc, ATE, LATE, cf LATE bias, cf RMSE, cf CATE bias, cf CATE rmse, RDD LATE bias, RDD RMSE
row.55 <- c(spec.details.55, cf.ATE.55, cf.CATE.55, RDD.LATE.55)
row.55




#################
###0.70
data.0.70 <- read.csv(paste(project.dir, "pman/manipulation_70_10000_obs_sims_alt.csv", sep="/"))

manip.perc = 70

late.70 <- 0.02

data.70.ate <- data.0.70 %>% 
  select(forest.ate, ols.ate) %>%
  mutate(forest.diff = (forest.ate - 0.02)/0.02) %>%
  mutate(ols.diff = (ols.ate - 0.02)/0.02) %>%
  select(-forest.ate, -ols.ate)

ate.70 <- round(apply(data.70.ate, 2, mean, na.rm = TRUE)*100, digits = 3)


data.70.late <- data.0.70 %>% 
  select(rd.late, forest.close.ate) %>%
  mutate(forest.diff = (forest.close.ate - as.vector(late.70))/as.vector(late.70)) %>%
  mutate(rd.diff = (rd.late - as.vector(late.70))/as.vector(late.70)) %>%
  select(-rd.late, -forest.close.ate)

late.cate.70 <- round(apply(data.70.late, 2, mean, na.rm = TRUE)*100, digits = 3)

###RMSE:
rmse.forest.ate.70 <- round(100 * sqrt(mean((data.0.70$forest.ate - 0.02)^2, na.rm = TRUE)) / 0.02, digits = 2)
rmse.rdd.late.70 <- round(100 * sqrt(mean((data.0.70$rd.late - as.vector(late.70))^2, na.rm = TRUE)) / late.70, digits = 2)
rmse.forest.cate.70 <- round(100 * sqrt(mean((data.0.70$forest.close.ate - as.vector(late.70))^2, na.rm = TRUE)) / late.70, digits = 2)

###For table wrap-up:
spec.details.70 <- c(manip.perc, 0, 2, late.70*100)
spec.details.70

cf.ATE.70 <- c(ate.70[[1]], rmse.forest.ate.70)
cf.ATE.70

cf.CATE.70 <- c(late.cate.70[[1]], rmse.forest.cate.70)
cf.CATE.70

RDD.LATE.70 <- c(late.cate.70[[2]], rmse.rdd.late.70)
RDD.LATE.70

###This gives manip perc, ATE, LATE, cf LATE bias, cf RMSE, cf CATE bias, cf CATE rmse, RDD LATE bias, RDD RMSE
row.70 <- c(spec.details.70, cf.ATE.70, cf.CATE.70, RDD.LATE.70)
row.70


final.table <- t(cbind(row.05,
                       row.10,
                       row.15,
                       row.20,
                       row.25,
                       row.40,
                       row.55,
                       row.70))

final.table

print(xtable(final.table,digits=c(0, 0, 0, 0, 0, 3, 2, 3, 2, 3, 2)), include.rownames = FALSE,
      file = "output/Table_2_Panel_A.txt")

#row.05    5    0    2    2 0.036 18.29 -2.119 28.33 -13.461  27.89
#row.10   10    0    2    2 0.182 18.23 -2.082 29.89 -25.272  36.03
#row.15   15    0    2    2 0.303 18.35 -2.090 31.48 -35.316  44.22
#row.20   20    0    2    2 0.463 18.29 -2.133 32.61 -43.410  51.28
#row.25   25    0    2    2 0.625 18.25 -2.198 33.46 -49.723  56.98
#row.40   40    0    2    2 1.014 18.18 -1.957 34.73 -63.006  69.82
#row.55   55    0    2    2 1.466 18.28 -0.822 39.56 -77.980  86.37
#row.70   70    0    2    2 1.942 18.35  0.013 49.85 -88.769 101.37

