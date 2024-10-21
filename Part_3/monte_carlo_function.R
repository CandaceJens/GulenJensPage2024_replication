
rdd.monte.carlo.linear.manipulation <- function(num.simulations = 1000, num.obs = 1000,
					linear = TRUE, manipulation = FALSE, latent = FALSE, 
					p.manipulate = NULL, latent_corr = NULL,
					sim.name = NULL) {

### load R libraries
library(grf)
library(MASS)
library(rdd)
library(svMisc)
library(plyr)
library(dplyr)

### setup
set.seed(3780811)
my.seed = matrix(sample(1:2^15, num.simulations*2), num.simulations, 2) # randomly chosen random seed for both simulated data and for causal forests (column 2)

mu <- NULL
sig <- NULL
rho.mat <- NULL
if (latent == TRUE) {
	mu <- as.matrix(c(1.4, 5.5, 0.1, 0.2, 0.30)) # variable means
	sig <- as.matrix(c(1.00, 1.50, 0.30, 0.30, 0.80)) # variable standard deviations
	if (is.null(latent_corr) | !(latent_corr %in% c("none", "low", "high"))) {
		return("latent_corr must be one of none, low, or high")
	} else if (latent_corr == "none") {
		rho.mat = matrix(c( 1.00,-0.05,-0.30, 0.15, 0.00,
						   -0.05, 1.00, 0.20, 0.35, 0.00,
						   -0.30, 0.20, 1.00, 0.50, 0.00,
							0.15, 0.35, 0.50, 1.00, 0.00,
							0.00, 0.00, 0.00, 0.00, 1.00),
						 5,5) # correlation matrix
	} else if (latent_corr == "low") {
		rho.mat = matrix(c( 1.00,-0.05,-0.30, 0.15, 0.10,
						   -0.05, 1.00, 0.20, 0.35, 0.10,
						   -0.30, 0.20, 1.00, 0.50, 0.05,
							0.15, 0.35, 0.50, 1.00, 0.10,
							0.10, 0.10, 0.05, 0.10, 1.00),
						 5,5) # correlation matrix
	} else if (latent_corr == "high") {
		rho.mat = matrix(c( 1.00,-0.05,-0.30, 0.15, 0.35,
						   -0.05, 1.00, 0.20, 0.35, 0.30,
						   -0.30, 0.20, 1.00, 0.50, 0.25,
							0.15, 0.35, 0.50, 1.00, 0.40,
							0.35, 0.30, 0.25, 0.40, 1.00),
						 5,5) # correlation matrix
	}
} else {
	mu <- as.matrix(c(1.4, 5.5, 0.1, 0.2)) # variable means
	sig <- as.matrix(c(1.00, 1.50, 0.30, 0.30)) # variable standard deviations
	rho.mat = matrix(c( 1.00,-0.05,-0.30, 0.15,
	                   -0.05, 1.00, 0.20, 0.35,
	                   -0.30, 0.20, 1.00, 0.50,
        	            0.15, 0.35, 0.50, 1.00),
                	 4,4) # correlation matrix
}
sigma <- rho.mat * (sig %*% t(sig)) # covariance matrix
y.sig = 0.065 # standard deviation of y error
te <- 0.02 # true treatment effect

sim_data_function <- NULL
if (linear == TRUE) {
	if (manipulation == TRUE & latent == TRUE) {
		return("Only one of manipulation or latent should be true")
	} else if (manipulation == TRUE) {
		if (is.null(p.manipulate) | !between(p.manipulate, 0, 1))) return("Please set p.manipulate to a value between 0 and 1")
		sim_data_function <- function(num.obs, mu, sigma) {
			sim.data = data.frame(mvrnorm(num.obs,mu,sigma))
			names(sim.data) = c("x1","x2","x3","d")
			p.can.manipulate = runif(num.obs)
			man.d = ifelse(p.can.manipulate <= p.manipulate & between(sim.data$d, -te / 0.20, 0),
					sim.data$d, 0)
			sim.data$d = sim.data$d - man.d
			sim.data$D = ifelse(sim.data$d > 0, 1, 0) # sharp regression discontinuity
			sim.data$y = 0.05 * sim.data$x1 - 0.005 * sim.data$x2 + 0.01 * sim.data$x3 + 
				0.02 * sim.data$D + man.d * 0.20 + y.sig * rnorm(num.obs)
			sim.data
		}
	} else if (latent == TRUE) {
		sim_data_function <- function(num.obs, mu, sigma) {
			sim.data = data.frame(mvrnorm(num.obs,mu,sigma))
			names(sim.data) = c("x1","x2","x3","d","z")
			sim.data$D = ifelse(sim.data$d > 0, 1, 0) # sharp regression discontinuity
			sim.data$y = 0.05 * sim.data$x1 - 0.005 * sim.data$x2 + 0.01 * sim.data$x3 + 
				0.02 * sim.data$D + 0.02 * sim.data$z + y.sig * rnorm(num.obs)
			sim.data
		}
	} else { # base case with no manipulation or omitted variable
		sim_data_function <- function(num.obs, mu, sigma) {
			sim.data = data.frame(mvrnorm(num.obs,mu,sigma))
			names(sim.data) = c("x1","x2","x3","d")
			sim.data$D = ifelse(sim.data$d > 0, 1, 0) # sharp regression discontinuity
			sim.data$y = 0.05 * sim.data$x1 - 0.005 * sim.data$x2 + 0.01 * sim.data$x3 + 
				0.02 * sim.data$D + y.sig * rnorm(num.obs)
			sim.data
		}
	}
} else {
	if (manipulation == TRUE & latent == TRUE) {
		return("Only one of manipulation or latent should be true")
	} else if (manipulation == TRUE) {
		if (is.null(p.manipulate) | !between(p.manipulate, 0, 1))) return("Please set p.manipulate to a value between 0 and 1")
		sim_data_function <- function(num.obs, mu, sigma) {
			sim.data = data.frame(mvrnorm(num.obs,mu,sigma))
			names(sim.data) = c("x1","x2","x3","d")
			p.can.manipulate = runif(num.obs)
			man.d = ifelse(p.can.manipulate <= p.manipulate & between(sim.data$d, -te / 0.20, 0),
					sim.data$d, 0)
			sim.data$d = sim.data$d - man.d
			sim.data$D = ifelse(sim.data$d > 0, 1, 0) # sharp regression discontinuity
			sim.data$y = 0.05 * sim.data$x1 - 0.005 * sim.data$x2 + 0.01 * sim.data$x3 + 
				0.025 * sim.data$x1^2 - 0.01 * sim.data$x2^2 + 0.015 * sim.data$x3^2 +
				0.02 * sim.data$D + man.d * 0.20 + y.sig * rnorm(num.obs)
			sim.data
		}
	} else if (latent == TRUE) {
		sim_data_function <- function(num.obs, mu, sigma) {
			sim.data = data.frame(mvrnorm(num.obs,mu,sigma))
			names(sim.data) = c("x1","x2","x3","d","z")
			sim.data$D = ifelse(sim.data$d > 0, 1, 0) # sharp regression discontinuity
			sim.data$y = 0.05 * sim.data$x1 - 0.005 * sim.data$x2 + 0.01 * sim.data$x3 + 
				0.025 * sim.data$x1^2 - 0.01 * sim.data$x2^2 + 0.015 * sim.data$x3^2 +
				0.02 * sim.data$D + 0.02 * sim.data$z - 0.01 * sim.data$z^2 + y.sig * rnorm(num.obs)
			sim.data
		}
	} else { # base case with no manipulation or omitted variable
		sim_data_function <- function(num.obs, mu, sigma) {
			sim.data = data.frame(mvrnorm(num.obs,mu,sigma))
			names(sim.data) = c("x1","x2","x3","d")
			sim.data$D = ifelse(sim.data$d > 0, 1, 0) # sharp regression discontinuity
			sim.data$y = 0.05 * sim.data$x1 - 0.005 * sim.data$x2 + 0.01 * sim.data$x3 + 
				0.025 * sim.data$x1^2 - 0.01 * sim.data$x2^2 + 0.015 * sim.data$x3^2 +
				0.02 * sim.data$D + y.sig * rnorm(num.obs)
			sim.data
		}
	}
}

mc.sims <- data.frame(
  rd.late = rep(0,num.simulations),
  rd.late.obs = rep(0,num.simulations),
  rd.late.se = rep(0,num.simulations),
  rd.half = rep(0,num.simulations),
  rd.half.obs = rep(0,num.simulations),
  rd.half.se = rep(0,num.simulations),
  rd.double = rep(0,num.simulations),
  rd.double.obs = rep(0,num.simulations),
  rd.double.se = rep(0,num.simulations),
  rd.mcreary = rep(0,num.simulations),
  ols.ate = rep(0,num.simulations),
  ols.ate.se = rep(0,num.simulations),
  forest.ate = rep(0,num.simulations),
  forest.ate.se = rep(0,num.simulations),
  forest.att = rep(0,num.simulations),
  forest.att.se = rep(0,num.simulations),
  forest.overlap = rep(0,num.simulations),
  forest.overlap.se = rep(0,num.simulations),
  forest.close.ate = rep(0,num.simulations),
  forest.close.ate.se = rep(0,num.simulations),
  forest.close.att = rep(0,num.simulations),
  forest.close.att.se = rep(0,num.simulations),
  forest.close.overlap = rep(0,num.simulations),
  forest.close.overlap.se = rep(0,num.simulations),
  forest.away.ate = rep(0,num.simulations),
  forest.away.ate.se = rep(0,num.simulations),
  forest.away.att = rep(0,num.simulations),
  forest.away.att.se = rep(0,num.simulations),
  forest.away.overlap = rep(0,num.simulations),
  forest.away.overlap.se = rep(0,num.simulations)
)

# to save time, only want to do the tuning once, then use those parameters below
set.seed(my.seed[1,1])
sim.data <- sim_data_function(num.obs, mu, sigma)
bd=IKbandwidth(sim.data$d, sim.data$y)
set.seed(my.seed[1,2])
tuned.params = tune_causal_forest(sim.data[c("x1","x2","x3")], sim.data$y, sim.data$D, 
                                  num.fit.trees = 400, # number of trees in each mini forests used to fit the training model
                                  num.fit.reps = 20, # number of mini forests used to fit the training model
                                  num.optimize.reps = 1000, # number of random parameter values considered during training
                                  min.node.size = NULL, # target for the minimum number of observations per leaf
                                  sample.fraction = 0.5, #NULL, # fraction of the data used to build the tree (this fraction is cut in half if also using honesty)
                                  mtry = NULL, # number of variables tried for each split
                                  alpha = NULL, # tunes the maximum imbalance of a split
                                  imbalance.penalty = NULL, # controls how harshly imbalanced splits are punished
                                  stabilize.splits = TRUE, # is the treatment considered when considering how imbalanced split is
                                  num.threads = NULL, # number of threads (just set to NULL and let softward decide)
                                  honesty = TRUE, # use honest sample splits? (see causal regression forest paper for definition)
                                  seed = my.seed[1,2], # seed for the C++ random number generator, but per Chet it is currently non-functional, set seed before function call
                                  clusters = NULL, # vector of integers or factors denoting which cluster an observation belongs to
                                  samples_per_cluster = NULL # how many observations to be sampled from each cluster
)$params

for (i in 1:num.simulations) {
  # simulate the data
  set.seed(my.seed[i,1])
  sim.data <- sim_data_function(num.obs,mu,sigma)
  
  # first run a typical regression discontinuity regression
  temp.capture = capture.output( {temp.rd <- summary(RDestimate(y ~ d | x1 + x2 + x3, data=sim.data))} )
  bd=IKbandwidth(sim.data$d, sim.data$y)
  mc.sims$rd.mcreary[i] = DCdensity(sim.data$d, plot = FALSE)
  mc.sims$rd.late[i] = temp.rd$coefficients[1,3]
  mc.sims$rd.late.obs[i] = temp.rd$coefficients[1,2]
  mc.sims$rd.late.se[i] = temp.rd$coefficients[1,4]
  mc.sims$rd.half[i] = temp.rd$coefficients[2,3]
  mc.sims$rd.half.obs[i] = temp.rd$coefficients[2,2]
  mc.sims$rd.half.se[i] = temp.rd$coefficients[2,4]
  mc.sims$rd.double[i] = temp.rd$coefficients[3,3]
  mc.sims$rd.double.obs[i] = temp.rd$coefficients[3,2]
  mc.sims$rd.double.se[i] = temp.rd$coefficients[3,4]
  
  # regular OLS and with the bandwidths
  temp.ols = summary(lm(y ~ x1 + x2 + x3 + D, data=sim.data), noprint=T)
  mc.sims$ols.ate[i] = temp.ols$coefficients[5,1]
  mc.sims$ols.ate.se[i] = temp.ols$coefficients[5,2]
  
  # and now a causal forest (not including d)
  set.seed(my.seed[i,2])
  forest.out = causal_forest(sim.data[c("x1","x2","x3")], sim.data$y, sim.data$D, 
                             num.trees = 1000,
                             seed = my.seed[i,2],
                             honesty = TRUE,
                             # the below all come from the tuning done above #
                             min.node.size = as.numeric(tuned.params["min.node.size"]),
                             sample.fraction = as.numeric(tuned.params["sample.fraction"]),
                             mtry = as.numeric(tuned.params["mtry"]),
                             alpha = as.numeric(tuned.params["alpha"]),
                             imbalance.penalty = as.numeric(tuned.params["imbalance.penalty"]))
  temp.forest.ate = average_treatment_effect(forest.out, target.sample = "all")
  temp.forest.att = average_treatment_effect(forest.out, target.sample = "treated")
  temp.forest.overlap = average_treatment_effect(forest.out, target.sample = "overlap")
  mc.sims$forest.ate[i] = temp.forest.ate[1]
  mc.sims$forest.ate.se[i] = temp.forest.ate[2]
  mc.sims$forest.att[i] = temp.forest.att[1]
  mc.sims$forest.att.se[i] = temp.forest.att[2]
  mc.sims$forest.overlap[i] = temp.forest.overlap[1]
  mc.sims$forest.overlap.se[i] = temp.forest.overlap[2]
  temp.forest.ate = average_treatment_effect(forest.out, target.sample = "all", subset = abs(sim.data$d) <= bd)
  temp.forest.att = average_treatment_effect(forest.out, target.sample = "treated", subset = abs(sim.data$d) <= bd)
  temp.forest.overlap = average_treatment_effect(forest.out, target.sample = "overlap", subset = abs(sim.data$d) <= bd)
  mc.sims$forest.close.ate[i] = temp.forest.ate[1]
  mc.sims$forest.close.ate.se[i] = temp.forest.ate[2]
  mc.sims$forest.close.att[i] = temp.forest.att[1]
  mc.sims$forest.close.att.se[i] = temp.forest.att[2]
  mc.sims$forest.close.overlap[i] = temp.forest.overlap[1]
  mc.sims$forest.close.overlap.se[i] = temp.forest.overlap[2]
  if (sum(abs(sim.data$d) > bd) > 30) {
    temp.forest.ate = average_treatment_effect(forest.out, target.sample = "all", subset = abs(sim.data$d) > bd)
    temp.forest.att = average_treatment_effect(forest.out, target.sample = "treated", subset = abs(sim.data$d) > bd)
    temp.forest.overlap = average_treatment_effect(forest.out, target.sample = "overlap", subset = abs(sim.data$d) > bd)
    mc.sims$forest.away.ate[i] = temp.forest.ate[1]
    mc.sims$forest.away.ate.se[i] = temp.forest.ate[2]
    mc.sims$forest.away.att[i] = temp.forest.att[1]
    mc.sims$forest.away.att.se[i] = temp.forest.att[2]
    mc.sims$forest.away.overlap[i] = temp.forest.overlap[1]
    mc.sims$forest.away.overlap.se[i] = temp.forest.overlap[2]
  } else { # not enough observations outside of the bandwidth, so set to NA
    mc.sims$forest.away.ate[i] = NA
    mc.sims$forest.away.ate.se[i] = NA
    mc.sims$forest.away.att[i] = NA
    mc.sims$forest.away.att.se[i] = NA
    mc.sims$forest.away.overlap[i] = NA
    mc.sims$forest.away.overlap.se[i] = NA
  }
  
  progress(i, max.value = num.simulations)

}

method.names = c("RDD","RDD_half_bw","RDD_double_bw","OLS",
                 "Forest_ATE","Forest_ATT","Forest_overlap",
                 "Forest_close_ATE","Forest_close_ATT","Forest_close_overlap",
                 "Forest_away_ATE","Forest_away_ATT","Forest_away_overlap")
num.method = length(method.names)
mc.out = data.frame(method = method.names,
                     bias = rep(0,num.method), 
                     rmse = rep(0,num.method),
                     coverage = rep(0,num.method))
coverage.t = qt(0.975, num.obs - length(coeffs) - 2)
# bias as a percentage of the treatment effect
mc.out$bias[1] = (mean(mc.sims$rd.late - te)) / te
mc.out$bias[2] = (mean(mc.sims$rd.half - te)) / te
mc.out$bias[3] = (mean(mc.sims$rd.double - te)) / te
mc.out$bias[4] = (mean(mc.sims$ols.ate - te)) / te
mc.out$bias[5] = (mean(mc.sims$forest.ate - te)) / te
mc.out$bias[6] = (mean(mc.sims$forest.att - te)) / te
mc.out$bias[7] = (mean(mc.sims$forest.overlap - te)) / te
mc.out$bias[8] = (mean(mc.sims$forest.close.ate - te)) / te
mc.out$bias[9] = (mean(mc.sims$forest.close.att - te)) / te
mc.out$bias[10] = (mean(mc.sims$forest.close.overlap - te)) / te
mc.out$bias[11] = (mean(mc.sims$forest.away.ate - te, na.rm = TRUE)) / te
mc.out$bias[12] = (mean(mc.sims$forest.away.att - te, na.rm = TRUE)) / te
mc.out$bias[13] = (mean(mc.sims$forest.away.overlap - te, na.rm = TRUE)) / te

# root mean squared error (RMSE) as a percentage of the treatment effect
mc.out$rmse[1] = (sd(mc.sims$rd.late - te)) / te
mc.out$rmse[2] = (sd(mc.sims$rd.half - te)) / te
mc.out$rmse[3] = (sd(mc.sims$rd.double - te)) / te
mc.out$rmse[4] = (sd(mc.sims$ols.ate - te)) / te
mc.out$rmse[5] = (sd(mc.sims$forest.ate - te)) / te
mc.out$rmse[6] = (sd(mc.sims$forest.att - te)) / te
mc.out$rmse[7] = (sd(mc.sims$forest.overlap - te)) / te
mc.out$rmse[8] = (sd(mc.sims$forest.close.ate - te)) / te
mc.out$rmse[9] = (sd(mc.sims$forest.close.att - te)) / te
mc.out$rmse[10] = (sd(mc.sims$forest.close.overlap - te)) / te
mc.out$rmse[11] = (sd(mc.sims$forest.away.ate[!is.na(mc.sims$forest.away.ate)] - te)) / te
mc.out$rmse[12] = (sd(mc.sims$forest.away.att[!is.na(mc.sims$forest.away.att)] - te)) / te
mc.out$rmse[13] = (sd(mc.sims$forest.away.overlap[!is.na(mc.sims$forest.away.overlap)] - te)) / te

# coverage as the percentage of times the test rejects at the 5% level
mc.out$coverage[1] = 1 - sum((abs(mc.sims$rd.late - te) / mc.sims$rd.late.se) <= coverage.t) / num.simulations
mc.out$coverage[2] = 1 - sum((abs(mc.sims$rd.half - te) / mc.sims$rd.half.se) <= coverage.t) / num.simulations
mc.out$coverage[3] = 1 - sum((abs(mc.sims$rd.double - te) / mc.sims$rd.double.se) <= coverage.t) / num.simulations
mc.out$coverage[4] = 1 - sum((abs(mc.sims$ols.ate - te) / mc.sims$ols.ate.se) <= coverage.t) / num.simulations
mc.out$coverage[5] = 1 - sum((abs(mc.sims$forest.ate - te) / mc.sims$forest.ate.se) <= coverage.t) / num.simulations
mc.out$coverage[6] = 1 - sum((abs(mc.sims$forest.att - te) / mc.sims$forest.att.se) <= coverage.t) / num.simulations
mc.out$coverage[7] = 1 - sum((abs(mc.sims$forest.overlap - te) / mc.sims$forest.overlap.se) <= coverage.t) / num.simulations
mc.out$coverage[8] = 1 - sum((abs(mc.sims$forest.close.ate - te) / mc.sims$forest.close.ate.se) <= coverage.t) / num.simulations
mc.out$coverage[9] = 1 - sum((abs(mc.sims$forest.close.att - te) / mc.sims$forest.close.att.se) <= coverage.t) / num.simulations
mc.out$coverage[10] = 1 - sum((abs(mc.sims$forest.close.overlap - te) / mc.sims$forest.close.overlap.se) <= coverage.t) / num.simulations
num.simulations.away = sum(!is.na(mc.sims$forest.away.overlap))
mc.out$coverage[11] = 1 - sum((abs(mc.sims$forest.away.ate - te) / mc.sims$forest.away.ate.se) <= coverage.t, na.rm = TRUE) / num.simulations.away
mc.out$coverage[12] = 1 - sum((abs(mc.sims$forest.away.att - te) / mc.sims$forest.away.att.se) <= coverage.t, na.rm = TRUE) / num.simulations.away
mc.out$coverage[13] = 1 - sum((abs(mc.sims$forest.away.overlap - te) / mc.sims$forest.away.overlap.se) <= coverage.t, na.rm = TRUE) / num.simulations.away


save(mc.out, file = paste(sim.name,"out.RData",sep="_"))
save(mc.sims, file = paste(sim.name,"sims.RRData",sep="."),sep="/")
write.csv(mc.out, file = paste(sim.name,"out.csv",sep="_"),sep="/")
write.csv(mc.sims, file = paste(sim.name,"sims.csv",sep="_"),sep="/")

mc.out # return mc.out
}
