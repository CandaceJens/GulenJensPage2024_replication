

# I think the main thing this does is update the "i"th observation of mc.sims
# It also assigns to sim.data (which might just be local?)
# I would obviously like it to be a pure function
simulate_once <- function(i, my.seed, mu, sigma, te, int.coeff,
                          het.slope,
                          num.obs, sig,
                          function.form,
                          x.jump, manip.per,
                          coeffs, y.sig, interaction_strength, num.trees=2000){

  set.seed(my.seed[i,1])
  sim.data = data.frame(mvrnorm(num.obs,mu,sigma))
  names(sim.data) = c("x1","x2","x3","d","x4")
  sim.data$D = ifelse(sim.data$d > 0, 1, 0) # sharp regression discontinuity

  print("sim.data$D and x.jump[1]:")
  print(sim.data$D)
  print(x.jump)
  sim.data$x1 <- sim.data$x1 + sim.data$D * x.jump[1] * sig[1]
  sim.data$x2 <- sim.data$x2 + sim.data$D * x.jump[2] * sig[2]
  sim.data$x3 <- sim.data$x3 + sim.data$D * x.jump[3] * sig[3]
  print("checkpoint 1")

  sim.data$te <- matrix(te, num.obs, 1)
  if (het.slope != 0.00) { # heterogeneity in treatment effects
    print("hi")
    print(sim.data$x1)
    print(sim.data$x3)
    print(sim.data$d)
    print(mu)
    print(dim(sim.data$te))
    print(dim(0.02 + het.slope * (sim.data$x2 - mu[2])))
    print(length(0.02 + het.slope * (sim.data$x2 - mu[2])))
    print(length(0.02 + het.slope * (sim.data$x2 - mu[2])))
    print(het.slope)
    sim.data$te <- 0.02 + het.slope * (sim.data$x2 - mu[2])
  }
  sim.data$man.d <- 0
  if (manip.per > 0) { # manipulation
    p.can.manipulate = runif(num.obs)
    sim.data$man.d = ifelse(p.can.manipulate <= manip.per & between(sim.data$d, -te * 10, 0),
                            sim.data$d, 0)
    sim.data$d = sim.data$d - sim.data$man.d
  }
  if (function.form == "base") {
    # sim.data$y = sim.data$x1 * coeffs[1] + sim.data$x2 * coeffs[2] + sim.data$x3 * coeffs[3] + sim.data$x4 * coeffs[4] +
    #   (sim.data$x1 - mu[1])^2 * coeffs[5] + (sim.data$x2 - mu[2])^2 * coeffs[6] + (sim.data$x3 - mu[3])^2 * coeffs[7] +
    #   (sim.data$x4 - mu[5])^2 * coeffs[8] +
    #   sim.data$man.d * 0.01 + # with manipulation there is a conversion between manipulation and outcome (0.01)
    #   sim.data$te * sim.data$D + y.sig * rnorm(num.obs)

    sim.data$y = sim.data$x1 * coeffs[1] + sim.data$x2 * coeffs[2] + sim.data$x3 * coeffs[3] + sim.data$x4 * coeffs[4] +
      (sim.data$x1 - mu[1])^2 * coeffs[5] + (sim.data$x2 - mu[2])^2 * coeffs[6] + (sim.data$x3 - mu[3])^2 * coeffs[7] +
      (sim.data$x4 - mu[5])^2 * coeffs[8] +
      int.coeff * (sim.data$x2 - mu[2]) * (sim.data$x3 - mu[3]) + # add in the interaction, with the coefficient found above
      sim.data$man.d * 0.01 + # with manipulation there is a conversion between manipulation and outcome (0.01)
      sim.data$te * sim.data$D + y.sig * rnorm(num.obs)
  } else if (function.form == "alternative") {
    sim.data$y = (sim.data$x1 < (mu[1] - sig[1])) * coeffs[1] + (sim.data$x1 < (mu[1] + sig[1]/2)) * coeffs[2] +
      (sim.data$x2 < mu[2]) * coeffs[3] + (sim.data$x2 < (mu[2] + sig[2]/2)) * coeffs[4] + (sim.data$x2 < (mu[2] + sig[2])) * coeffs[5] +
      (sim.data$x2 >= (mu[2] + sig[2])) * coeffs[6] + sim.data$x3 * coeffs[7] + sim.data$x4 * coeffs[8] +
      sim.data$man.d * 0.01 + # with manipulation there is a conversion between manipulation and outcome (0.01)
      sim.data$te * sim.data$D + y.sig * rnorm(num.obs)
  } else if (function.form == "bad") {
    sim.data$y = sim.data$x1 * coeffs[1] + sim.data$x4 * coeffs[4] +
      (sim.data$x1 - mu[1])^2 * coeffs[5] + (sim.data$x4 - mu[5])^2 * coeffs[8] +
      sim.data$man.d * 0.01 + # with manipulation there is a conversion between manipulation and outcome (0.01)
      sim.data$te * sim.data$D + y.sig * rnorm(num.obs)
  } else {
    return(errorCondition("function.form must be one of base, alternative, or bad"))
  }

  print("checkpoint 2")
  # first run a typical regression discontinuity regression
  temp.capture = capture.output( {temp.rd <- summary(RDestimate(y ~ d | x1 + x2 + x3, data=sim.data))} )
  print("checkpoint 2.A")
  bd = tryCatch(IKbandwidth(sim.data$d, sim.data$y),
                warning = function(e){
                  print(paste0("dim(sim.data) ", dim(sim.data)))
                  NA},
                error = function(e){
                  print(paste0("dim(sim.data) ", dim(sim.data)))
                  NA})
  print("checkpoint 2.1")
  if(is.nan(bd) || is.na(bd)){
    print(paste0("dim(sim.data) ", dim(sim.data)))
    bd <- 1
  }
  print("checkpoint 2.2")
  mc.sims <- data.frame(
    rd.mccrary = DCdensity(sim.data$d, plot = FALSE),
    rd.late = temp.rd$coefficients[1,3],
    rd.late.obs = temp.rd$coefficients[1,2],
    rd.late.se = temp.rd$coefficients[1,4],
    rd.half = temp.rd$coefficients[2,3],
    rd.half.obs = temp.rd$coefficients[2,2],
    rd.half.se = temp.rd$coefficients[2,4],
    rd.double = temp.rd$coefficients[3,3],
    rd.double.obs = temp.rd$coefficients[3,2],
    rd.double.se = temp.rd$coefficients[3,4])

  # regular OLS and with the bandwidths
  temp.ols = summary(lm(y ~ x1 + x2 + x3 + D, data=sim.data), noprint=T)
  mc.sims$ols.ate = temp.ols$coefficients[5,1]
  mc.sims$ols.ate.se = temp.ols$coefficients[5,2]
  print("checkpoint 2.3")

  # and now a causal forest (not including d)
  set.seed(my.seed[i,2])
  ### In a small number of simulations untreated observations have an estimated propensity
  #   equal to 1 when using the default 2,000 tree forest. The ATE and ATT estimators use
  #   AIPW to correct for finite sample bias, which involves dividing by one minus the propensity
  #   for untreated observations. As a result, ATE and ATT are both NA when this occurs. The
  #   overlap-weighted estimator is unaffected, as it does not use inverse propensity weighting.
  #   To avoid having NAs for ATE and ATT, we include this loop. When there is at least one
  #   untreated observation with a propensity of 1 or a treated observation with a propensity
  #   of 0 the propensities are re-estimated using a forest with more trees. This loop adds
  #   more trees by 250 until the issue is resolved. To prevent an infinite loop, the maximum
  #   number of trees is set to 25,000.
  ###

  print("checkpoint 2.4")
  repeat {
    set.seed(my.seed[i,2])
    w.hat <- regression_forest(X = sim.data[c("x1","x2","x3")], Y = sim.data$D, num.trees = num.trees, seed = my.seed[i,2])$predictions
    if (num.trees == 25000) {
      if (sum(w.hat == 0) > 0) w.hat[w.hat == 0] <- min(w.hat[w.hat > 0])
      if (sum(w.hat == 1) > 0) w.hat[w.hat == 1] <- min(w.hat[w.hat < 1])
      break
    }
    if ((sum(w.hat==0) + sum(w.hat==1)) == 0) break
    num.trees <- num.trees + 250
  }
  print("checkpoint 2.5")
  set.seed(my.seed[i,2])
  y.hat <- regression_forest(X = sim.data[c("x1","x2","x3")], Y = sim.data$y, seed = my.seed[i,2], num.threads = 4)$predictions
  set.seed(my.seed[i,2])
  forest.out = causal_forest(X = sim.data[c("x1","x2","x3")], Y = sim.data$y, W = sim.data$D,
                             Y.hat = y.hat, W.hat = w.hat, seed = my.seed[i,2])

  temp.forest.ate = average_treatment_effect(forest.out, target.sample = "all")
  temp.forest.att = average_treatment_effect(forest.out, target.sample = "treated")
  temp.forest.overlap = average_treatment_effect(forest.out, target.sample = "overlap")
  mc.sims$forest.ate = temp.forest.ate[1]
  mc.sims$forest.ate.se = temp.forest.ate[2]
  mc.sims$forest.att = temp.forest.att[1]
  mc.sims$forest.att.se = temp.forest.att[2]
  mc.sims$forest.overlap = temp.forest.overlap[1]
  mc.sims$forest.overlap.se = temp.forest.overlap[2]
  temp.forest.ate = average_treatment_effect(forest.out, target.sample = "all", subset = abs(sim.data$d) <= bd)
  temp.forest.att = average_treatment_effect(forest.out, target.sample = "treated", subset = abs(sim.data$d) <= bd)
  temp.forest.overlap = average_treatment_effect(forest.out, target.sample = "overlap", subset = abs(sim.data$d) <= bd)
  mc.sims$forest.close.ate = temp.forest.ate[1]
  mc.sims$forest.close.ate.se = temp.forest.ate[2]
  mc.sims$forest.close.att = temp.forest.att[1]
  mc.sims$forest.close.att.se = temp.forest.att[2]
  mc.sims$forest.close.overlap = temp.forest.overlap[1]
  mc.sims$forest.close.overlap.se = temp.forest.overlap[2]


  temp.forest.ate = average_treatment_effect(forest.out, target.sample = "all", subset = abs(sim.data$d) <= (bd/2))
  temp.forest.att = average_treatment_effect(forest.out, target.sample = "treated", subset = abs(sim.data$d) <= (bd/2))
  temp.forest.overlap = average_treatment_effect(forest.out, target.sample = "overlap", subset = abs(sim.data$d) <= (bd/2))
  mc.sims$forest.very.close.ate = temp.forest.ate[1]
  mc.sims$forest.very.close.ate.se = temp.forest.ate[2]
  mc.sims$forest.very.close.att = temp.forest.att[1]
  mc.sims$forest.very.close.att.se = temp.forest.att[2]
  mc.sims$forest.very.close.overlap = temp.forest.overlap[1]
  mc.sims$forest.very.close.overlap.se = temp.forest.overlap[2]
  mc.sims$number.of.trees = num.trees

  print(paste("final dim(mc.sims) = ", dim(mc.sims), collapse = ","))

  mc.sims
}


mc_loop <- function(num.simulations = 10000, num.obs = 5000,
                    latent.corr = 0,
                    manip.per = 0.0,
                    het.slope = 0.0,
                        end.setup = FALSE, end.rho = 0.00, end.coeff = 0.02,
                    function.form = "base",
                    interaction_strength = 0.0, jump_size = 0.0,
                    x.jump = c(0,0,0),
                    out.directory, out.name) {
  mc_loop_int(num.simulations, num.obs,
              latent.corr,
              interaction_strength, jump_size,
              manip.per,
              het.slope = het.slope,
	      end.setup = end.setup, end.rho = end.rho, end.coeff = end.coeff,
              function.form = function.form,
              x.jump,
              out.directory, out.name)
}


mc_loop_int <- function(num.simulations = 10000, num.obs = 5000,
                        latent.corr,
                        interaction_strength = 0.0, jump_size = 0.0,
                        manip.per = 0.0,
                        het.slope = 0,
                        end.setup = FALSE, end.rho = 0.00, end.coeff = 0.02,
                        function.form = "base",
                        x.jump = c(0,0,0),
                        out.directory, out.name) {

  print("het.slope:")
  print(het.slope)

  ### load R libraries
  library(grf)
  library(rdd)
  library(MASS)
  library(svMisc)
  library(dplyr)
  library(logging)


  loginfo(memory.limit())
  #memory.limit(size = memory.limit()*2)
  # library(lineprof)

  print("checkpoint 3")
  options(mc.cores=18)
  # start timer
  start.clock = proc.time()

  ### setup
  random.seed <- 3780811
  set.seed(random.seed)
  my.seed = matrix(sample(1:2^15, num.simulations*2), num.simulations, 2) # randomly chosen random seed for both simulated data and for causal forests (column 2)

  mu = as.matrix(c(1.4,5.5,0.1,0.2,0.3)) # variable means
  sig = as.matrix(c(1.00, 1.50, 0.30, 0.30, 0.80)) # variable standard deviations

  if (end.setup) {
	  rho.mat = matrix(c(  1.00,-0.05,-0.30, 0.15, 0.00,
							 -0.05, 1.00, 0.20, 0.35, 0.00,
							 -0.30, 0.20, 1.00, 0.50, 0.00,
							 0.15, 0.35, 0.50, 1.00, end.rho,
							 0.00, 0.00, 0.00, end.rho, 1.00),
						 5,5) # correlation matrix
	  sigma = rho.mat * (sig %*% t(sig)) # covariance matrix

	  coeffs = as.matrix(c(0.05, -0.005, 0.01, end.coeff, # linear coefficients in determining the y variable
						   0.025, -0.01, 0.015, 0.0)) # 2d order coefficients in determining the y variable -- dropping x4 impact
	  te = 0.02

	  # determine the residual variance
	  # originally we used y.sig = 0.065, which (with no omitted variable) gives V(y) = 0.009175
	  # the residual variance = 0.4606 of total variance
	  # as we add x4, we want to adjust y.sig so that V(y) remains the same
	  x4_impact = end.coeff^2 * sig[5]^2
	  y.sig = sqrt(0.009175 * 0.4606 - x4_impact) # standard deviation of y error
  } else {
	  if (latent.corr == 0) { # uncorrelated omitted variable
		rho.mat = matrix(c(  1.00,-0.05,-0.30, 0.15, 0.00,
							 -0.05, 1.00, 0.20, 0.35, 0.00,
							 -0.30, 0.20, 1.00, 0.50, 0.00,
							 0.15, 0.35, 0.50, 1.00, 0.00,
							 0.00, 0.00, 0.00, 0.00, 1.00),
						 5,5) # correlation matrix
	  } else if (latent.corr == 1) { # low correlation omitted variable
		rho.mat = matrix(c(  1.00,-0.05,-0.30, 0.15, 0.00,
							 -0.05, 1.00, 0.20, 0.35, 0.00,
							 -0.30, 0.20, 1.00, 0.50, 0.00,
							 0.15, 0.35, 0.50, 1.00, 0.10,
							 0.00, 0.00, 0.00, 0.10, 1.00),
						 5,5) # correlation matrix
	  } else if (latent.corr == 2) { # medium correlation omitted variable
		rho.mat = matrix(c(   1.00,-0.05,-0.30, 0.15, 0.00,
							  -0.05, 1.00, 0.20, 0.35, 0.00,
							  -0.30, 0.20, 1.00, 0.50, 0.00,
							  0.15, 0.35, 0.50, 1.00, 0.20,
							  0.00, 0.00, 0.00, 0.20, 1.00),
						 5,5) # correlation matrix
	  } else if (latent.corr == 3) { # high correlation omitted variable
		rho.mat = matrix(c(   1.00,-0.05,-0.30, 0.15, 0.00,
							  -0.05, 1.00, 0.20, 0.35, 0.00,
							  -0.30, 0.20, 1.00, 0.50, 0.00,
							  0.15, 0.35, 0.50, 1.00, 0.25,
							  0.00, 0.00, 0.00, 0.25, 1.00),
						 5,5) # correlation matrix
	  }
	  sigma = rho.mat * (sig %*% t(sig)) # covariance matrix

	  coeffs = as.matrix(c(0.05, -0.005, 0.01, 0.02, # linear coefficients in determining the y variable
  						   0.025, -0.01, 0.015, -0.01)) # 2d order coefficients in determining the y variable
	  y.sig = 0.065 # standard deviation of y error
	  te = 0.02
  }
  print("checkpoint 4")
  print(proc.time() - start.clock)

  ## if there is an interaction term, compute the size of the coefficient based on the strength and reduce residual variance
  int.coeff = 0
  if (interaction_strength > 0) {
    V_int = sig[2]^2 * sig[3]^2 + 2 * sigma[2,3]^2
    V_other = coeffs[5] * (sig[1]^2 * sigma[2,3] + 2 * sigma[1,2] * sigma[1,3]) +
      coeffs[6] * 3 * sig[2]^2 * sigma[2,3] +
      coeffs[7] * 3 * sig[3]^2 * sigma[2,3] +
      te * 0.0505
    #int.coeff = sqrt(interaction_strength) * y.sig / sqrt(V_int)
    int.coeff = interaction_strength * (sqrt(V_other^2 + V_int * y.sig^2) - V_other) / V_int
    V_int.sig = sqrt(max(0, int.coeff^2 * V_int + 2 * int.coeff * V_other))
    y.sig = max(0, y.sig - V_int.sig)
  }

  ## if there is a jump term, compute the jump effect on residual variance

  jump.coeff <- 0
  if (jump_size > 0) {
    p_x1 = 0.50 # assume jump happens at mu[1]
    mu_xz = 0.025 #0.0925 # too hard to find analytically, so I simulated it
    jump.coeff <- jump_size * (sqrt(mu_xz^2 * p_x1^2 + p_x1 * y.sig^2) - mu_xz * p_x1) / p_x1
    V.jump.sig <- sqrt(max(0, jump.coeff * p_x1 * (jump.coeff + 2 * mu_xz)))
    y.sig <- max(0, y.sig - V.jump.sig)
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
    rd.mccrary = rep(0,num.simulations),
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
    forest.very.close.ate = rep(0,num.simulations),
    forest.very.close.ate.se = rep(0,num.simulations),
    forest.very.close.att = rep(0,num.simulations),
    forest.very.close.att.se = rep(0,num.simulations),
    forest.very.close.overlap = rep(0,num.simulations),
    forest.very.close.overlap.se = rep(0,num.simulations),
    number.of.trees = rep(0,num.simulations)
  )

  run_number <- as.numeric(Sys.getenv("RUN_NUMBER"))
  number_of_runs <- as.numeric(Sys.getenv("NUMBER_OF_RUNS"))

  print(paste("run_number", run_number, "number_of_runs", number_of_runs))

  simulations.per.thread <- num.simulations %/% number_of_runs

  print("checkpoint 5")
  print(proc.time() - start.clock)

  remote_fun <- function(thread_num){
    start_idx <- (thread_num * simulations.per.thread) + 1
    end_idx <- ((thread_num + 1) * simulations.per.thread)
    if(thread_num == num.simulations - 1){
      end_idx <- number_of_runs
    }
    lapply(start_idx:end_idx, function(i){
      progress(i - start_idx, max.value = end_idx - start_idx)
      simulated_data <- simulate_once(i, my.seed, mu, sigma, te, int.coeff,
                                      het.slope,
                                      num.obs, sig,
                                      function.form,
                                      x.jump, manip.per,
                                      coeffs, y.sig, interaction_strength)
      print(paste("dim(simulated_data) =", dim(simulated_data)))
      simulated_data
    })
  }


  print("checkpoint 6")
  print(proc.time() - start.clock)

  rds_file_prefix <- paste0(out.name, "-")
  if(run_number < number_of_runs){
    rds_file <- paste0(out.directory, "/", rds_file_prefix, run_number, ".rds")
    if(file.exists(rds_file)){
      print(paste(rds_file, "already exists"))
      return
    } else {
    print(paste(rds_file, "to be created"))
    mc.sims.partial <- remote_fun(run_number)
    print(paste("dim(mc.sims.partial)", dim(mc.sims.partial)))
    str(mc.sims.partial)
    print(x.jump)
    print(manip.per)
    print(run_number)
    print(paste("rds_file: ", rds_file))
    saveRDS(mc.sims.partial, file=rds_file)
    }
  } else {
    rds_pattern <- paste0(rds_file_prefix, "\\d+.rds")
    print(rds_pattern)

    files <- list.files(path = out.directory, pattern=rds_pattern)
    while(length(files) < number_of_runs){
      print(paste0("ONly ", length(files), " files found out of ", number_of_runs))
      Sys.sleep(300)
      files <- list.files(path = out.directory, pattern=rds_pattern)
      print("Files:")
      print(files)
      print("")
    }
    mc.sims.parts <- lapply(files, function(this_file){
      readRDS(paste(out.directory, this_file, sep="/"))
    })
    mc.sims <- bind_rows(mc.sims.parts)



    print("dim(mc.sims)")
    print(dim(mc.sims))
    print("mc.sims")
    str(mc.sims)


    method.names = c("RDD","RDD_half_bw","RDD_double_bw","OLS",
                     "Forest_ATE","Forest_ATT","Forest_overlap",
                     "Forest_close_ATE","Forest_close_ATT","Forest_close_overlap",
                     "Forest_very_close_ATE","Forest_very_close_ATT","Forest_very_close_overlap",
                     "McCrary")
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
    mc.out$bias[11] = (mean(mc.sims$forest.very.close.ate - te)) / te
    mc.out$bias[12] = (mean(mc.sims$forest.very.close.att - te)) / te
    mc.out$bias[13] = (mean(mc.sims$forest.very.close.overlap - te)) / te
    mc.out$bias[14] = NA

    # root mean squared error (RMSE) as a percentage of the treatment effect
    mc.out$rmse[1] = sqrt(mean((mc.sims$rd.late - te)^2)) / te
    mc.out$rmse[2] = sqrt(mean((mc.sims$rd.half - te)^2)) / te
    mc.out$rmse[3] = sqrt(mean((mc.sims$rd.double - te)^2)) / te
    mc.out$rmse[4] = sqrt(mean((mc.sims$ols.ate - te)^2)) / te
    mc.out$rmse[5] = sqrt(mean((mc.sims$forest.ate - te)^2)) / te
    mc.out$rmse[6] = sqrt(mean((mc.sims$forest.att - te)^2)) / te
    mc.out$rmse[7] = sqrt(mean((mc.sims$forest.overlap - te)^2)) / te
    mc.out$rmse[8] = sqrt(mean((mc.sims$forest.close.ate - te)^2)) / te
    mc.out$rmse[9] = sqrt(mean((mc.sims$forest.close.att - te)^2)) / te
    mc.out$rmse[10] = sqrt(mean((mc.sims$forest.close.overlap - te)^2)) / te
    mc.out$rmse[11] = sqrt(mean((mc.sims$forest.very.close.ate - te)^2)) / te
    mc.out$rmse[12] = sqrt(mean((mc.sims$forest.very.close.att - te)^2)) / te
    mc.out$rmse[13] = sqrt(mean((mc.sims$forest.very.close.overlap - te)^2)) / te
    mc.out$rmse[14] = NA

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
    mc.out$coverage[11] = 1 - sum((abs(mc.sims$forest.very.close.ate - te) / mc.sims$forest.very.close.ate.se) <= coverage.t) / num.simulations
    mc.out$coverage[12] = 1 - sum((abs(mc.sims$forest.very.close.att - te) / mc.sims$forest.very.close.att.se) <= coverage.t) / num.simulations
    mc.out$coverage[13] = 1 - sum((abs(mc.sims$forest.very.close.overlap - te) / mc.sims$forest.very.close.overlap.se) <= coverage.t) / num.simulations
    mc.out$coverage[14] = sum(mc.sims$rd.mccrary <= 0.05) / num.simulations

    # multiply by 100 to be percentages
    mc.out$bias <- mc.out$bias * 100
    mc.out$rmse <- mc.out$rmse * 100
    mc.out$coverage <- mc.out$coverage * 100

    print("checkpoint 7")
    print(proc.time() - start.clock)

    if (het.slope != 0.00) {
      hte.out <- data.frame(Sample = c("Full","Bandwidth","Half-Bandwidth"),
                            Bias = 100 * c(mean(mc.sims$hte.bias),mean(mc.sims$hte.bias.close),mean(mc.sims$hte.bias.very.close)),
                            RMSE = 100 * c(mean(mc.sims$hte.rmse),mean(mc.sims$hte.rmse.close),mean(mc.sims$hte.rmse.very.close)),
                            Slope = c(mean(mc.sims$hte.slope),mean(mc.sims$hte.slope.close),mean(mc.sims$hte.slope.very.close)),
                            Rsquared = c(mean(mc.sims$hte.rsquare),mean(mc.sims$hte.rsquare.close),mean(mc.sims$hte.rsquare.very.close)))
      save(mc.sims, mc.out, hte.out,
           file = paste(out.directory,paste0(out.name,".Rdata"),sep="/"))
      write.csv(mc.sims, file = paste(out.directory,paste0(out.name,"_sims",".csv"),sep="/"))
      write.csv(mc.out, file = paste(out.directory,paste0(out.name,"_bias",".csv"),sep="/"))
      write.csv(hte.out, file = paste(out.directory,paste0(out.name,"_htes",".csv"),sep="/"))
    } else {
      save(mc.sims, mc.out,
           file = paste(out.directory,paste0(out.name,".Rdata"),sep="/"))
      write.csv(mc.sims, file = paste(out.directory,paste0(out.name,"_sims",".csv"),sep="/"))
      write.csv(mc.out, file = paste(out.directory,paste0(out.name,"_bias",".csv"),sep="/"))
    }

    print("checkpoint omega")
    print(proc.time() - start.clock)

  }
}
