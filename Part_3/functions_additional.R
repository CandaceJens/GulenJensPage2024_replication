#' Additional functions used in 
#' Applying Causal Forest in Corporate Finance
#' Gulen, Jens, and Page (2019)
#' 
#' 

#' mean.by
#' 
#' Computes means for different bins around a threshold
#' 
#' @param x The forcing variable
#' @param y The outcome variable
#' @param n The number of equally-spaced groups for which means are computed on
#' each side of a threshold defined in q.
#' @param q Takes 3 values: the first is the smallest value of x, the second
#' is the threshold, and the third is the max.
#' 
#' @return y.mean A length n*2 data frame. Each row provides the minimum
#' value of the forcing variable (x.min), maximum value of the forcing
#' variable (x.max), an x value for the midpoint of the group (x), the count of observations
#' in the group (count), the mean for the group (mean), the standard deviation
#' for the group (sd), the standard error of the mean (se), and the multiple 
#' for a 95% confidence interval (ci.mult).

mean.by <- function(x,y,n,q, cum=FALSE) {
  x.seq = c(seq(q[1],q[2],length=n+1), seq(q[2] + (q[3] - q[2]) / n, q[3], length=n));
  y.mean = data.frame(x.min = rep(0,n*2), x.max = rep(0,n*2), x = rep(0,n*2), 
                      count = rep(0,n*2), mean = rep(0,n*2), sd = rep(0,n*2), se = rep(0,n*2));
  for (i in 1:(n*2)) {
    if (cum) {
      if (i <= n) {
        y.mean$x.min[i] = x.seq[i]; y.mean$x.max[i] = x.seq[i+1]; y.mean$x[i] = x.seq[i];
        y.mean$count[i] = length(x[between(x,x.seq[i],q[2])]);
        y.mean$mean[i] = mean(y[between(x,x.seq[i],q[2])]); y.mean$sd[i] = sd(y[between(x,x.seq[i],q[2])]);
        y.mean$se[i] = y.mean$sd[i] / sqrt(y.mean$count[i] - 1);
        y.q = quantile(y[between(x,x.seq[i],q[2])], c(0.25, 0.50, 0.75))
        y.mean$quart1[i] = y.q[1]
        y.mean$median[i] = y.q[2]
        y.mean$quart3[i] = y.q[3]
      } else {
        y.mean$x.min[i] = x.seq[i]; y.mean$x.max[i] = x.seq[i+1]; y.mean$x[i] = x.seq[i+1];
        y.mean$count[i] = length(x[between(x,q[2],x.seq[i+1])]);
        y.mean$mean[i] = mean(y[between(x,q[2],x.seq[i+1])]); y.mean$sd[i] = sd(y[between(x,q[2],x.seq[i+1])]);
        y.mean$se[i] = y.mean$sd[i] / sqrt(y.mean$count[i] - 1);
        y.q = quantile(y[between(x,q[2],x.seq[i+1])], c(0.25, 0.50, 0.75))
        y.mean$quart1[i] = y.q[1]
        y.mean$median[i] = y.q[2]
        y.mean$quart3[i] = y.q[3]
      }
    } else {
      y.mean$x.min[i] = x.seq[i]; y.mean$x.max[i] = x.seq[i+1]; y.mean$x[i] = (x.seq[i] + x.seq[i+1]) / 2;
      y.mean$count[i] = length(x[between(x,x.seq[i],x.seq[i+1])]);
      y.mean$mean[i] = mean(y[between(x,x.seq[i],x.seq[i+1])]); y.mean$sd[i] = sd(y[between(x,x.seq[i],x.seq[i+1])]);
      y.mean$se[i] = y.mean$sd[i] / sqrt(y.mean$count[i] - 1);
      y.q = quantile(y[between(x,x.seq[i],x.seq[i+1])], c(0.25, 0.50, 0.75))
      y.mean$quart1[i] = y.q[1]
      y.mean$median[i] = y.q[2]
      y.mean$quart3[i] = y.q[3]
    }
  }
  y.mean
}

#' diff.by
#' 
#' Computest differences in means for observations in an increasing
#' window around a threshold of 0.
#' 
#' @param x The forcing variable (x = 0 is the threshold)
#' @param y The outcome variable
#' @param n The number of equally-spaced groups for which differences in means are compute
#' @param max The maximum distance from the threshold for which means are compared
#' 
#' @return y.diff A length n data frame. Each row provides the minimum
#' value of the forcing variable (x.min), maximum value of the forcing
#' variable (x.max), an x value for the window (x), the count of observations
#' in the window (count), the count of observations with x < 0 in the window
#' (count.neg), the count of observations with x >= 0 in the window (count.pos),
#' the difference in mean for the groups above and below x = 0 (mean), the
#' standard error of the difference (se), the t-statistic comparing the
#' difference in means to 0 (t), the p-value comparing the difference
#' to 0 (p), and the multiple for a 95% confidence interval (ci.mult).

diff.by <- function(x, y, n, max, cum = FALSE) {
  x.seq = seq(0, max, length = n+1)
  y.diff = data.frame(x.min = rep(0,n), x.max = rep(0,n), x = rep(0,n), 
                      count = rep(0,n), count.neg = rep(0,n), count.pos = rep(0,n),
                      mean = rep(0,n), se = rep(0,n),
                      t = rep(0,n), p = rep(0,n));
  if (cum) {
    for (i in 1:n) {
      y.diff$x.min[i] = -x.seq[i+1]; y.diff$x.max[i] = x.seq[i+1]; y.diff$x[i] = x.seq[i+1]
      y.diff$count.neg[i] = length(x[between(x,-x.seq[i+1],0)])
      y.diff$count.pos[i] = length(x[between(x,0,x.seq[i+1])])
      y.diff$count[i] = y.diff$count.neg[i] + y.diff$count.pos[i]
      y.diff$mean[i] = mean(y[between(x,-x.seq[i+1],0)]) - mean(y[between(x,0,x.seq[i+1])])
      y.diff$se[i] = sqrt(var(y[between(x,-x.seq[i+1], 0)]) / (length(x[between(x,-x.seq[i+1], 0)]) - 1) + 
                            var(y[between(x, 0, x.seq[i+1])]) / (length(x[between(x, 0, x.seq[i+1])]) - 1))
      y.diff$t[i] = y.diff$mean[i] / y.diff$se[i]
      y.diff$p[i] = pt(-abs(y.diff$t[i]), df = y.diff$count[i] - 2) * 2
    }
  } else {
    for (i in 1:n) {
      y.diff$x.min[i] = x.seq[i]; y.diff$x.max[i] = x.seq[i+1]; y.diff$x[i] = x.seq[i + 1]
      y.diff$count.neg[i] = length(x[between(x,-x.seq[i+1],-x.seq[i])])
      y.diff$count.pos[i] = length(x[between(x,x.seq[i],x.seq[i+1])])
      y.diff$count[i] = y.diff$count.neg[i] + y.diff$count.pos[i]
      y.diff$mean[i] = mean(y[between(x,-x.seq[i+1],-x.seq[i])]) - mean(y[between(x,x.seq[i],x.seq[i+1])])
      y.diff$se[i] = sqrt(var(y[between(x,-x.seq[i+1],-x.seq[i])]) / (length(x[between(x,-x.seq[i+1],-x.seq[i])]) - 1) + 
                            var(y[between(x, x.seq[i], x.seq[i+1])]) / (length(x[between(x, x.seq[i], x.seq[i+1])]) - 1))
      y.diff$t[i] = y.diff$mean[i] / y.diff$se[i]
      y.diff$p[i] = pt(-abs(y.diff$t[i]), df = y.diff$count[i] - 2) * 2
    }
  }
  y.diff
}


#' causal_forest_wrapper
#' A function that tunes and then estimates a causal forest. See the grf documentation for variable definitions.
causal_forest_wrapper <- function(X, Y, W, Y.hat = NULL, W.hat = NULL, honesty = TRUE, num.trees = 1000, 
                                  tune.num.trees = 200, tune.num.reps = 50, tune.num.draws = 1000,
                                  min.node.size = NULL, sample.fraction = NULL, mtry = NULL, alpha = NULL, imbalance.penalty = NULL,
                                  honesty.fraction = NULL, honesty.prune.leaves = NULL, clusters = NULL,
                                  seed = NULL) {
  if (!is.null(sample.fraction) | 
      !is.null(mtry) | 
      !is.null(min.node.size) | 
      !is.null(honesty.fraction) | 
      !is.null(honesty.prune.leaves) | 
      !is.null(alpha) | 
      !is.null(imbalance.penalty)) {
    tune.parameters = c()
    if (is.null(sample.fraction)) tune.parameters = c(tune.parameters, "sample.fraction")
    if (is.null(mtry)) tune.parameters = c(tune.parameters, "mtry")
    if (is.null(min.node.size)) tune.parameters = c(tune.parameters, "min.node.size")
    if (is.null(honesty.fraction)) tune.parameters = c(tune.parameters, "honesty.fraction")
    if (is.null(honesty.prune.leaves)) tune.parameters = c(tune.parameters, "honesty.prune.leaves")
    if (is.null(alpha)) tune.parameters = c(tune.parameters, "alpha")
    if (is.null(imbalance.penalty)) tune.parameters = c(tune.parameters, "imbalance.penalty")
  } else {
    tune.parameters = "all"
  }
  set.seed(seed)
  forest.tune = tune_causal_forest(X = X, Y = Y, W = W, Y.hat = Y.hat, W.hat = W.hat,
                                       clusters = clusters,
                                       sample.fraction = sample.fraction,
                                       mtry = mtry,
                                       alpha = alpha,
                                       imbalance.penalty = imbalance.penalty,
                                       min.node.size = min.node.size,
                                       honesty = honesty,
                                       honesty.fraction = honesty.fraction,
                                       tune.num.trees = tune.num.trees,
                                       tune.num.reps = tune.num.reps,
                                       tune.num.draws = tune.num.draws,
                                       tune.parameters = tune.parameters
  )$params
  
  if (is.null(sample.fraction)) sample.fraction = as.numeric(forest.tune["sample.fraction"])
  if (is.null(mtry)) mtry = as.numeric(forest.tune["mtry"])
  if (is.null(min.node.size)) min.node.size = as.numeric(forest.tune["min.node.size"])
  if (is.null(honesty.fraction)) honesty.fraction = as.numeric(forest.tune["honesty.fraction"])
  if (is.null(honesty.prune.leaves)) honesty.prune.leaves = as.numeric(forest.tune["honesty.prune.leaves"])
  if (is.null(alpha)) alpha = as.numeric(forest.tune["alpha"])
  if (is.null(imbalance.penalty)) imbalance.penalty = as.numeric(forest.tune["imbalance.penalty"])
  
  
  set.seed(seed)
  forest = causal_forest(X, Y, W, Y.hat = Y.hat, W.hat = W.hat,
                             num.trees = num.trees,
                             seed = seed,
                             honesty = honesty,
                             clusters = clusters,
                             # the below all come from the tuning done above #
                             min.node.size = min.node.size,
                             sample.fraction = sample.fraction,
                             mtry = mtry,
                             alpha = alpha,
                             imbalance.penalty = imbalance.penalty,
                             honesty.fraction = honesty.fraction,
                             honesty.prune.leaves = honesty.prune.leaves
  )
  
  return(forest)
  
}

#' regression_forest_wrapper
#' A function that tunes and then estimates a causal forest. See the grf documentation for variable definitions.
regression_forest_wrapper <- function(X, Y, honesty = TRUE, num.trees = 1000,
                                      tune.num.trees = 200, tune.num.reps = 50, tune.num.draws = 1000,
                                  min.node.size = NULL, sample.fraction = NULL, mtry = NULL, alpha = NULL, imbalance.penalty = NULL,
                                  honesty.fraction = NULL, honesty.prune.leaves = NULL, clusters = NULL,
                                  seed = NULL) {
  if (!is.null(sample.fraction) | 
      !is.null(mtry) | 
      !is.null(min.node.size) | 
      !is.null(honesty.fraction) | 
      !is.null(honesty.prune.leaves) | 
      !is.null(alpha) | 
      !is.null(imbalance.penalty)) {
    tune.parameters = c()
    if (is.null(sample.fraction)) tune.parameters = c(tune.parameters, "sample.fraction")
    if (is.null(mtry)) tune.parameters = c(tune.parameters, "mtry")
    if (is.null(min.node.size)) tune.parameters = c(tune.parameters, "min.node.size")
    if (is.null(honesty.fraction)) tune.parameters = c(tune.parameters, "honesty.fraction")
    if (is.null(honesty.prune.leaves)) tune.parameters = c(tune.parameters, "honesty.prune.leaves")
    if (is.null(alpha)) tune.parameters = c(tune.parameters, "alpha")
    if (is.null(imbalance.penalty)) tune.parameters = c(tune.parameters, "imbalance.penalty")
  } else {
    tune.parameters = "all"
  }
  set.seed(seed)
  forest.tune = tune_regression_forest(X = X, Y = Y,
                                       clusters = clusters,
                                       sample.fraction = sample.fraction,
                                       mtry = mtry,
                                       alpha = alpha,
                                       imbalance.penalty = imbalance.penalty,
                                       min.node.size = min.node.size,
                                       honesty = honesty,
                                       honesty.fraction = honesty.fraction,
                                       tune.num.trees = tune.num.trees,
                                       tune.num.reps = tune.num.reps,
                                       tune.num.draws = tune.num.draws,
                                       tune.parameters = tune.parameters
                                       )$params

  if (is.null(sample.fraction)) sample.fraction = as.numeric(forest.tune["sample.fraction"])
  if (is.null(mtry)) mtry = as.numeric(forest.tune["mtry"])
  if (is.null(min.node.size)) min.node.size = as.numeric(forest.tune["min.node.size"])
  if (is.null(honesty.fraction)) honesty.fraction = as.numeric(forest.tune["honesty.fraction"])
  if (is.null(honesty.prune.leaves)) honesty.prune.leaves = as.numeric(forest.tune["honesty.prune.leaves"])
  if (is.null(alpha)) alpha = as.numeric(forest.tune["alpha"])
  if (is.null(imbalance.penalty)) imbalance.penalty = as.numeric(forest.tune["imbalance.penalty"])
  
  
  set.seed(seed)
  forest = regression_forest(X, Y,
                         num.trees = num.trees,
                         seed = seed,
                         honesty = honesty,
                         clusters = clusters,
                         # the below all come from the tuning done above #
                         min.node.size = min.node.size,
                         sample.fraction = sample.fraction,
                         mtry = mtry,
                         alpha = alpha,
                         imbalance.penalty = imbalance.penalty,
                         honesty.fraction = honesty.fraction,
                         honesty.prune.leaves = honesty.prune.leaves
                         )

  return(forest)
}

#' instrumental_forest_wrapper
#' A function that tunes and then estimates a causal forest. See the grf documentation for variable definitions.
instrumental_forest_wrapper <- function(X, Y, W, Z, Y.hat = NULL, W.hat = NULL, Z.hat= NULL, honesty = TRUE, num.trees = 1000, 
                                  tune.num.trees = 200, tune.num.reps = 50, tune.num.draws = 1000,
                                  min.node.size = NULL, sample.fraction = NULL, mtry = NULL, alpha = NULL, imbalance.penalty = NULL,
                                  honesty.fraction = NULL, honesty.prune.leaves = NULL, clusters = NULL,
                                  seed = NULL) {
  if (!is.null(sample.fraction) | 
      !is.null(mtry) | 
      !is.null(min.node.size) | 
      !is.null(honesty.fraction) | 
      !is.null(honesty.prune.leaves) | 
      !is.null(alpha) | 
      !is.null(imbalance.penalty)) {
    tune.parameters = c()
    if (is.null(sample.fraction)) tune.parameters = c(tune.parameters, "sample.fraction")
    if (is.null(mtry)) tune.parameters = c(tune.parameters, "mtry")
    if (is.null(min.node.size)) tune.parameters = c(tune.parameters, "min.node.size")
    if (is.null(honesty.fraction)) tune.parameters = c(tune.parameters, "honesty.fraction")
    if (is.null(honesty.prune.leaves)) tune.parameters = c(tune.parameters, "honesty.prune.leaves")
    if (is.null(alpha)) tune.parameters = c(tune.parameters, "alpha")
    if (is.null(imbalance.penalty)) tune.parameters = c(tune.parameters, "imbalance.penalty")
  } else {
    tune.parameters = "all"
  }
  set.seed(seed)
  forest.tune = tune_instrumental_forest(X = X, Y = Y, W = W, Z = Z, Y.hat = Y.hat, W.hat = W.hat, Z.hat = Z.hat,
                                   clusters = clusters,
                                   sample.fraction = sample.fraction,
                                   mtry = mtry,
                                   alpha = alpha,
                                   imbalance.penalty = imbalance.penalty,
                                   min.node.size = min.node.size,
                                   honesty = honesty,
                                   honesty.fraction = honesty.fraction,
                                   tune.num.trees = tune.num.trees,
                                   tune.num.reps = tune.num.reps,
                                   tune.num.draws = tune.num.draws,
                                   tune.parameters = tune.parameters
  )$params
  
  if (is.null(sample.fraction)) sample.fraction = as.numeric(forest.tune["sample.fraction"])
  if (is.null(mtry)) mtry = as.numeric(forest.tune["mtry"])
  if (is.null(min.node.size)) min.node.size = as.numeric(forest.tune["min.node.size"])
  if (is.null(honesty.fraction)) honesty.fraction = as.numeric(forest.tune["honesty.fraction"])
  if (is.null(honesty.prune.leaves)) honesty.prune.leaves = as.numeric(forest.tune["honesty.prune.leaves"])
  if (is.null(alpha)) alpha = as.numeric(forest.tune["alpha"])
  if (is.null(imbalance.penalty)) imbalance.penalty = as.numeric(forest.tune["imbalance.penalty"])
  
  
  set.seed(seed)
  forest = instrumental_forest(X, Y, W, Z, Y.hat = Y.hat, W.hat = W.hat, Z.hat = Z.hat,
                         num.trees = num.trees,
                         seed = seed,
                         honesty = honesty,
                         clusters = clusters,
                         # the below all come from the tuning done above #
                         min.node.size = min.node.size,
                         sample.fraction = sample.fraction,
                         mtry = mtry,
                         alpha = alpha,
                         imbalance.penalty = imbalance.penalty,
                         honesty.fraction = honesty.fraction,
                         honesty.prune.leaves = honesty.prune.leaves
  )
  
  return(forest)
  
}


#' ate_table
#' Compute average treatment effects (ATE, ATC, ATT, and ATO) for different subsets.

ate_table = function(forest, subsets = NULL, subset.names = NULL, bind = NULL, N.full = NULL) {
  
  if (!is.null(subsets)) {
    n.subsets = dim(subsets)[2]
    if (is.null(subset.names) | (length(subset.names)!=n.subsets)) { # if no names given for subsets, throw an error
      print("Please provide subset names with subsets or wrong number of names given")
      return(NULL)
    }
    type.order = c("ATE","ATT","ATC","ATO")
    result = c()
    for (i in 1:n.subsets) {
      ate = average_treatment_effect(forest, target.sample = "all", subset = subsets[,i])
      atc = average_treatment_effect(forest, target.sample = "control", subset = subsets[,i])
      att = average_treatment_effect(forest, target.sample = "treated", subset = subsets[,i])
      ato = average_treatment_effect(forest, target.sample = "overlap", subset = subsets[,i])
      ests = c(ate[1], att[1], atc[1], ato[1])
      ses = c(ate[2], att[2], atc[2], ato[2])
      N.sub = sum(subsets[,i])
      N.bind = sum(bind[subsets[,i]])
      result = rbind(result,
                     data.frame(Subset = rep(subset.names[i],4),
                                Type = type.order,
                                Estimate = ests,
                                Standard.Error = ses,
                                T.stat = ests / ses,
                                P.value = 2 * pt(-abs(ests / ses), df = N.full),
                                N = rep(N.sub, 4),
                                N.per = rep(N.sub / N.full,4) * 100,
                                bind.per = rep(N.bind / N.sub * 100,4)))
    }
  } else {
    ate = average_treatment_effect(forest, target.sample = "all")
    atc = average_treatment_effect(forest, target.sample = "control")
    att = average_treatment_effect(forest, target.sample = "treated")
    ato = average_treatment_effect(forest, target.sample = "overlap")
    type.order = c("ATE","ATT","ATC","ATO")
    ests = c(ate[1], att[1], atc[1], ato[1])
    ses = c(ate[2], att[2], atc[2], ato[2])
    result = data.frame(Subset = rep("Full",4),
                        Type = type.order,
                        Estimate = ests,
                        Standard.Error = ses,
                        T.stat = ests / ses,
                        P.value = 2 * pt(-abs(ests / ses), df = N.full),
                        N = rep(N.full, 4),
                        N.per = rep(100,4),
                        bind.per = rep(sum(bind) / N.full * 100,4))
  }
  rownames(result) = paste(result$Subset,result$Type,sep=" ")
  result = result[c("Estimate","Standard.Error","T.stat","P.value","N","N.per","bind.per")]
  return(result)
  
}



#' my_variable_importance 
#' Takes in the feature data (the Xs that make up the forest),
#' the already computed forest object itself, and the number of perturbations
#' to be made for each feature in X (200 is the default, and in limited testing
#' I haven't seen a reason to do more). 
#' 
#' The function takes each feature individually and re-orders (perturbs) it among the observations,
#' leaving all of the other features alone. A perturbed observation has all of its
#' features the same, except for the current feature, in which that observed value
#' has randomly been replaced with the value from a different observation.
#'
#' Variable importance is ranked by the variance of predicted values caused
#' by the perturbations.
#'
#' Inputs
#' @param X the data that is used to compute importance
#' @param forest the causal forest generated by the grf function
#' @param num.perturb the number of perturbations of the data for each feature in X (200 by default)
#' @param seed the seed value for the random number generator, so that we can replicate a specific run (NULL by default)

my_variable_importance <- function(X, industry = NULL, industry.name = NULL, forest, num.perturb = 200, seed = NULL) {
  library(data.table)
  X.names <- names(X)
  
  if (!is.null(industry)) {
    X.names.ind = X.names[X.names %like% industry.name]
    X.names.short = X.names[!(X.names %like% industry.name)]
    X <- X[order(industry),]
    industry <- industry[order(industry)]
  } else {
    X.names.short = X.names
  }
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  base.predictions <- predict(forest, X)$predictions
  sample.length = length(base.predictions)
  
  meanDiff <- c()
  absMeanDiff <- c()
  absMeanDiff.sd <- c()
  diffVar <- c()
  diffVar.sd <- c()
  iter = 1
  len.X.names = length(X.names.short)
  if (is.null(industry))
    len.X.names = len.X.names + 1
  for (variable in X.names.short) {
    variableMeanDiff <- c()
    variableAbsMeanDiff <- c()
    variableDiffVar <- c()
    perturb.data <- X
    for (iter in 1:num.perturb) {
      perturb.data[,variable] <- sample(X[[variable]], sample.length)
      predictions <- predict(forest, perturb.data[X.names])$predictions
      diff.predictions <- predictions - base.predictions
      variableMeanDiff <- c(variableMeanDiff, mean(diff.predictions))
      variableAbsMeanDiff <- c(variableAbsMeanDiff, mean(abs(diff.predictions)))
      variableDiffVar <- c(variableDiffVar, var(diff.predictions))
    }
    meanDiff <- c(meanDiff, mean(variableMeanDiff))
    absMeanDiff <- c(absMeanDiff, mean(variableAbsMeanDiff))
    absMeanDiff.sd <- c(absMeanDiff.sd, sd(variableAbsMeanDiff))
    diffVar <- c(diffVar, mean(variableDiffVar))
    diffVar.sd <- c(diffVar.sd, sd(variableDiffVar))
    
    iter = iter + 1
    
  }
  
  if (!is.null(industry)) {
    variableMeanDiff <- c()
    variableAbsMeanDiff <- c()
    variableDiffVar <- c()
    perturb.data <- X
    for (iter in 1:num.perturb) {
      perturb.industry <- sample(industry,sample.length)
      if (industry.name == "ff12") {
        perturb.data = ff12.indicators(perturb.data, perturb.industry)
      } else if (industry.name == "ff30") {
        perturb.data = ff30.indicators(perturb.data, perturb.industry)
      }
      predictions <- predict(forest, perturb.data[X.names])$predictions
      diff.predictions <- predictions - base.predictions
      variableMeanDiff <- c(variableMeanDiff, mean(diff.predictions))
      variableAbsMeanDiff <- c(variableAbsMeanDiff, mean(abs(diff.predictions)))
      variableDiffVar <- c(variableDiffVar, var(diff.predictions))
    }
    meanDiff <- c(meanDiff, mean(variableMeanDiff))
    absMeanDiff <- c(absMeanDiff, mean(variableAbsMeanDiff))
    absMeanDiff.sd <- c(absMeanDiff.sd, sd(variableAbsMeanDiff))
    diffVar <- c(diffVar, mean(variableDiffVar))
    diffVar.sd <- c(diffVar.sd, sd(variableDiffVar))
    out.df <- data.frame('variable'=c(X.names.short,industry.name), 'meanDiff'=meanDiff, 'absMeanDiff'=absMeanDiff, 'absMeanDiff.sd'=absMeanDiff.sd,
                         'diffVar'=diffVar, 'diffVar.sd'=diffVar.sd, 'diffSD'=sqrt(diffVar))
  } else {
    out.df <- data.frame('variable'=X.names, 'meanDiff'=meanDiff, 'absMeanDiff'=absMeanDiff, 'absMeanDiff.sd'=absMeanDiff.sd,
                         'diffVar'=diffVar, 'diffVar.sd'=diffVar.sd, 'diffSD'=sqrt(diffVar))
  }
  out.df[order(out.df$diffVar, decreasing=TRUE),]
}

##### do descriptive statistics on two samples along with t-test of differences in means #####
summary.compare <- function(x, compare = NULL, na.rm=F) {
  x = as.data.frame(x)
  x.names = names(x)
  if (is.null(compare)) {
    out = matrix(0,dim(x)[2],6)
    for (c in 1:length(x.names)) {
      x.c = as.data.frame(x[,x.names[c]])
      q.c = quantile(x[,x.names[c]],c(0.25,0.5,0.75), na.rm=na.rm)
      out[c,1] = mean(x[,x.names[c]], na.rm=na.rm)
      out[c,2] = q.c[1]
      out[c,3] = q.c[2]
      out[c,4] = q.c[3]
      out[c,5] = sd(x[,x.names[c]], na.rm = na.rm)
      out[c,6] = sum(!is.na(x[,x.names[c]]))
    }
    rownames(out) = x.names
    colnames(out) = c("Mean","25%","50%","75%","SD","N")
    return(out)
  } else {
    if (length(unique(compare)) > 2) {
      print("Let's keep it simple for now, okay? Just 1 comparison")
    } else {
      out = matrix(0,dim(x)[2],14)#15) # dropping the t-test of differences
      comp = unique(compare)
      for (c in 1:length(x.names)) {
        q.c = quantile(x[,x.names[c]][compare==comp[1]],c(0.25,0.5,0.75), na.rm=na.rm)
        out[c,1] = comp[1]
        out[c,2] = mean(x[,x.names[c]][compare==comp[1]], na.rm=na.rm)
        out[c,3] = q.c[1]
        out[c,4] = q.c[2]
        out[c,5] = q.c[3]
        out[c,6] = sd(x[,x.names[c]][compare==comp[1]], na.rm=na.rm)
        out[c,7] = sum(!is.na(x[,x.names[c]][compare==comp[1]]))
        q.c = quantile(x[,x.names[c]][compare==comp[2]],c(0.25,0.5,0.75), na.rm=na.rm)
        out[c,8] = comp[2]
        out[c,9] = mean(x[,x.names[c]][compare==comp[2]], na.rm=na.rm)
        out[c,10] = q.c[1]
        out[c,11] = q.c[2]
        out[c,12] = q.c[3]
        out[c,13] = sd(x[,x.names[c]][compare==comp[2]], na.rm=na.rm)
        out[c,14] = sum(!is.na(x[,x.names[c]][compare==comp[2]]))
        #out[c,15] = (out[c,2] - out[c,9]) / sqrt(out[c,6]^2/out[c,7]+out[c,13]^2/out[c,14])
      }
      rownames(out) = x.names
      colnames(out) = c("Group","Mean","25%","50%","75%","SD","N","Group","Mean","25%","50%","75%","SD","N")#,"T-test")
      return(out)
    }
  }
}

means.test <- function(x,w=NULL) {
  x = as.data.frame(x)
  if (is.null(w)) {
    print("Please include a variable to split data on for comparison")
  } else {
    x.names = names(x)
    out = matrix(0,dim(x)[2],8)
    comp = unique(w)
    for (c in 1:length(x.names)) {
      x.1 = x[,x.names[c]][w==comp[1]]
      x.1 = x.1[!is.na(x.1)]
      x.2 = x[,x.names[c]][w==comp[2]]
      x.2 = x.2[!is.na(x.2)]
      x.mean.test = t.test(x.1,x.2)
      med.x = median(x[,x.names[c]], na.rm=T)
      x.med.table = matrix(c(sum(x.1>med.x),sum(x.1<=med.x),sum(x.2>med.x),sum(x.2<=med.x)),2,2)
      x.med.test = chisq.test(x.med.table)
      out[c,1] = mean(x.1)
      out[c,2] = median(x.1)
      out[c,3] = mean(x.2)
      out[c,4] = median(x.2)
      out[c,5] = x.mean.test$statistic
      out[c,6] = x.mean.test$p.value
      out[c,7] = x.med.test$statistic
      out[c,8] = x.med.test$p.value
    }
    rownames(out) = x.names
    colnames(out) = c("Mean.1","Median.1","Mean.2","Median.2",
                      "Mean.test","Mean.p.value","Median.test","Median.p.value")
    return(out)
  }
}
