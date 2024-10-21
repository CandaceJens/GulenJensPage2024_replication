majorMinorPatch <- paste(R.version[c("major", "minor")], collapse=".")
majorMinor <- gsub("(.*)\\..*", "\\1", majorMinorPatch)
print(paste0("majorMinor=", majorMinor))

clusterp <- !(Sys.getenv("COMPUTERNAME") %in% c("SANCTUS", "BUS-CJENS", "LITTLEGUY"))

if(clusterp){
  myLibPath <- paste0("/home/cejens/RLibrary/", majorMinor)
  print(paste0("export R_LIBS_USER=\"", myLibPath, "\""))
  dir.create(myLibPath, showWarnings = FALSE)
  Sys.setenv(R_LIBS_USER=myLibPath)
} else {
  Sys.setenv(BATCH_NUMBER = "1",
             RUN_NUMBER = "100",
             NUMBER_OF_RUNS = "100")
}

rm(list=setdiff(ls(), c("clusterp", "myLibPath")))

if(clusterp){
  .libPaths(myLibPath)
}
print(.libPaths())
REPOSITORY <- "https://cran.us.r-project.org"
REPOSITORY <- "https://cran.rstudio.com/"

print(Sys.getenv("PATH"))


required_pkgs <- c("rlang", "RcppEigen", "grf", "rdd", "svMisc", "logging", "dplyr")
for(pkg in required_pkgs){
  if(!require(pkg, character.only = TRUE, quietly = TRUE)){
    print(paste("Installing", pkg))
    install.packages(pkg, repos=REPOSITORY, lib=myLibPath)
  }
  print(paste0(pkg, " version: ", packageVersion(pkg)))
}
# require("tidyverse")
library(dplyr)


# possibly confusingly, batch number indicates which set of simulations we are doing,
# while run number indicates which piece of the set of simulations
# I.e., we will typically run a simulation on 20-100 processes at once and then
# assemble those results -- each process has its own run number, but they are the same batch

batch_number <- as.numeric(Sys.getenv("BATCH_NUMBER"))
if(is.na(batch_number)){
  stop("No batch number found")
}

run_number <- as.numeric(Sys.getenv("RUN_NUMBER"))
if(is.na(run_number)){
  stop("No run number found")
}




# directory where all of the results will be stored and where loop script is
out.directory <- "."

# load loop script
source(paste(out.directory, "mc_for_loop_6.R", sep = "/"))

out.directory <- paste0(out.directory, "/batch", batch_number)
print(paste("output directory:", out.directory))
if(!dir.exists(out.directory)){
  print(paste0("directory \"", out.directory, "\" not found -- creating"))
  dir.create(out.directory)
} else {
  print("Using existing directory")
}


# load loop script


###
# manip.per
# het.multiplier

if(batch_number < 6){
  specs_df <- expand.grid(manip.per = c(0.05, 0.1, 0.2, 0.4, 0.7),
                          het.multiplier = c(1, 3, 5, 7, 10) / 1000)
  specs_df$ordering <- 1:25
  specs_df <- specs_df %>%
    mutate(out.name = paste0("pman", manip.per, "_slope", het.multiplier, "_10000_obs"),
           batch = 3 + (1:25) %% 3)

  specs_df$batch[c(1, 3, 5, 11, 13, 15, 21, 23, 25)] <- 2
  specs_df$batch[c(1, 5, 21, 25)] <- 1
  print(specs_df)

  for(idx in (1:25)[specs_df$batch == batch_number]){
    mc_loop(num.simulations = 10000, num.obs = 10000,
            latent.corr = 0, manip.per = specs_df$manip.per[idx],
            het.slope = specs_df$het.multiplier[idx],
            end.setup = FALSE, end.rho = 0.00, end.coeff = 0.00,
            function.form = "base",
            out.directory = out.directory, out.name = specs_df$out.name[idx])
  }
} else if(batch_number == 6) {

  for(hetmul in c(0.001, 0.003, 0.005, 0.007, 0.01)){
    mc_loop(num.simulations = 10000, num.obs = 10000,
            latent.corr = 0, manip.per = 0,
            het.slope = hetmul,
            end.setup = FALSE, end.rho = 0.00, end.coeff = 0.00,
            function.form = "base",
            out.directory = out.directory, out.name = paste0("pman0_slope", (hetmul*1000), "_10000_obs_ps13"))
  }

} else if(batch_number == 7) {
  mc_loop(num.simulations = 10000, num.obs = 10000,
          latent.corr = 0, manip.per = 0.40, het.slope = 0,
          end.setup = FALSE, end.rho = 0.00, end.coeff = 0.00,
          function.form = "base",
          out.directory = out.directory, out.name = "manipulation_40_10000_obs")
  mc_loop(num.simulations = 10000, num.obs = 10000,
          latent.corr = 0, manip.per = 0.55, het.slope = 0,
          end.setup = FALSE, end.rho = 0.00, end.coeff = 0.00,
          function.form = "base",
          out.directory = out.directory, out.name = "manipulation_55_10000_obs")
  mc_loop(num.simulations = 10000, num.obs = 10000,
          latent.corr = 0, manip.per = 0.70, het.slope = 0,
          end.setup = FALSE, end.rho = 0.00, end.coeff = 0.00,
          function.form = "base",
          out.directory = out.directory, out.name = "manipulation_70_10000_obs")
  mc_loop(num.simulations = 10000, num.obs = 10000,
          latent.corr = 0, manip.per = 0.85, het.slope = 0,
          end.setup = FALSE, end.rho = 0.00, end.coeff = 0.00,
          function.form = "base",
          out.directory = out.directory, out.name = "manipulation_85_10000_obs")

} else if(batch_number == 8) {

  mc_loop(num.simulations = 10000, num.obs = 10000,
          latent.corr = 0, manip.per = 1.00, het.slope = 0,
          end.setup = FALSE, end.rho = 0.00, end.coeff = 0.00,
          function.form = "base",
          out.directory = out.directory, out.name = "manipulation_100_10000_obs")

  ### Alternative functional form (not quadratic, but instead step function in x1 and x2 then linear in x3 and x4
  mc_loop(num.simulations = 10000, num.obs = 10000,
          latent.corr = 0, manip.per = 0.00, het.slope = 0,
          end.setup = FALSE, end.rho = 0.00, end.coeff = 0.00,
          function.form = "alternative",
          out.directory = out.directory, out.name = "alt_form_10000_obs")

  ### Bad controls
  mc_loop(num.simulations = 10000, num.obs = 10000,
          latent.corr = 0, manip.per = 0.00, het.slope = 0,
          end.setup = FALSE, end.rho = 0.00, end.coeff = 0.00,
          function.form = "bad",
          out.directory = out.directory, out.name = "bad_controls_form_10000_obs")

} else if(batch_number == 9) {

  for(pman in c(0.05, 0.1, 0.15, 0.2, 0.25)){
    mc_loop(num.simulations = 10000, num.obs = 10000,
            latent.corr = 0, manip.per = pman,
            het.slope = 0,
            end.setup = FALSE, end.rho = 0.00, end.coeff = 0.00,
            function.form = "base",
            out.directory = out.directory, out.name = paste0("pman", pman, "_slope_10000_obs"))
  }

} else if(batch_number == 10 || batch_number == 11 || batch_number == 12) {

  num_obs_vec <- if(batch_number == 10){
    c(250, 500, 1000, 1500, 2000)
  } else if(batch_number == 11){
    c(2500, 3000, 4000, 5000)
  } else {
    c(7500, 10000, 12500)
  }

  for(num_obs in num_obs_vec){
    mc_loop(num.simulations = 10000, num.obs = num_obs,
            latent.corr = 0, manip.per = 0,
            het.slope = 0,
            end.setup = FALSE, end.rho = 0.00, end.coeff = 0.00,
            function.form = "base",
            out.directory = out.directory,
            out.name = paste0("pman0_slope0_", num_obs, "_obs"))
  }

} else if(batch_number >= 13 && batch_number <= 17) { # endogeneity (latent variable)

  specs_df <- expand.grid(end.rho = c(0.05, 0.1, 0.2, 0.4, 0.55),
                          end.coeff = c(0.01, 0.03, 0.05, 0.07, 0.08))

  specs_df$ordering <- 1:25
  specs_df <- specs_df %>%
    mutate(out.name = paste0("heterogeneity_rho_", (end.rho * 100),
                             "_slope", (end.coeff * 100), "_10000_obs"),
           batch = 15 + (1:25) %% 3)

  specs_df$batch[c(1, 3, 5, 11, 13, 15, 21, 23, 25)] <- 14
  specs_df$batch[c(1, 5, 21, 25)] <- 13
  print(specs_df)

  for(idx in (1:25)[specs_df$batch == batch_number]){
    mc_loop(num.simulations = 10000, num.obs = 10000,
            latent.corr = 0, manip.per = 0,
            het.slope = 0,
            end.setup = TRUE, end.rho = specs_df$end.rho[idx], end.coeff = specs_df$end.coeff[idx],
            function.form = "base",
            out.directory = out.directory, out.name = specs_df$out.name[idx])
  }

} else if(batch_number == 18 || batch_number == 19){
  offset <- if(batch_number == 18) 0.0 else 0.5

  for(i_strength in 1:5){
    total_interaction_strength <- offset + (i_strength/10.0)
    mc_loop(num.simulations = 10000, num.obs = 10000,
            latent.corr = 0, manip.per = 0,
            het.slope = 0,
            end.setup = FALSE, end.rho = 0, end.coeff = 0,
            interaction_strength = total_interaction_strength,
            function.form = "base",
            out.directory = out.directory, out.name = paste0("interaction_", (total_interaction_strength * 10), "_10k_obs"))
  }

}

