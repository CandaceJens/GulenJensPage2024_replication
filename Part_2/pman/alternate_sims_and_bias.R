library(tidyverse)
library(xtable)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

at_cols <- c("rd.late", "rd.half", "rd.double","ols.ate",
             "forest.ate","forest.att","forest.overlap",
             "forest.close.ate","forest.close.att","forest.close.overlap",
             "forest.very.close.ate","forest.very.close.att","forest.very.close.overlap")
se_cols <- paste0(at_cols, ".se")

create_alternate_sims <- function(input_filename, drop_top_bottom = 10, at_cols = NULL, se_cols = NULL, 
                      output_filename=NULL, WRITE_TO_FILE = TRUE) {
  if(is.null(output_filename) && WRITE_TO_FILE){
    output_filename <- str_replace(input_filename, "_sims", "_sims_alt")
    if(output_filename == input_filename){
      WRITE_TO_FILE <- FALSE
      warn(paste("output filename identical to input filename -- I will not write to ", input_filename))
    }
  }

  mydata <- read_csv(input_filename, col_names=TRUE)
  
  n_data <- nrow(mydata)
  n_cols <- length(at_cols)
  for (i in 1:n_cols) {
    at <- at_cols[i]
    atse <- se_cols[i]
    top_bottom <- quantile(mydata[[at]], c(drop_top_bottom, n_data - drop_top_bottom) / n_data)
    mydata[[at]][!between(mydata[[at]], top_bottom[1], top_bottom[2])] <- NA
    mydata[[atse]][is.na(mydata[[at]])] <- NA
  }
  
  # McCrary test deserves special treatment
  mydata$rd.mccrary[is.na(mydata$rd.late)] <- NA
  
  if(WRITE_TO_FILE){
    write.csv(mydata, row.names=FALSE, file=output_filename)
  }
  
}

create_alternate_bias_table <- function(input_filename,  at_cols = NULL, se_cols = NULL,
                                        output_filename = NULL, WRITE_TO_FILE = TRUE) {
  if(is.null(output_filename) && WRITE_TO_FILE){
    output_filename <- str_replace(input_filename, "_sims_alt", "_bias_alt")
    if(output_filename == input_filename){
      WRITE_TO_FILE <- FALSE
      warn(paste("output filename identical to input filename -- I will not write to ", input_filename))
    }
  }
  
  mydata <- read_csv(input_filename, col_names=TRUE)
  
  out_df <- data.frame(method = c("RDD", "RDD_half_bw", "RDD_double_bw", "OLS", 
                                  "Forest_ATE", "Forest_ATT", "Forest_overlap", 
                                  "Forest_close_ATE", "Forest_close_ATT", "Forest_close_overlap", 
                                  "Forest_very_close_ATE", "Forest_very_close_ATT", 
                                  "Forest_very_close_overlap", "McCrary"),
                       bias = NA,
                       rmse = NA,
                       coverage = NA)
  
  
  for (i in 1:length(at_cols)) {
    at <- at_cols[i]
    atse <- se_cols[i]
    out_df$bias[i] <- mean(mydata[[at]], na.rm = T) / 0.02 - 1
    out_df$rmse[i] <- sqrt(mean((mydata[[at]] - 0.02)^2, na.rm = T)) / 0.02
    out_df$coverage[i] <- 1 - mean(abs((mydata[[at]] - 0.02) / mydata[[atse]]) <= qnorm(0.975), na.rm = T)
  }
  out_df[14,2:3] <- NA
  out_df$coverage[14] <- mean(mydata$rd.mccrary <= 0.05, na.rm = T)
  out_df[,c("bias","rmse","coverage")] <- 100 * out_df[,c("bias","rmse","coverage")]
  
  if(WRITE_TO_FILE){
    write.csv(out_df, row.names=FALSE, file=output_filename)
  }
  
  out_mc <- data.frame(Estimator = c("RDD", "OLS", "Causal Forest"),
                       Bias = out_df$bias[c(1,4,5)],
                       RMSE = out_df$rmse[c(1,4,5)],
                       Coverage = out_df$coverage[c(1,4,5)])
  
  output_filename <- str_replace(input_filename, "_sims_alt.csv", "_bias_alt.tex")

  rv <- xtable(out_mc, digits=2, caption = caption)
  if(WRITE_TO_FILE){
    print(rv, include.rownames=FALSE, file=output_filename)
  }
  rv

}

for(csv_file in list.files(".", pattern="\\_sims.csv$")){
  create_alternate_sims(csv_file, drop_top_bottom = 10,
                        at_cols = at_cols, se_cols = se_cols)
}

for(csv_file in list.files(".", pattern="\\_sims_alt.csv$")){
  create_alternate_bias_table(csv_file,
                        at_cols = at_cols, se_cols = se_cols)
}


