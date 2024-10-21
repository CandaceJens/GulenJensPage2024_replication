###Figure 1 - bias and precision tradeoff 
require(ggplot2)
require(ggpubr)
require(ggforce)

library(stringr)

##Setting directory
project.dir <- if(str_ends(getwd(), "Part_1.output")){
  str_sub(getwd(), 1, -8)
} else if(str_ends(getwd(), "Part_1")){
  str_sub(getwd(), 1, -8)
} else {
  "."
}

print(Sys.getenv("USERDOMAIN"))

setwd(project.dir)


out.folder <- paste(project.dir, "Part_1", "output", sep="/")



set.seed(13215)

### set the 4 bias and precision values along with the number of draws
bias_values <- c(0, 0.20, 0.40, 0.60)
precision_values <- c(0.0625, 0.125, 0.25, 0.5) #precision here measured as standard deviation (0.05, 0.10, 0.175, 0.275 old)
number_draws <- 10

radii <- c(1,2/3,1/3) # radii for the circles on the plot

draw_sim <- function(bias, precision, number_draws = 5, use_same_draws = TRUE) {
  n_bias <- length(bias)
  n_precision <- length(precision)
  out_draws <- matrix(0, n_bias * n_precision * number_draws, 2)
  if (use_same_draws) {
    random_draws = runif(number_draws*2)
    for (b in 1:n_bias) for (p in 1:n_precision)
      out_draws[((b-1)*n_precision*number_draws + (p-1)*number_draws + 1):
                  ((b-1)*n_precision*number_draws + p*number_draws),] <- qnorm(random_draws, 
                                                                               mean = bias[b],
                                                                               sd = precision[p])
  } else {
    for (b in 1:n_bias) for (p in 1:n_precision) {
      random_draws = runif(number_draws*2)
      out_draws[((b-1)*n_precision*number_draws + (p-1)*number_draws + 1):
                  ((b-1)*n_precision*number_draws + p*number_draws),] <- qnorm(random_draws, 
                                                                               mean = bias[b],
                                                                               sd = precision[p])
    }
  }
  out_draws
}

new_dist <- function(x,y) {
  X <- mean(x)
  Y <- mean(y)
  sqrt(X^2+Y^2)
}
new_var <- function(x,y) {
  X <- var(x)
  Y <- var(y)
  (X+Y)
}

plot_df <- data.frame(Bias = rep(bias_values, each = 4 * number_draws),
                      Precision = rep(rep(precision_values, each = number_draws), 4),
                      x = 0, y = 0)
plot_df[,c("x","y")] <- draw_sim(bias_values, precision_values, number_draws, use_same_draws = TRUE)
agg_df <- data.frame(Bias = rep(c("Low","Med-Low","Med-High","High"),each=4),
                     Variance = rep(c("Low","Med-Low","Med-High","High"),4),
                     Theoretical_Bias = rep(sqrt(2*bias_values^2), each = 4), 
                     Theoretical_RMSE = rep(2*precision_values^2,4),
                     Estimate_Bias = 0, Estimate_RMSE = 0)
agg_df$Theoretical_RMSE <- sqrt(agg_df$Theoretical_Bias^2 + agg_df$Theoretical_RMSE)

agg_df$Theoretical_sd <- round(sqrt(agg_df$Theoretical_RMSE^2 - agg_df$Theoretical_Bias^2), digits = 4)
agg_df$Theoretical_var <- round(agg_df$Theoretical_RMSE^2 - agg_df$Theoretical_Bias^2, digits = 4)

circles_df <- data.frame(x0 = rep(0,3),
                         y0 = rep(0,3),
                         r = radii,
                         r_fill = factor(radii,
                                         levels = radii))

plot_list <- vector(mode = "list", length = length(bias_values)*length(precision_values))
plot_counter <- 1
for (b in 1:length(bias_values)) for (p in 1:length(precision_values)) {
  plot_list[[plot_counter]] <- ggplot() +
    geom_circle(mapping = aes(x0 = x0, y0 = y0, r = r, fill = r_fill), data = circles_df) +
    geom_point(mapping = aes(x = x, y = y),
               data = plot_df[plot_df$Bias==bias_values[b] & 
                                plot_df$Precision==precision_values[p],],
               size = 1, shape = 19) +
    coord_fixed() +
    scale_fill_manual(values = c("grey90","white","grey90")) +
    scale_x_continuous(limits = c(-1.5, 1.5), breaks = c(-1, -0.5, 0, 0.5, 1)) +
    scale_y_continuous(limits = c(-1.5, 1.5), breaks = c(-1, -0.5, 0, 0.5, 1)) +
    theme_bw() +
    theme(legend.position = "none",
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          #panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  agg_df$Estimate_Bias[(b-1)*4+p] <- new_dist(plot_df$x[plot_df$Bias==bias_values[b] & 
                                                            plot_df$Precision==precision_values[p]],
                                              plot_df$y[plot_df$Bias==bias_values[b] & 
                                                          plot_df$Precision==precision_values[p]])
  agg_df$Estimate_RMSE[(b-1)*4+p] <- sqrt(agg_df$Estimate_Bias[(b-1)*4+p]^2 +
                                            new_var(plot_df$x[plot_df$Bias==bias_values[b] & 
                                                                 plot_df$Precision==precision_values[p]],
                                                     plot_df$y[plot_df$Bias==bias_values[b] & 
                                                                 plot_df$Precision==precision_values[p]]))
  plot_counter <- plot_counter + 1
}


agg_df$Estimate_sd <- sqrt(agg_df$Estimate_RMSE^2 - agg_df$Estimate_Bias^2)
agg_df$Estimate_var <- agg_df$Estimate_RMSE^2 - agg_df$Estimate_Bias^2


ggarrange(plot_list[[13]], plot_list[[16]],
          plot_list[[1]], plot_list[[4]],
          nrow = 2, ncol = 2)


ggsave(filename = paste(out.folder, "Figure_1_Panel_A.png", sep = "/"),
       height = 4, width = 4, units = "in",
       ggarrange(plot_list[[13]], plot_list[[16]],
                 plot_list[[1]], plot_list[[4]],
                 nrow = 2, ncol = 2))

ggarrange(plot_list[[13]], plot_list[[14]], plot_list[[15]], plot_list[[16]],
          plot_list[[9]], plot_list[[10]], plot_list[[11]], plot_list[[12]],
          plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]],
          plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]],
          nrow = 4, ncol = 4)


ggsave(filename = paste(out.folder, "Figure_1_Panel_B.png", sep = "/"),
       height = 4, width = 4, units = "in",
       ggarrange(plot_list[[13]], plot_list[[14]], plot_list[[15]], plot_list[[16]],
                 plot_list[[9]], plot_list[[10]], plot_list[[11]], plot_list[[12]],
                 plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]],
                 plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]],
                 nrow = 4, ncol = 4))

### Bias and RMSE for each plot
agg_df
write.csv(agg_df, file = paste(out.folder, "targets_table.csv", sep = "/"))





bpfig <- ggplot(data = agg_df, mapping = aes(x = Theoretical_Bias, y = Theoretical_RMSE, 
                                      shape = factor(Theoretical_sd))) +
      geom_line() +
      geom_point(size = 1.5) +
      scale_shape_manual(name = "standard deviation", values = c(16, 0, 17, 6)) + 
      theme_bw() +
      labs(x="bias",y="RMSE",
        title="RMSE as a function of bias and standard deviation") +
      theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))


ggsave(filename = paste(out.folder, "bpfig.png", sep = "/"),
       height = 4, width = 6, units = "in",
       bpfig)





