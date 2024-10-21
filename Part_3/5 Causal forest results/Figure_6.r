### clear memory
rm(list=ls())


### Required libraries
require(tidyverse)
require(ggplot2)
require(scales)
require(gridExtra)
require(ggpubr)
require(grf)
require(pracma)

# which dataset?
data.set = "NWallnoloan"

### directories
#setwd(..)
project.dir = getwd()
forest.dir = paste(project.dir,"Causal Forest Output/Forests",sep="/")
data.dir = paste(project.dir, "Causal Forest Output/Forest Data",sep="/")
output.dir = paste(project.dir,"output",sep="/")

# load data, forest, and some additional functions
load(file = paste(data.dir, paste(paste(data.set, "forest_data", sep="_"), "Rdata", sep="."), sep="/"))
load(file=paste(forest.dir, paste(data.set, "forest.RData", sep="_"),sep="/"))

# function to produce data on "typical" firm (using median, mean, or geomedian)
forest.comparative.static.data <- function(X, constant_point = "median") {
  if (is.null(X))
    return(print("Data and comparative static variable must be provided"))
  
  X.names = colnames(X)
  if (constant_point == "median") {
    X.cs = data.frame(t(apply(X, 2, median)))
  } else if (constant_point == "mean") {
    X.cs = data.frame(t(apply(X, 2, mean)))
  } else if (constant_point == "geomedian") {
    X.cs = data.frame(t(as.matrix(geo_median(as.matrix(X))$p)))
  }
  
  return(X.cs)
}

# function to create a matrix of data for typical firm with only one variable allowed to vary
cs.column <- function(X, variable, variable_range, num_points = 101) {
  if (is.null(X) | is.null(variable) | is.null(variable_range))
    return(print("Data and comparative static variable must be provided"))

  X.names = colnames(X)
  X = X[rep(1,times=num_points),]
  X[[variable]] = seq(variable_range[1], variable_range[2], length = num_points)
  
  return(X)
}

# variable names
X.names.vars = c("lag.macro.q","cashflow","lag.cashflow","lag.log.assets", "lag.altman_alt", "initial_slack_ratio", 
                 "syndicate_size", "has.rating.all", "cash_assets")
X.names.time = c("year","quarter")
X.names = c(X.names.vars,X.names.time, "firm_fe", "slack.ratio")


# Figure 6: Comparative statics plots for variables of interest
plot.list = vector('list', 12)
plot.names = c("lag.macro.q","cashflow","lag.cashflow","lag.log.assets", "lag.altman_alt", "initial_slack_ratio", 
               "syndicate_size", "cash_assets", "year", "firm_fe", "slack.ratio")
plot.labels = c("Lag Macro Q", "Cash Flow", "Lag Cash Flow", "Lag Log Assets", "Lag Altman Z",
                "Initial Slack as Ratio", "Syndicate Size", "Cash / Assets", "Year", "Firm Fixed Effect", "Distance to Default")

X.base = forest.comparative.static.data(X = data[X.names], constant_point = "geomedian")
X.base$syndicate_size <- ifelse(X.base$syndicate_size - floor(X.base$syndicate_size) >= 0.5, 1, 0) + floor(X.base$syndicate_size)
X.base$has.rating.all <- ifelse(X.base$has.rating.all >= 0.5, 1, 0)
X.base$year <- ifelse(X.base$year - floor(X.base$year) >= 0.5, 1, 0) + floor(X.base$year)
X.base$quarter = 4
num_points = 251

for (i in 1:11) {
  if (plot.names[i] == "year") {
    variable_range = quantile(data[[plot.names[i]]], c(0,1))
  } else {
    variable_range = quantile(data[[plot.names[i]]], c(0.01,0.99))
  }
  X = cs.column(X = X.base, variable = plot.names[i], variable_range = variable_range, num_points = num_points)
  X$predictions = predict(forest.main,X)$predictions
  if (plot.names[i] %in% c("initial_slack_ratio", "syndicate_size")) {
    plot.list[[i]] <- ggplot(X, aes(x = .data[[plot.names[i]]], y = predictions)) + 
      geom_line(size = 0.8) +
      labs(x = plot.labels[i],
           y = "Treatment Effect") +
      scale_y_continuous(limits = c(-0.03, 0.02), breaks = c(-0.02,0.00,0.02)) +
      scale_x_continuous(trans = "log10") +
      theme_bw() +
      theme(legend.position = "none")
  } else if (plot.names[i] == "lag.log.assets") {
    plot.list[[i]] <- ggplot(X, aes(x = exp(.data[[plot.names[i]]]), y = predictions)) + 
      geom_line(size = 0.8) +
      labs(x = plot.labels[i],
           y = "Treatment Effect") +
      scale_y_continuous(limits = c(-0.03, 0.02), breaks = c(-0.02,0.00,0.02)) +
      scale_x_continuous(trans = "log10") +
      theme_bw() +
      theme(legend.position = "none")
  } else {
    plot.list[[i]] <- ggplot(X, aes(x = .data[[plot.names[i]]], y = predictions)) + 
      geom_line(size = 0.8) +
      labs(x = plot.labels[i],
           y = "Treatment Effect") +
      scale_y_continuous(limits = c(-0.03, 0.02), breaks = c(-0.02,0.00,0.02)) +
      theme_bw() +
      theme(legend.position = "none")
  }
}

# display each chart
plot.list[[1]]
plot.list[[2]]
plot.list[[3]]
plot.list[[4]]
plot.list[[5]]
plot.list[[6]]
plot.list[[7]]
plot.list[[8]]
plot.list[[9]]

# save the combined plots to a file
ggsave(paste(output.dir,"Figure_6.pdf",sep="/"),width=9,height=6,
       arrangeGrob(plot.list[[1]],plot.list[[2]],plot.list[[3]],
                   plot.list[[4]],plot.list[[5]],plot.list[[6]],
                   plot.list[[7]],plot.list[[8]],plot.list[[9]],
                   layout_matrix = rbind(c(1,2,3),c(4,5,6),c(7,8,9))))
