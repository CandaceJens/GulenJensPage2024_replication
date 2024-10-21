### clear memory
rm(list=ls())

### Required libraries
require(tidyverse)
require(ggplot2)
require(scales)
require(gridExtra)

# which dataset?
data.set = "NWallnoloan"

### directories
#setwd(..)
project.dir = getwd()
data.dir = paste(project.dir, "Causal Forest Output/Forest Data",sep="/")
output.dir = paste(project.dir,"output",sep="/")

# load data
load(file = paste(data.dir, paste(paste(data.set, "forest_data", sep="_"), "Rdata", sep="."), sep="/"))

# Figure IA.4: Scatter plots for the below variables
plot.list = vector('list', 9)
plot.names = c("lag.macro.q","cashflow","lag.cashflow","lag.log.assets", "lag.altman_alt", "initial_slack_ratio", 
               "syndicate_size", "cash_assets", "slack.ratio")
plot.labels = c("Lag Macro Q", "Cash Flow", "Lag Cash Flow", "Lag Log Assets", "Lag Altman Z",
                "Initial Slack as Ratio", "Syndicate Size", "Cash / Assets", "Distance to Default")
for (i in 1:9) {
  plot.list[[i]] <- ggplot(data, aes(x = .data[[plot.names[i]]], y = hte)) +
    geom_point(size = 0.5) +
    labs(x = plot.labels[i],
         y = "Treatment Effect") +
    theme_bw()
}

# display each plot individually
plot.list[[1]]
plot.list[[2]]
plot.list[[3]]
plot.list[[4]]
plot.list[[5]]
plot.list[[6]]
plot.list[[7]]
plot.list[[8]]
plot.list[[9]]

# save to a pdf
ggsave(paste(output.dir,"Figure_IA_4.pdf",sep="/"),width=9,height=6,
       arrangeGrob(plot.list[[1]],plot.list[[2]],plot.list[[3]],
                   plot.list[[4]],plot.list[[5]],plot.list[[6]],
                   plot.list[[7]],plot.list[[8]],plot.list[[9]],
                   layout_matrix = rbind(c(1,2,3),c(4,5,6),c(7,8,9))))
