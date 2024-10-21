### clear memory
rm(list=ls())

# libraries
require(tidyverse)
require(ggplot2)
require(gridExtra)
require(ggpubr)

# which dataset?
data.set = "NWallnoloan"

### directories
#setwd(..)
project.dir = getwd()
data.dir = paste(project.dir, "Causal Forest Output/Forest Data",sep="/")
output.dir = paste(project.dir,"output",sep="/")

# load in data
load(file = paste(data.dir, paste(paste(data.set, "forest_data", sep="_"), "Rdata", sep="."), sep="/"))

# additional functions
source(paste(project.dir, "functions_additional.R", sep="/"))

# Figure 7: Differences in investment around threshold for different propensities to be in default
max.p <- max(data$pred.bind[data$bind==0])
mean.func <- function(p, data = data, max.p = max.p) {(mean(data$bind[between(data$pred.bind, p, max.p)]) - 0.5)^2}
even.p <- optimise(f = mean.func, interval = c(0,max.p), data = data, max.p = max.p)$minimum
diff.invest = diff.by(data$slack.ratio[between(data$pred.bind, even.p, max.p)],
                      data$investment[between(data$pred.bind, even.p, max.p)],
                      200, 1, cum = TRUE)
diff.invest$ci.mult = qt(0.975, diff.invest$count-1)
top_range_y = 0.05
sample_mult = 0.5 / top_range_y
diff.invest$percent.of.sample = (diff.invest$count / dim(data)[1] - 0.5) / sample_mult
diff.invest$pos.percent.of.sample = (diff.invest$count.pos / dim(data)[1] - 0.5) / sample_mult
diff.invest$neg.percent.of.sample = (diff.invest$count.neg / dim(data)[1] - 0.5) / sample_mult
plot_A <- ggplot() +
  geom_line(data = diff.invest, aes(x = x, y = mean), color = "darkblue", size = 1, linetype = "solid") +
  geom_ribbon(data = diff.invest, aes(x = x, ymin = mean - ci.mult * se, ymax = mean + ci.mult * se), color = "lightblue", fill = "lightblue", alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_line(data = diff.invest, aes(x = x, y = percent.of.sample), color = "sienna2", size = 1, linetype = "dashed") +
  coord_cartesian(ylim = c(-top_range_y, top_range_y)) +
  scale_y_continuous(breaks = c(-top_range_y, 0, top_range_y), 
                     sec.axis = sec_axis(~.*sample_mult + 0.5, breaks = c(0,0.5,1), name = "Percent of Sample")) +
  scale_x_continuous(labels = scales::percent) +
  ggtitle(label = "Panel A. Firms likely to be in default (propensity between 22.99% and 75.48%)") +
  xlab(label = "Percent Distance from Threshold") +
  ylab(label = "Difference in Mean Investment") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size=16),
        axis.title = element_text(size=16),
        axis.text = element_text(size=14))

diff.invest = diff.by(data$slack.ratio[data$pred.bind < even.p], 
                      data$investment[data$pred.bind < even.p],
                      200, 1, cum = TRUE)
diff.invest$ci.mult = qt(0.975, diff.invest$count-1)
top_range_y = 0.05
sample_mult = 0.5 / top_range_y
diff.invest$percent.of.sample = (diff.invest$count / dim(data)[1] - 0.5) / sample_mult
diff.invest$pos.percent.of.sample = (diff.invest$count.pos / dim(data)[1] - 0.5) / sample_mult
diff.invest$neg.percent.of.sample = (diff.invest$count.neg / dim(data)[1] - 0.5) / sample_mult
plot_B <- ggplot() +
  geom_line(data = diff.invest, aes(x = x, y = mean), color = "darkblue", size = 1, linetype = "solid") +
  geom_ribbon(data = diff.invest, aes(x = x, ymin = mean - ci.mult * se, ymax = mean + ci.mult * se), color = "lightblue", fill = "lightblue", alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_line(data = diff.invest, aes(x = x, y = percent.of.sample), color = "sienna2", size = 1, linetype = "dashed") +
  coord_cartesian(ylim = c(-top_range_y, top_range_y)) +
  scale_y_continuous(breaks = c(-top_range_y, 0, top_range_y), 
                     sec.axis = sec_axis(~.*sample_mult + 0.5, breaks = c(0,0.5,1), name = "Percent of Sample")) +
  scale_x_continuous(labels = scales::percent) +
  ggtitle(label = "Panel B. Firms less likely to be in default (propensity < 22.99%)") +
  xlab(label = "Percent Distance from Threshold") +
  ylab(label = "Difference in Mean Investment") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size=16),
        axis.title = element_text(size=16),
        axis.text = element_text(size=14))

ggsave(paste(output.dir,"Figure_7.pdf",sep="/"),width=9,height=9,
       arrangeGrob(plot_A,plot_B,
                   nrow = 2, ncol = 1))
