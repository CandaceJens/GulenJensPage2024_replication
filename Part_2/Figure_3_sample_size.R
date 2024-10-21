rm(list=ls())


##Loads required packages 
require(ggplot2)
require(tidyverse)

##
project.dir <- "."

FIGURES_DIR <- "output"


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



###### Load Data ######
data.250 <- read.csv(file = paste(project.dir, "sample_sizes/pman0_slope0_250_obs_sims_alt.csv", sep="/"))


data.250 <- data.250 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.250.melted <- data.250 %>% 
				gather(data.250)

#data.250.melted$specification <- "250"

data.250.melted <- rename(data.250.melted , 
			model.spec = data.250,
			mc.sims = value)

data.250.melted$model.spec <- factor(data.250.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######250 plot:
plot.250 <- ggplot(data = data.250.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-500, 500) + 
	ylim(0, 0.015) + 
	xlab("bias (%)") + 
	ylab("Density") +
	ggtitle("Sample size = 250") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.250


###### Load Data for 500######

data.500 <- read.csv(file = paste(project.dir, "sample_sizes/pman0_slope0_500_obs_sims_alt.csv", sep="/"))


data.500 <- data.500 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.500.melted <- data.500 %>% 
				gather(data.500)

#data.500.melted$specification <- "500"

data.500.melted <- rename(data.500.melted , 
			model.spec = data.500,
			mc.sims = value)

data.500.melted$model.spec <- factor(data.500.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######500 plot:
plot.500 <- ggplot(data = data.500.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth =0.3) + 
	geom_vline(xintercept = 0, linewidth =0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-500, 500) + 
	ylim(0, 0.015) + 
	xlab("bias (%)") + 
	ylab("Density") +
	ggtitle("Sample size = 500") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.500


###### Load Data for 1000######

data.1000 <- read.csv(file = paste(project.dir, "sample_sizes/pman0_slope0_1000_obs_sims_alt.csv", sep="/"))


data.1000 <- data.1000 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.1000.melted <- data.1000 %>% 
				gather(data.1000)

#data.1000.melted$specification <- "1000"

data.1000.melted <- rename(data.1000.melted , 
			model.spec = data.1000,
			mc.sims = value)

data.1000.melted$model.spec <- factor(data.1000.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######1000 plot:
plot.1000 <- ggplot(data = data.1000.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-500, 500) + 
	ylim(0, 0.015) + 
	xlab("bias (%)") + 
	ylab("Density") +
	ggtitle("Sample size = 1,000") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.1000

###### Load Data for 1500######

data.1500 <- read.csv(file = paste(project.dir, "sample_sizes/pman0_slope0_1500_obs_sims_alt.csv", sep="/"))


data.1500 <- data.1500 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.1500.melted <- data.1500 %>% 
				gather(data.1500)

#data.1500.melted$specification <- "1500"

data.1500.melted <- rename(data.1500.melted , 
			model.spec = data.1500,
			mc.sims = value)

data.1500.melted$model.spec <- factor(data.1500.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######1500 plot:
plot.1500 <- ggplot(data = data.1500.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-500, 500) + 
	ylim(0, 0.015) + 
	xlab("bias (%)") + 
	ylab("Density") +
	ggtitle("Sample size = 1,500") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.1500


###### Load Data for 2000######

data.2000 <- read.csv(file = paste(project.dir, "sample_sizes/pman0_slope0_2000_obs_sims_alt.csv", sep="/"))


data.2000 <- data.2000 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.2000.melted <- data.2000 %>% 
				gather(data.2000)

#data.2000.melted$specification <- "2000"

data.2000.melted <- rename(data.2000.melted , 
			model.spec = data.2000,
			mc.sims = value)

data.2000.melted$model.spec <- factor(data.2000.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######2000 plot:
plot.2000 <- ggplot(data = data.2000.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-250, 250) + 
	ylim(0, 0.025) + 
	xlab("bias (%)") + 
	ylab("Density") +
	ggtitle("Sample size = 2,000") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.2000

###### Load Data for 2500######

data.2500 <- read.csv(file = paste(project.dir, "sample_sizes/pman0_slope0_2500_obs_sims_alt.csv", sep="/"))


data.2500 <- data.2500 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.2500.melted <- data.2500 %>% 
				gather(data.2500)

#data.2500.melted$specification <- "2500"

data.2500.melted <- rename(data.2500.melted , 
			model.spec = data.2500,
			mc.sims = value)

data.2500.melted$model.spec <- factor(data.2500.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######2500 plot:
plot.2500 <- ggplot(data = data.2500.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-250, 250) + 
	ylim(0, 0.025) + 
	xlab("bias (%)") + 
	ylab("Density") +
	ggtitle("Sample size = 2,500") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") +
	theme(plot.title = element_text(hjust = 0.5))

plot.2500




###### Load Data for 3000######

data.3000 <- read.csv(file = paste(project.dir, "sample_sizes/pman0_slope0_3000_obs_sims_alt.csv", sep="/"))


data.3000 <- data.3000 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.3000.melted <- data.3000 %>% 
				gather(data.3000)

#data.3000.melted$specification <- "3000"

data.3000.melted <- rename(data.3000.melted , 
			model.spec = data.3000,
			mc.sims = value)

data.3000.melted$model.spec <- factor(data.3000.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######3000 plot:
plot.3000 <- ggplot(data = data.3000.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-250, 250) + 
	ylim(0, 0.025) + 
	xlab("bias (%)") + 
	ylab("Density") +
	ggtitle("Sample size = 3,000") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") +
	theme(plot.title = element_text(hjust = 0.5))

plot.3000


###### Load Data for 4000######

data.4000 <- read.csv(file = paste(project.dir, "sample_sizes/pman0_slope0_4000_obs_sims_alt.csv", sep="/"))


data.4000 <- data.4000 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.4000.melted <- data.4000 %>% 
				gather(data.4000)

#data.4000.melted$specification <- "4000"

data.4000.melted <- rename(data.4000.melted , 
			model.spec = data.4000,
			mc.sims = value)

data.4000.melted$model.spec <- factor(data.4000.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######4000 plot:
plot.4000 <- ggplot(data = data.4000.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-250, 250) + 
	ylim(0, 0.025) + 
	xlab("bias (%)") + 
	ylab("Density") +
	ggtitle("Sample size = 4,000") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") +
	theme(plot.title = element_text(hjust = 0.5))

plot.4000


###### Load Data for 5000######

data.5000 <- read.csv(file = paste(project.dir, "sample_sizes/pman0_slope0_5000_obs_sims_alt.csv", sep="/"))


data.5000 <- data.5000 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.5000.melted <- data.5000 %>% 
				gather(data.5000)

#data.5000.melted$specification <- "5000"

data.5000.melted <- rename(data.5000.melted , 
			model.spec = data.5000,
			mc.sims = value)

data.5000.melted$model.spec <- factor(data.5000.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######5000 plot:
plot.5000 <- ggplot(data = data.5000.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 100) + 
	ylim(0, 0.045) + 
	xlab("bias (%)") + 
	ylab("Density") +
	ggtitle("Sample size = 5,000") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") +
	theme(plot.title = element_text(hjust = 0.5))

plot.5000

###### Load Data for 7500######

data.7500 <- read.csv(file = paste(project.dir, "sample_sizes/pman0_slope0_7500_obs_sims_alt.csv", sep="/"))


data.7500 <- data.7500 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.7500.melted <- data.7500 %>% 
				gather(data.7500)

#data.7500.melted$specification <- "7500"

data.7500.melted <- rename(data.7500.melted , 
			model.spec = data.7500,
			mc.sims = value)

data.7500.melted$model.spec <- factor(data.7500.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######7500 plot:
plot.7500 <- ggplot(data = data.7500.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 100) + 
	ylim(0, 0.045) + 
	xlab("bias (%)") + 
	ylab("Density") +
	ggtitle("Sample size = 7,500") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") +
	theme(plot.title = element_text(hjust = 0.5))

plot.7500


###### Load Data for 10000######

data.10000 <- read.csv(file = paste(project.dir, "sample_sizes/pman0_slope0_10000_obs_sims_alt.csv", sep="/"))


data.10000 <- data.10000 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.10000.melted <- data.10000 %>% 
				gather(data.10000)

#data.10000.melted$specification <- "10000"

data.10000.melted <- rename(data.10000.melted , 
			model.spec = data.10000,
			mc.sims = value)

data.10000.melted$model.spec <- factor(data.10000.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######10000 plot:
plot.10000 <- ggplot(data = data.10000.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 100) + 
	ylim(0, 0.045) + 
	xlab("bias (%)") + 
	ylab("Density") +
	ggtitle("Sample size = 10,000") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") +
	theme(plot.title = element_text(hjust = 0.5))

plot.10000


###### Load Data for 12500######

data.12500 <- read.csv(file = paste(project.dir, "sample_sizes/pman0_slope0_12500_obs_sims_alt.csv", sep="/"))


data.12500 <- data.12500 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.12500.melted <- data.12500 %>% 
				gather(data.12500)

#data.12500.melted$specification <- "12500"

data.12500.melted <- rename(data.12500.melted , 
			model.spec = data.12500,
			mc.sims = value)

data.12500.melted$model.spec <- factor(data.12500.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######12500 plot:
plot.12500 <- ggplot(data = data.12500.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 100) + 
	ylim(0, 0.045) + 
	xlab("bias (%)") + 
	ylab("Density") +
	ggtitle("Sample size = 12,500") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") +
	theme(plot.title = element_text(hjust = 0.5))

plot.12500


#########
##250, 500, 1000, 1500, 
##2000, 2500, 3000, 4000, 
##5000, 7500, 10000, 1250 


multiplot(plot.250, plot.2000, plot.5000, plot.500, plot.2500, plot.7500, 
		plot.1000, plot.3000, plot.10000, plot.1500, plot.4000, plot.12500, cols = 4)




###save this:
#ggsave(multiplot(plot.250, plot.2000, plot.5000, plot.500, plot.2500, plot.7500, 
#		plot.1000, plot.3000, plot.10000, plot.1500, plot.4000, plot.12500, cols = 4), 	
#		filename = paste(project.dir, "figures/samplesize.jpg", sep="/"),
 #      	width = 9, height = 6, units = "in")


library(ggpubr)

all.plots <- ggarrange(plot.250, plot.500, plot.1000, plot.1500, 
		plot.2000, plot.2500, plot.3000, plot.4000,
		plot.5000,   plot.7500, plot.10000,   plot.12500, 
		 ncol = 4, nrow = 3)

ggsave(all.plots, 	
		filename = paste(project.dir, FIGURES_DIR, "Figure_IA_1.pdf", sep="/"),
       	width = 9, height = 6, units = "in")



##################
####Smaller figure for paper


smaller.plots <- ggarrange( plot.500, plot.1000, 
		plot.2000,   plot.3000,
		plot.5000,    plot.10000,  
		 ncol = 2, nrow = 3)
smaller.plots

ggsave(smaller.plots, 	
		filename = paste(project.dir, FIGURES_DIR, "Figure_3.pdf", sep="/"),
       	width = 6, height = 8, units = "in")




