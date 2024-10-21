rm(list=ls())


##Loads required packages 
#require(ggplot2)
require(tidyverse)
library(ggpubr)
require(ggplot2)

##
project.dir <- "."

FIGURES_DIR <- "output"


###### Load Data ######

data.0.1 <- read.csv(file = paste(project.dir, "interactions/interaction_110k_obs_sims_alt.csv", sep="/"))


data.0.1 <- data.0.1 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.0.1.melted <- data.0.1 %>% 
				gather(data.0.1)

data.0.1.melted <- rename(data.0.1.melted , 
			model.spec = data.0.1,
			mc.sims = value)

data.0.1.melted$model.spec <- factor(data.0.1.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.0.1 plot:
plot.0.1 <- ggplot(data = data.0.1.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 75) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Interaction strength = 0.1") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.0.1

################################
####0.2 data
data.0.2 <- read.csv(file = paste(project.dir, "interactions/interaction_210k_obs_sims_alt.csv", sep="/"))


data.0.2 <- data.0.2 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.0.2.melted <- data.0.2 %>% 
				gather(data.0.2)

data.0.2.melted <- rename(data.0.2.melted , 
			model.spec = data.0.2,
			mc.sims = value)

data.0.2.melted$model.spec <- factor(data.0.2.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.0.2 plot:
plot.0.2 <- ggplot(data = data.0.2.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 75) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Interaction strength = 0.2") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.0.2


################################
####0.3 data
data.0.3 <- read.csv(file = paste(project.dir, "interactions/interaction_310k_obs_sims_alt.csv", sep="/"))


data.0.3 <- data.0.3 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.0.3.melted <- data.0.3 %>% 
				gather(data.0.3)

data.0.3.melted <- rename(data.0.3.melted , 
			model.spec = data.0.3,
			mc.sims = value)

data.0.3.melted$model.spec <- factor(data.0.3.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.0.3 plot:
plot.0.3 <- ggplot(data = data.0.3.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 75) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Interaction strength = 0.3") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.0.3


################################
####0.4 data
data.0.4 <- read.csv(file = paste(project.dir, "interactions/interaction_410k_obs_sims_alt.csv", sep="/"))


data.0.4 <- data.0.4 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.0.4.melted <- data.0.4 %>% 
				gather(data.0.4)

data.0.4.melted <- rename(data.0.4.melted , 
			model.spec = data.0.4,
			mc.sims = value)

data.0.4.melted$model.spec <- factor(data.0.4.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.0.4 plot:
plot.0.4 <- ggplot(data = data.0.4.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 75) + 
  ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Interaction strength = 0.4") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.0.4



################################
####0.5 data
data.0.5 <- read.csv(file = paste(project.dir, "interactions/interaction_510k_obs_sims_alt.csv", sep="/"))


data.0.5 <- data.0.5 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.0.5.melted <- data.0.5 %>% 
				gather(data.0.5)

data.0.5.melted <- rename(data.0.5.melted , 
			model.spec = data.0.5,
			mc.sims = value)

data.0.5.melted$model.spec <- factor(data.0.5.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.0.5 plot:
plot.0.5 <- ggplot(data = data.0.5.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 75) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Interaction strength = 0.5") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.0.5

################################
####0.6 data
data.0.6 <- read.csv(file = paste(project.dir, "interactions/interaction_610k_obs_sims_alt.csv", sep="/"))


data.0.6 <- data.0.6  %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.0.6.melted <- data.0.6 %>% 
				gather(data.0.6)

data.0.6.melted <- rename(data.0.6.melted , 
			model.spec = data.0.6,
			mc.sims = value)

data.0.6.melted$model.spec <- factor(data.0.6.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.0.6 plot:
plot.0.6 <- ggplot(data = data.0.6.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 75) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Interaction strength = 0.6") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.0.6



################################
####0.7 data
data.0.7 <- read.csv(file = paste(project.dir, "interactions/interaction_710k_obs_sims_alt.csv", sep="/"))


data.0.7 <- data.0.7 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.0.7.melted <- data.0.7 %>% 
				gather(data.0.7)

data.0.7.melted <- rename(data.0.7.melted , 
			model.spec = data.0.7,
			mc.sims = value)

data.0.7.melted$model.spec <- factor(data.0.7.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.0.7 plot:
plot.0.7 <- ggplot(data = data.0.7.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 75) + 
	ylim(0, 0.075) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Interaction strength = 0.7") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.0.7

################################
####0.8 data
data.0.8 <- read.csv(file = paste(project.dir, "interactions/interaction_810k_obs_sims_alt.csv", sep="/"))


data.0.8 <- data.0.8 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.0.8.melted <- data.0.8 %>% 
				gather(data.0.8)

data.0.8.melted <- rename(data.0.8.melted , 
			model.spec = data.0.8,
			mc.sims = value)

data.0.8.melted$model.spec <- factor(data.0.8.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.0.8 plot:
plot.0.8 <- ggplot(data = data.0.8.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 75) + 
	ylim(0, 0.075) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Interaction strength = 0.8") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.0.8


################################
####1 data
data.1 <- read.csv(file = paste(project.dir, "interactions/interaction_810k_obs_sims_alt.csv", sep="/"))


data.1 <- data.1 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.1.melted <- data.1 %>% 
				gather(data.1)

data.1.melted <- rename(data.1.melted , 
			model.spec = data.1,
			mc.sims = value)

data.1.melted$model.spec <- factor(data.1.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.1 plot:
plot.1 <- ggplot(data = data.1.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 75) + 
	ylim(0, 0.075) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Interaction strength = 1") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.1




##############3
all.plots <- ggarrange(plot.0.1, plot.0.2, plot.0.3,
			plot.0.4, plot.0.5, plot.0.6,
			plot.0.7, plot.0.8, plot.1, 
		 ncol = 3, nrow = 3)
all.plots



ggsave(all.plots, 	
       filename = paste(project.dir, "output/Figure_IA_2.pdf", sep="/"),
       width = 9, height = 6, units = "in")







