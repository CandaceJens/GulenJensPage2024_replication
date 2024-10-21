rm(list=ls())


##Loads required packages 
require(ggplot2)
require(tidyverse)
library(ggpubr)

project.dir <- "."

OUTPUT_DIRECTORY <- "output"


###### Load Data ######

data.corr_5_coeff_1 <- read.csv(file = paste(project.dir, "latency/heterogeneity_rho_5_slope1_10000_obs_sims_alt.csv", sep="/"))


data.corr_5_coeff_1 <- data.corr_5_coeff_1 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.corr_5_coeff_1.melted <- data.corr_5_coeff_1 %>% 
				gather(data.corr_5_coeff_1)

data.corr_5_coeff_1.melted <- rename(data.corr_5_coeff_1.melted , 
			model.spec = data.corr_5_coeff_1,
			mc.sims = value)

data.corr_5_coeff_1.melted$model.spec <- factor(data.corr_5_coeff_1.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.corr_5_coeff_1 plot:
plot.corr_5_coeff_1 <- ggplot(data = data.corr_5_coeff_1.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 100) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Slope on latent = 0.01 \n Latent corr with w = 0.05") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.corr_5_coeff_1




###########
####Latent corr 5 is first row, 1, 3, 5, 7 across


data.corr_5_coeff_3 <- read.csv(file = paste(project.dir, "latency/heterogeneity_rho_5_slope3_10000_obs_sims_alt.csv", sep="/"))

data.corr_5_coeff_3 <- data.corr_5_coeff_3 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.corr_5_coeff_3.melted <- data.corr_5_coeff_3 %>% 
				gather(data.corr_5_coeff_3)

data.corr_5_coeff_3.melted <- rename(data.corr_5_coeff_3.melted , 
			model.spec = data.corr_5_coeff_3,
			mc.sims = value)

data.corr_5_coeff_3.melted$model.spec <- factor(data.corr_5_coeff_3.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.corr_5_coeff_3 plot:
plot.corr_5_coeff_3 <- ggplot(data = data.corr_5_coeff_3.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 100) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Slope on latent = 0.03 \n Latent corr with w = 0.05") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.corr_5_coeff_3


###########
####Latent corr 5 is first row, 1, 3, 5, 7 across
data.corr_5_coeff_5 <- read.csv(file = paste(project.dir, "latency/heterogeneity_rho_5_slope5_10000_obs_sims_alt.csv", sep="/"))


data.corr_5_coeff_5 <- data.corr_5_coeff_5  %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.corr_5_coeff_5.melted <- data.corr_5_coeff_5 %>% 
				gather(data.corr_5_coeff_5)

data.corr_5_coeff_5.melted <- rename(data.corr_5_coeff_5.melted , 
			model.spec = data.corr_5_coeff_5,
			mc.sims = value)

data.corr_5_coeff_5.melted$model.spec <- factor(data.corr_5_coeff_5.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.corr_5_coeff_5 plot:
plot.corr_5_coeff_5 <- ggplot(data = data.corr_5_coeff_5.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 100) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Slope on latent = 0.05 \n Latent corr with w = 0.05") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.corr_5_coeff_5



###########
####Latent corr 5 is first row, 1, 3, 5, 7 across
data.corr_5_coeff_7 <- read.csv(file = paste(project.dir, "latency/heterogeneity_rho_5_slope7_10000_obs_sims_alt.csv", sep="/"))


data.corr_5_coeff_7 <- data.corr_5_coeff_7 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.corr_5_coeff_7.melted <- data.corr_5_coeff_7 %>% 
				gather(data.corr_5_coeff_7)

data.corr_5_coeff_7.melted <- rename(data.corr_5_coeff_7.melted , 
			model.spec = data.corr_5_coeff_7,
			mc.sims = value)

data.corr_5_coeff_7.melted$model.spec <- factor(data.corr_5_coeff_7.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.corr_5_coeff_7 plot:
plot.corr_5_coeff_7 <- ggplot(data = data.corr_5_coeff_7.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 100) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Slope on latent = 0.07 \n Latent corr with w = 0.05") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.corr_5_coeff_7



###################
###First row:

first.row <- ggarrange(plot.corr_5_coeff_1, plot.corr_5_coeff_3,
		plot.corr_5_coeff_5, plot.corr_5_coeff_7,
		 ncol = 4, nrow = 1)

first.row




###################
##2nd row, corr_10


data.corr_10_coeff_1 <- read.csv(file = paste(project.dir, "latency/heterogeneity_rho_10_slope1_10000_obs_sims_alt.csv", sep="/"))


data.corr_10_coeff_1 <- data.corr_10_coeff_1 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.corr_10_coeff_1.melted <- data.corr_10_coeff_1 %>% 
				gather(data.corr_10_coeff_1)

data.corr_10_coeff_1.melted <- rename(data.corr_10_coeff_1.melted , 
			model.spec = data.corr_10_coeff_1,
			mc.sims = value)

data.corr_10_coeff_1.melted$model.spec <- factor(data.corr_10_coeff_1.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.corr_10_coeff_1 plot:
plot.corr_10_coeff_1 <- ggplot(data = data.corr_10_coeff_1.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 100) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Slope on latent = 0.01 \n Latent corr with w = 0.10") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.corr_10_coeff_1


############
data.corr_10_coeff_3 <- read.csv(file = paste(project.dir, "latency/heterogeneity_rho_10_slope3_10000_obs_sims_alt.csv", sep="/"))


data.corr_10_coeff_3 <- data.corr_10_coeff_3 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.corr_10_coeff_3.melted <- data.corr_10_coeff_3 %>% 
				gather(data.corr_10_coeff_3)

data.corr_10_coeff_3.melted <- rename(data.corr_10_coeff_3.melted , 
			model.spec = data.corr_10_coeff_3,
			mc.sims = value)

data.corr_10_coeff_3.melted$model.spec <- factor(data.corr_10_coeff_3.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.corr_10_coeff_3 plot:
plot.corr_10_coeff_3 <- ggplot(data = data.corr_10_coeff_3.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 100) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Slope on latent = 0.03 \n Latent corr with w = 0.10") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.corr_10_coeff_3


###########

data.corr_10_coeff_5 <- read.csv(file = paste(project.dir, "latency/heterogeneity_rho_10_slope5_10000_obs_sims_alt.csv", sep="/"))


data.corr_10_coeff_5 <- data.corr_10_coeff_5 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.corr_10_coeff_5.melted <- data.corr_10_coeff_5 %>% 
				gather(data.corr_10_coeff_5)

data.corr_10_coeff_5.melted <- rename(data.corr_10_coeff_5.melted , 
			model.spec = data.corr_10_coeff_5,
			mc.sims = value)

data.corr_10_coeff_5.melted$model.spec <- factor(data.corr_10_coeff_5.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.corr_10_coeff_5 plot:
plot.corr_10_coeff_5 <- ggplot(data = data.corr_10_coeff_5.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 100) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Slope on latent = 0.05 \n Latent corr with w = 0.10") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.corr_10_coeff_5



###########
data.corr_10_coeff_7 <- read.csv(file = paste(project.dir, "latency/heterogeneity_rho_10_slope7_10000_obs_sims_alt.csv", sep="/"))


data.corr_10_coeff_7 <- data.corr_10_coeff_7 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.corr_10_coeff_7.melted <- data.corr_10_coeff_7 %>% 
				gather(data.corr_10_coeff_7)

data.corr_10_coeff_7.melted <- rename(data.corr_10_coeff_7.melted , 
			model.spec = data.corr_10_coeff_7,
			mc.sims = value)

data.corr_10_coeff_7.melted$model.spec <- factor(data.corr_10_coeff_7.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.corr_10_coeff_7 plot:
plot.corr_10_coeff_7 <- ggplot(data = data.corr_10_coeff_7.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 100) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Slope on latent = 0.07 \n Latent corr with w = 0.10") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.corr_10_coeff_7



##################33
####Next up is corr_20

data.corr_20_coeff_1 <- read.csv(file = paste(project.dir, "latency/heterogeneity_rho_20_slope1_10000_obs_sims_alt.csv", sep="/"))


data.corr_20_coeff_1 <- data.corr_20_coeff_1 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.corr_20_coeff_1.melted <- data.corr_20_coeff_1 %>% 
				gather(data.corr_20_coeff_1)

data.corr_20_coeff_1.melted <- rename(data.corr_20_coeff_1.melted , 
			model.spec = data.corr_20_coeff_1,
			mc.sims = value)

data.corr_20_coeff_1.melted$model.spec <- factor(data.corr_20_coeff_1.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.corr_20_coeff_1 plot:
plot.corr_20_coeff_1 <- ggplot(data = data.corr_20_coeff_1.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 200) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Slope on latent = 0.01 \n Latent corr with w = 0.20") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.corr_20_coeff_1




data.corr_20_coeff_3 <- read.csv(file = paste(project.dir, "latency/heterogeneity_rho_20_slope3_10000_obs_sims_alt.csv", sep="/"))



data.corr_20_coeff_3 <- data.corr_20_coeff_3 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.corr_20_coeff_3.melted <- data.corr_20_coeff_3 %>% 
				gather(data.corr_20_coeff_3)

data.corr_20_coeff_3.melted <- rename(data.corr_20_coeff_3.melted , 
			model.spec = data.corr_20_coeff_3,
			mc.sims = value)

data.corr_20_coeff_3.melted$model.spec <- factor(data.corr_20_coeff_3.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.corr_20_coeff_3 plot:
plot.corr_20_coeff_3 <- ggplot(data = data.corr_20_coeff_3.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 200) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Slope on latent = 0.03 \n Latent corr with w = 0.20") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.corr_20_coeff_3


###########
####Latent corr 5 is first row, 1, 3, 5, 7 across
data.corr_20_coeff_5 <- read.csv(file = paste(project.dir, "latency/heterogeneity_rho_20_slope5_10000_obs_sims_alt.csv", sep="/"))



data.corr_20_coeff_5 <- data.corr_20_coeff_5 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.corr_20_coeff_5.melted <- data.corr_20_coeff_5 %>% 
				gather(data.corr_20_coeff_5)

data.corr_20_coeff_5.melted <- rename(data.corr_20_coeff_5.melted , 
			model.spec = data.corr_20_coeff_5,
			mc.sims = value)

data.corr_20_coeff_5.melted$model.spec <- factor(data.corr_20_coeff_5.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.corr_20_coeff_5 plot:
plot.corr_20_coeff_5 <- ggplot(data = data.corr_20_coeff_5.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 200) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Slope on latent = 0.05 \n Latent corr with w = 0.20") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.corr_20_coeff_5


####
data.corr_20_coeff_7 <- read.csv(file = paste(project.dir, "latency/heterogeneity_rho_20_slope7_10000_obs_sims_alt.csv", sep="/"))


data.corr_20_coeff_7 <- data.corr_20_coeff_7 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.corr_20_coeff_7.melted <- data.corr_20_coeff_7 %>% 
				gather(data.corr_20_coeff_7)

data.corr_20_coeff_7.melted <- rename(data.corr_20_coeff_7.melted , 
			model.spec = data.corr_20_coeff_7,
			mc.sims = value)

data.corr_20_coeff_7.melted$model.spec <- factor(data.corr_20_coeff_7.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.corr_20_coeff_7 plot:
plot.corr_20_coeff_7 <- ggplot(data = data.corr_20_coeff_7.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 200) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Slope on latent = 0.07 \n Latent corr with w = 0.20") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.corr_20_coeff_7



##################33
####second to last is corr_40

data.corr_40_coeff_1 <- read.csv(file = paste(project.dir, "latency/heterogeneity_rho_40_slope1_10000_obs_sims_alt.csv", sep="/"))


data.corr_40_coeff_1 <- data.corr_40_coeff_1 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.corr_40_coeff_1.melted <- data.corr_40_coeff_1 %>% 
				gather(data.corr_40_coeff_1)

data.corr_40_coeff_1.melted <- rename(data.corr_40_coeff_1.melted , 
			model.spec = data.corr_40_coeff_1,
			mc.sims = value)

data.corr_40_coeff_1.melted$model.spec <- factor(data.corr_40_coeff_1.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.corr_40_coeff_1 plot:
plot.corr_40_coeff_1 <- ggplot(data = data.corr_40_coeff_1.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 200) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Slope on latent = 0.01 \n Latent corr with w = 0.40") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.corr_40_coeff_1




data.corr_40_coeff_3 <- read.csv(file = paste(project.dir, "latency/heterogeneity_rho_40_slope3_10000_obs_sims_alt.csv", sep="/"))


data.corr_40_coeff_3 <- data.corr_40_coeff_3 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.corr_40_coeff_3.melted <- data.corr_40_coeff_3 %>% 
				gather(data.corr_40_coeff_3)

data.corr_40_coeff_3.melted <- rename(data.corr_40_coeff_3.melted , 
			model.spec = data.corr_40_coeff_3,
			mc.sims = value)

data.corr_40_coeff_3.melted$model.spec <- factor(data.corr_40_coeff_3.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.corr_40_coeff_3 plot:
plot.corr_40_coeff_3 <- ggplot(data = data.corr_40_coeff_3.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 200) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Slope on latent = 0.03 \n Latent corr with w = 0.40") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.corr_40_coeff_3


###########
####Latent corr 5 is first row, 1, 3, 5, 7 across
data.corr_40_coeff_5 <- read.csv(file = paste(project.dir, "latency/heterogeneity_rho_40_slope5_10000_obs_sims_alt.csv", sep="/"))


data.corr_40_coeff_5 <- data.corr_40_coeff_5 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.corr_40_coeff_5.melted <- data.corr_40_coeff_5 %>% 
				gather(data.corr_40_coeff_5)

data.corr_40_coeff_5.melted <- rename(data.corr_40_coeff_5.melted , 
			model.spec = data.corr_40_coeff_5,
			mc.sims = value)

data.corr_40_coeff_5.melted$model.spec <- factor(data.corr_40_coeff_5.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.corr_40_coeff_5 plot:
plot.corr_40_coeff_5 <- ggplot(data = data.corr_40_coeff_5.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 250) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Slope on latent = 0.05 \n Latent corr with w = 0.40") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.corr_40_coeff_5


####
data.corr_40_coeff_7 <- read.csv(file = paste(project.dir, "latency/heterogeneity_rho_40_slope7_10000_obs_sims_alt.csv", sep="/"))


data.corr_40_coeff_7 <- data.corr_40_coeff_7 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.corr_40_coeff_7.melted <- data.corr_40_coeff_7 %>% 
				gather(data.corr_40_coeff_7)

data.corr_40_coeff_7.melted <- rename(data.corr_40_coeff_7.melted , 
			model.spec = data.corr_40_coeff_7,
			mc.sims = value)

data.corr_40_coeff_7.melted$model.spec <- factor(data.corr_40_coeff_7.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.corr_40_coeff_7 plot:
plot.corr_40_coeff_7 <- ggplot(data = data.corr_40_coeff_7.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 350) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Slope on latent = 0.07 \n Latent corr with w = 0.40") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.corr_40_coeff_7








################

###first page:  4x2 with first two rows - 1, 3, 5, 7 and 0.05, 0.1

##second page:  4x2 with second two rows - 1, 3, 5, 7 and 0.2, 0.55


smaller.plots.panel.1 <- ggarrange(plot.corr_5_coeff_1, plot.corr_5_coeff_3,	plot.corr_5_coeff_5, 
		plot.corr_5_coeff_7, 
		plot.corr_10_coeff_1, plot.corr_10_coeff_3,	plot.corr_10_coeff_5, 
		plot.corr_10_coeff_7, 
		 ncol = 4, nrow = 2)
smaller.plots.panel.1


##############

smaller.plots.panel.1.extended <- ggarrange(plot.corr_5_coeff_1, plot.corr_5_coeff_3,	plot.corr_5_coeff_5, 
		plot.corr_5_coeff_7, 
		plot.corr_10_coeff_1, plot.corr_10_coeff_3,	plot.corr_10_coeff_5, 
		plot.corr_10_coeff_7, 
		plot.corr_20_coeff_1, plot.corr_20_coeff_3,	plot.corr_20_coeff_5, 
		plot.corr_20_coeff_7, 
		 ncol = 4, nrow = 3)
smaller.plots.panel.1.extended





###################
###Panel.2 temp

smaller.plots.panel.2 <- ggarrange(plot.corr_20_coeff_1, plot.corr_20_coeff_3,	
		plot.corr_20_coeff_5, 
		plot.corr_20_coeff_7, 
		plot.corr_40_coeff_1, plot.corr_40_coeff_3,	plot.corr_40_coeff_5, 
		plot.corr_40_coeff_7, 
		 ncol = 4, nrow = 2)
smaller.plots.panel.2






##################33
####Finally, corr_55

data.corr_55_coeff_1 <- read.csv(file = paste(project.dir, "latency/heterogeneity_rho_55_slope1_10000_obs_sims_alt.csv", sep="/"))


data.corr_55_coeff_1 <- data.corr_55_coeff_1 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.corr_55_coeff_1.melted <- data.corr_55_coeff_1 %>% 
				gather(data.corr_55_coeff_1)

data.corr_55_coeff_1.melted <- rename(data.corr_55_coeff_1.melted , 
			model.spec = data.corr_55_coeff_1,
			mc.sims = value)

data.corr_55_coeff_1.melted$model.spec <- factor(data.corr_55_coeff_1.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.corr_55_coeff_1 plot:
plot.corr_55_coeff_1 <- ggplot(data = data.corr_55_coeff_1.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 200) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Slope on latent = 0.01 \n Latent corr with w = 0.55") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.corr_55_coeff_1




data.corr_55_coeff_3 <- read.csv(file = paste(project.dir, "latency/heterogeneity_rho_55_slope3_10000_obs_sims_alt.csv", sep="/"))



data.corr_55_coeff_3 <- data.corr_55_coeff_3 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.corr_55_coeff_3.melted <- data.corr_55_coeff_3 %>% 
				gather(data.corr_55_coeff_3)

data.corr_55_coeff_3.melted <- rename(data.corr_55_coeff_3.melted , 
			model.spec = data.corr_55_coeff_3,
			mc.sims = value)

data.corr_55_coeff_3.melted$model.spec <- factor(data.corr_55_coeff_3.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.corr_55_coeff_3 plot:
plot.corr_55_coeff_3 <- ggplot(data = data.corr_55_coeff_3.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 200) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Slope on latent = 0.03 \n Latent corr with w = 0.55") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.corr_55_coeff_3


###########
####Latent corr 5 is first row, 1, 3, 5, 7 across
data.corr_55_coeff_5 <- read.csv(file = paste(project.dir, "latency/heterogeneity_rho_55_slope5_10000_obs_sims_alt.csv", sep="/"))


data.corr_55_coeff_5 <- data.corr_55_coeff_5 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.corr_55_coeff_5.melted <- data.corr_55_coeff_5 %>% 
				gather(data.corr_55_coeff_5)

data.corr_55_coeff_5.melted <- rename(data.corr_55_coeff_5.melted , 
			model.spec = data.corr_55_coeff_5,
			mc.sims = value)

data.corr_55_coeff_5.melted$model.spec <- factor(data.corr_55_coeff_5.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.corr_55_coeff_5 plot:
plot.corr_55_coeff_5 <- ggplot(data = data.corr_55_coeff_5.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 400) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Slope on latent = 0.05 \n Latent corr with w = 0.55") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.corr_55_coeff_5


####
data.corr_55_coeff_7 <- read.csv(file = paste(project.dir, "latency/heterogeneity_rho_55_slope7_10000_obs_sims_alt.csv", sep="/"))


data.corr_55_coeff_7 <- data.corr_55_coeff_7 %>% 
		select(forest.ate, rd.late, ols.ate)
	

data.corr_55_coeff_7.melted <- data.corr_55_coeff_7 %>% 
				gather(data.corr_55_coeff_7)

data.corr_55_coeff_7.melted <- rename(data.corr_55_coeff_7.melted , 
			model.spec = data.corr_55_coeff_7,
			mc.sims = value)

data.corr_55_coeff_7.melted$model.spec <- factor(data.corr_55_coeff_7.melted$model.spec, 
			levels = c("forest.ate", "rd.late", "ols.ate"))

######.corr_55_coeff_7 plot:
plot.corr_55_coeff_7 <- ggplot(data = data.corr_55_coeff_7.melted, aes(((mc.sims - 0.02)/0.02)*100, fill = model.spec)) + 
	geom_hline(yintercept=0,  linewidth = 0.3) + 
	geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") + 
	geom_density( trim = FALSE, alpha = 0.2) + 
	theme_minimal() +
	xlim(-100, 400) + 
	ylim(0, 0.05) + 
	xlab("bias (%)") + 
	ylab("density") +
	ggtitle("Slope on latent = 0.07 \n Latent corr with w = 0.55") + 
	scale_fill_manual(values = c("grey", "#0072B2", "purple"), 
		name = "Model", labels = c("Causal Forest", "RDD", "OLS")) +
	theme(legend.position = "null") + 
	theme(plot.title = element_text(hjust = 0.5))

plot.corr_55_coeff_7


##########3
smaller.plots.panel.2 <- ggarrange(plot.corr_20_coeff_1, plot.corr_20_coeff_3,	
		plot.corr_20_coeff_5, 
		plot.corr_20_coeff_7, 
		plot.corr_40_coeff_1, plot.corr_40_coeff_3,	plot.corr_40_coeff_5, 
		plot.corr_40_coeff_7, 
		plot.corr_55_coeff_1, plot.corr_55_coeff_3,	plot.corr_55_coeff_5, 
		plot.corr_55_coeff_7, 
		 ncol = 4, nrow = 3)

smaller.plots.panel.2



all.panels <- ggarrange(plot.corr_5_coeff_1, plot.corr_5_coeff_3,	plot.corr_5_coeff_5, 
                                            plot.corr_5_coeff_7, 
                                            plot.corr_10_coeff_1, plot.corr_10_coeff_3,	plot.corr_10_coeff_5, 
                                            plot.corr_10_coeff_7, 
                                            plot.corr_20_coeff_1, plot.corr_20_coeff_3,	plot.corr_20_coeff_5, 
                                            plot.corr_20_coeff_7, 
                        plot.corr_40_coeff_1, plot.corr_40_coeff_3,	plot.corr_40_coeff_5, 
                        plot.corr_40_coeff_7, 
                        plot.corr_55_coeff_1, plot.corr_55_coeff_3,	plot.corr_55_coeff_5, 
                        plot.corr_55_coeff_7,
                                            ncol = 4, nrow = 5)
all.panels

ggsave(all.panels, 	
       filename = paste(project.dir, "output/Figure_4.pdf", sep="/"),
       width = 10, height = 8, units = "in")




