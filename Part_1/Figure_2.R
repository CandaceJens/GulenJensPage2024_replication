# Figure 2 --  forest explanation figures

require(ggplot2)
require(gridExtra)
require(plyr)
require(dplyr)

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


set.seed(8675309)
var1 = runif(50)
var2 = runif(50)
treat.prob = runif(50) ### added

plot.data = data.frame(var1 = var1, var2 = var2,
                       tree1 = ifelse(between(var1,0.20,0.75),ifelse(between(var2,0.20,0.75),1,0),0),
                       tree2 = ifelse(between(var1,0.30,0.70),ifelse(between(var2,0.00,0.70),1,0),0),
                       tree3 = ifelse(between(var1,0.45,1.00),ifelse(between(var2,0.00,0.55),1,0),0),
                       dist = sqrt((var1-0.5)^2+(var2-0.5)^2))
plot.data$treated = factor(ifelse(treat.prob < 0.4, "Treated", "Control"), levels = c("Treated", "Control")) ### added
plot.data$treeall = (plot.data$tree1 + 2 * plot.data$tree2 + 1.5 * plot.data$tree3 + 1) * (1 - plot.data$dist)
plot.data$treeall = ifelse(plot.data$treeall < 0, 0, plot.data$treeall)

summary(plot.data)

tree0 <- ggplot(data = plot.data, aes(x = var1, y = var2, shape = treated)) + ### updated
  scale_x_continuous(limits=c(0,1), expand = c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand = c(0,0)) +
  scale_shape_manual(values = c(17,1)) + ### added
  geom_point(size = 1) + 
  geom_point(aes(x = 0.5, y = 0.5), size = 2, shape = 15, color = "blue") + ### updated
  theme_classic() +
  ggtitle(label = "Data") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab(label = "Variable 1") +
  ylab(label = "Variable 2") +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_text(size = 10))

tree1 <- ggplot(data = plot.data, aes(x = var1, y = var2, size = factor(tree1), color = factor(tree1), shape = treated)) + ### updated
  scale_size_manual(values = c(1, 2)) +
  scale_color_manual(values = c("#7fcdbb","#253494")) +
  scale_shape_manual(values = c(17,1)) + ### added
  scale_x_continuous(limits=c(0,1), expand = c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand = c(0,0)) +
  geom_point() + 
  geom_point(aes(x = 0.5, y = 0.5), size = 2, shape = 15, color = "blue") + ### updated
  geom_rect(aes(xmin = 0.20, xmax = 0.75, ymin = 0.20, ymax = 0.75), alpha = 0, color = "black", size = 0.25, linetype = 2) +
  geom_rect(aes(xmin = 0.00, xmax = 0.20, ymin = 0.00, ymax = 0.75), alpha = 0, color = "black", size = 0.25, linetype = 2) +
  geom_rect(aes(xmin = 0.20, xmax = 1.00, ymin = 0.00, ymax = 0.20), alpha = 0, color = "black", size = 0.25, linetype = 2) +
  geom_rect(aes(xmin = 0.75, xmax = 1.00, ymin = 0.20, ymax = 0.75), alpha = 0, color = "black", size = 0.25, linetype = 2) +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 0), alpha = 0, color = "black", size = 0.25, linetype = 2) +
  theme_classic() +
  ggtitle(label = "Tree 1") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab(label = "Variable 1") +
  ylab(label = "Variable 2") +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_text(size = 10))

tree2 <- ggplot(data = plot.data, aes(x = var1, y = var2, size = factor(tree2), color = factor(tree2), shape = treated)) + ### updated
  scale_size_manual(values = c(1, 2)) +
  scale_color_manual(values = c("#7fcdbb","#253494")) +
  scale_shape_manual(values = c(17,1)) + ### added
  scale_x_continuous(limits=c(0,1), expand = c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand = c(0,0)) +
  geom_point() + 
  geom_point(aes(x = 0.5, y = 0.5), size = 2, shape = 15, color = "blue") + ### updated
  geom_rect(aes(xmin = 0.30, xmax = 0.70, ymin = 0.00, ymax = 0.65), alpha = 0, color = "black", size = 0.25, linetype = 2) +
  geom_rect(aes(xmin = 0.00, xmax = 0.30, ymin = 0.00, ymax = 1.00), alpha = 0, color = "black", size = 0.25, linetype = 2) +
  geom_rect(aes(xmin = 0.70, xmax = 1.00, ymin = 0.00, ymax = 0.65), alpha = 0, color = "black", size = 0.25, linetype = 2) +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 0), alpha = 0, color = "black", size = 0.25, linetype = 2) +
  theme_classic() +
  ggtitle(label = "Tree 2") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab(label = "Variable 1") +
  ylab(label = "Variable 2") +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_text(size = 10))

tree3 <- ggplot(data = plot.data, aes(x = var1, y = var2, size = factor(tree3), color = factor(tree3), shape = treated)) + ### updated
  scale_size_manual(values = c(1, 2)) +
  scale_color_manual(values = c("#7fcdbb","#253494")) +
  scale_shape_manual(values = c(17,1)) + ### added
  scale_x_continuous(limits=c(0,1), expand = c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand = c(0,0)) +
  geom_point() + 
  geom_point(aes(x = 0.5, y = 0.5), size = 2, shape = 15, color = "blue") + ### updated
  geom_rect(aes(xmin = 0.45, xmax = 1.00, ymin = 0.00, ymax = 0.55), alpha = 0, color = "black", size = 0.25, linetype = 2) +
  geom_rect(aes(xmin = 0.00, xmax = 0.45, ymin = 0.00, ymax = 0.65), alpha = 0, color = "black", size = 0.25, linetype = 2) +
  geom_rect(aes(xmin = 0.00, xmax = 0.45, ymin = 0.65, ymax = 1.00), alpha = 0, color = "black", size = 0.25, linetype = 2) +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 0), alpha = 0, color = "black", size = 0.25, linetype = 2) +
  theme_classic() +
  ggtitle(label = "Tree 3") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab(label = "Variable 1") +
  ylab(label = "Variable 2") +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_text(size = 10))

tree.all <- ggplot(data = plot.data, aes(x = var1, y = var2, size = treeall, color = treeall, shape = treated)) + ### updated
  scale_size_continuous(range = c(1, 3)) +
  scale_color_continuous(low = "#c7e9b4", high = "#253494") +
  scale_shape_manual(values = c(17,1)) + ### added
  geom_point() + 
  geom_point(aes(x = 0.5, y = 0.5), size = 2, shape = 15, color = "blue") + ### updated
  scale_x_continuous(limits=c(0,1), expand = c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand = c(0,0)) +
  theme_classic() +
  ggtitle(label = "Forest") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab(label = "Variable 1") +
  ylab(label = "Variable 2") +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_text(size = 10))


fig.layout <- rbind(c(1, 2),
                    c(3, 4),
                    c(5, 5),
                    c(5, 5))

ggsave(filename = paste(out.folder, "Figure_2.png", sep="/"),
       width = 5, height = 8, units = "in",
       arrangeGrob(tree0, tree1, tree2, tree3, tree.all,
                   layout_matrix = fig.layout))



