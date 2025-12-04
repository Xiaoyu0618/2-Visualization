library(ggplot2)
library(ggExtra)
library(readxl)
library(ggpubr)
library(gcookbook)
library(ggthemes)

# Part 1. Read data
data <- read_excel("C:/Users/yu/Desktop/Factors.xlsx")
data

# Part 2. Scatter plot with regression line and correlation annotation
plot.scatter <- ggplot(data, aes(Space, Income)) +
  geom_point(color = "red", shape = 21, size = 4) +
  theme_bw() +
  geom_smooth(method = "lm", color = "blue", fill = "grey") +
  stat_cor(p.accuracy = 0.01, r.accuracy = 0.01, method = "pearson", size = 6) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 22)) +
  theme(text = element_text(size = 20, family = "serif"))

ggMarginal(plot.scatter, type = "densigram", fill = 3, alpha = 0.5)

plot.scatter
