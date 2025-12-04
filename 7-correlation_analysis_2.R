# Part 1. Install and load packages
install.packages("corrplot")
install.packages("RColorBrewer")
install.packages("GGally")

library(RColorBrewer)
library(corrplot)
library(ggplot2)
library(ggpubr)
library(openxlsx)
library(GGally)

# Part 2. Pairwise correlation plots with GGally
setwd("C:\\Users\\yu\\Desktop")
td <- read.xlsx("RelationNEW.xlsx")

ggpairs(
  td,
  ggplot2::aes(color = Group),
  upper = list(
    continuous = wrap("cor", method = "pearson")
  ),
  lower = list(
    continuous = wrap("smooth", method = "loess", se = FALSE)
  )
) +
  theme(
    axis.text = element_text(colour = "black", size = 13),
    strip.background = element_rect(fill = "lightgrey"),
    strip.text = element_text(
      colour = "black",
      size   = 15,
      face   = "bold"
    )
  ) +
  scale_colour_manual(values = c("#C5E0B4", "#4D8E41")) +
  scale_fill_manual(values = c("#4D8E41", "#C5E0B4"))
