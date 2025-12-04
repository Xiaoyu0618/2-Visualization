# Part 1. Install and load packages
install.packages("corrplot")
install.packages("RColorBrewer")
library(RColorBrewer)
library(corrplot)
library(ggplot2)
library(ggpubr)
library(openxlsx)

# Part 2. Set working directory and read data
setwd("C:\\Users\\yu\\Desktop")
td <- read.xlsx("RelationNEW.xlsx")

# Part 3. Compute correlation matrix and significance test
cor(td, method = "spearman")
tdc <- cor(td, method = "pearson")

addcol <- colorRampPalette(c("red", "white", "blue"))
testRes <- cor.mtest(td, method = "pearson", conf.level = 0.95)

# Part 4. Main corrplot with significance stars
corrplot(
  tdc,
  method  = "square",
  col     = addcol(10),
  type    = "upper",
  tl.col  = "black",
  tl.cex  = 1.2,
  family  = "serif",
  tl.pos  = "lt",
  p.mat   = testRes$p,
  diag    = TRUE,
  sig.level = c(0.001, 0.01, 0.05),
  pch.cex = 2.3,
  insig   = "label_sig",
  pch.col = "black"
)

corrplot(
  tdc,
  method  = "number",
  type    = "lower",
  col     = addcol(10),
  tl.col  = "n",
  tl.cex  = 0.8,
  family  = "serif",
  tl.pos  = "n",
  order   = "AOE",
  add     = TRUE
)

# Part 5. Additional corrplot variants

# 5.1 Shade-based correlation heatmap
corrplot(tdc, method = "shade")
corrplot(tdc, method = "shade", order = "AOE", diag = FALSE)

# 5.2 Ellipse-based correlation plot
corrplot(
  tdc,
  method = "ellipse",
  type   = "upper",
  tl.col = "black",
  tl.cex = 1.2,
  tl.srt = 45
)

# 5.3 Combined: ellipse (upper) + numbers (lower)
corrplot(
  tdc,
  method = "ellipse",
  type   = "upper",
  tl.col = "black",
  tl.cex = 0.8,
  tl.srt = 45,
  tl.pos = "lt"
)
corrplot(
  tdc,
  method = "number",
  type   = "lower",
  tl.col = "n",
  tl.cex = 0.8,
  tl.pos = "n",
  add    = TRUE
)

# 5.4 Pie style with custom color palette
addcol <- colorRampPalette(c("red", "white", "blue"))
corrplot(
  tdc,
  method = "pie",
  type   = "upper",
  col    = addcol(100),
  tl.col = "black",
  tl.cex = 0.8,
  tl.srt = 45,
  tl.pos = "lt"
)
corrplot(
  tdc,
  method = "number",
  type   = "lower",
  col    = addcol(100),
  tl.col = "n",
  tl.cex = 0.8,
  tl.pos = "n",
  add    = TRUE
)

