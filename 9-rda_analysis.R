# Part 1. Update R and load packages
install.packages("installr")
library(installr)
updateR()

rm(list = ls())
library(vegan)
library(ggrepel)
library(ggplot2)
library(ggpubr)
library(openxlsx)

# Part 2. Set working directory and read data
setwd("C:\\Users\\yu\\Desktop")
# setwd("H:\\science\\F5-program\\F1-UVA1\\F3data\\P5Fig\\f20240512\\f6RDA")

data  <- read.xlsx("otu.xlsx",   1, rowNames = TRUE)
env   <- read.xlsx("env.xlsx",   1, rowNames = TRUE)
group <- read.xlsx("group.xlsx", 1, rowNames = TRUE)

sampledata <- t(data)

# Part 3. Hellinger transformation and DCA
sampledata <- decostand(sampledata, method = "hellinger")

col <- c("#F8766D", "#7CAE00", "#00BFC4")

dca <- decorana(veg = sampledata)
dca

# Part 4. RDA, scores, and plotting
rda <- rda(sampledata, env, scale = TRUE)

rda.sample <- data.frame(rda$CCA$u[, 1:2])
rda.sample$group <- group$group

rda.env <- data.frame(rda$CCA$biplot[, 1:2])

rda1 <- round(rda$CCA$eig[1] / sum(rda$CCA$eig) * 100, 2)
rda2 <- round(rda$CCA$eig[2] / sum(rda$CCA$eig) * 100, 2)

ggplot(rda.sample, aes(RDA1, RDA2)) +
  geom_point(aes(fill = group, color = "black"),
             size = 7, shape = 21, stroke = 1) +
  xlab(paste("RDA1 ( ", rda1, "%", " )", sep = "")) +
  ylab(paste("RDA2 ( ", rda2, "%", " )", sep = "")) +
  geom_segment(
    data = rda.env,
    aes(x = 0, y = 0, xend = rda.env[, 1], yend = rda.env[, 2]),
    arrow = arrow(length = unit(0.35, "cm"),
                  type = "closed", angle = 22.5),
    linetype = 1, colour = "black", size = 0.6
  ) +
  geom_text_repel(
    data = rda.env,
    segment.colour = "black",
    aes(x = rda.env[, 1], y = rda.env[, 2],
        label = rownames(rda.env)),
    family = "serif", face = "bold", size = 8
  ) +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  theme_bw() +
  theme(
    axis.title = element_text(
      family = "serif", face = "bold",
      size = 20, colour = "black"
    ),
    axis.text = element_text(
      family = "serif", face = "bold",
      size = 18, colour = "black"
    ),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_blank(),
    legend.key.size = unit(20, "point")
  )

# Part 5. Permutation test and summary
envfit <- envfit(rda, env, permutations = 999)
r <- as.matrix(envfit$vectors$r)
p <- as.matrix(envfit$vectors$pvals)
env.p <- cbind(r, p)
colnames(env.p) <- c("r2", "p-value")
KK <- as.data.frame(env.p)
KK$p.adj <- p.adjust(KK$`p-value`, method = "BH")
KK

summary(rda)
RsquareAdj(rda)
