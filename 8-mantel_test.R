# Part 1. Clear environment and load packages
rm(list = ls())

library(readxl)
library(vegan)
library(dplyr)
library(ggcor)
library(ggplot2)
library(openxlsx)
library(tibble)
library(RColorBrewer)

# Part 2. Set working directory and read data
setwd("C:\\Users\\yu\\Desktop")

env1_raw   <- read_excel("env_5y.xlsx")
waibu1_raw <- read_excel("otu.xlsx")

env1 <- env1_raw %>%
  column_to_rownames(var = colnames(env1_raw)[1])

waibu1 <- waibu1_raw %>%
  column_to_rownames(var = colnames(waibu1_raw)[1])

# Part 3. Align samples between matrices
common_samples <- intersect(rownames(env1), rownames(waibu1))

env1   <- env1[common_samples, ]
waibu1 <- waibu1[common_samples, ]

# Part 4. Mantel test
shiyan1_mantel <- mantel_test(
  waibu1,
  env1,
  mantel.fun       = "mantel",
  spec.dist.method = "bray",
  env.dist.method  = "euclidean",
  spec.select      = list(
    "Population Density" = 1
  )
)

# Part 5. Post-process Mantel results for plotting
shiyan1_mantel <- shiyan1_mantel %>%
  mutate(
    df_r = cut(
      r,
      breaks = c(-Inf, 0.25, 0.5, Inf),
      labels = c("< 0.25", "0.25 - 0.5", ">= 0.5")
    ),
    df_p = cut(
      p.value,
      breaks = c(-Inf, 0.01, 0.05, Inf),
      labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")
    ),
    linetype = factor(
      ifelse(r >= 0, "positive", "negative"),
      levels = c("positive", "negative")
    )
  )

# Part 6. Correlation heatmap + Mantel links
p1 <- quickcor(
  env1,
  type      = "upper",
  method    = "pearson",
  show.diag = FALSE,
  cor.test  = TRUE,
  fill      = "r"
) +
  geom_tile(aes(fill = r), color = "black", size = 0.3) +
  scale_fill_distiller(
    palette  = "PiYG",
    direction = 1,
    limits   = c(-1, 1),
    na.value = "grey90"
  ) +
  geom_mark(
    r          = NA,
    sig.thres  = 0.05,
    size       = 6,
    color      = "black"
  ) +
  theme(
    axis.text.x = element_text(
      size   = 13,
      angle  = 90,
      vjust  = 0.5,
      hjust  = 1,
      family = "serif"
    ),
    axis.text.y = element_text(
      size   = 13,
      family = "serif"
    ),
    legend.title = element_text(
      size   = 13,
      family = "serif",
      face   = "bold"
    ),
    legend.text = element_text(
      size   = 13,
      family = "serif"
    )
  ) +
  anno_link(
    shiyan1_mantel,
    aes(
      color    = df_p,
      size     = df_r,
      linetype = linetype
    ),
    label.size    = 5,
    label.family  = "serif",
    label.fontface = 1,
    curvature     = 0.2,
    nudge_x       = 0.2
  ) +
  scale_size_manual(values = c(0.8, 1, 1.3)) +
  scale_color_manual(values = c(
    "#7FBC41",
    "#DE77AE",
    "#D9D9D9"
  )) +
  scale_linetype_manual(
    values = c("positive" = "solid", "negative" = "dashed"),
    labels = c("positive" = "Positive", "negative" = "Negative")
  ) +
  guides(
    fill     = guide_colorbar(title = "Pearson's r",  order = 1),
    size     = guide_legend(title = "Mantel's r",     order = 2),
    linetype = guide_legend(title = "Correlation",    order = 3),
    color    = guide_legend(title = "p-value",        order = 4)
  )

print(p1)
