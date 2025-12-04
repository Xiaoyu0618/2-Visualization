# Part 1. Load packages and import data
library(ggplot2)
library(ggpubr)
library(reshape2)
library(RColorBrewer)
library(xlsx)
library(dplyr)
library(agricolae)
library(tidyverse)
library(ggh4x)

setwd("C:\\Users\\yu\\Desktop")
data2 <- read.xlsx("clcd1213 .xlsx", sheetName = "f4box")
head(data2)

# Part 2. Reshape and prepare data
names(data2)
mydata2 <- melt(
  data2,
  id.vars = c("Plots", "Level", "Layer"),
  variable.name = "Nutrient",
  value.name = "Soil.nutrient"
)
head(mydata2)
str(mydata2)
levels(mydata2$Nutrient)

mydata2$Nutrient <- factor(
  mydata2$Nutrient,
  levels = c("SOC", "TN", "TP", "pH")
)
mydata2$Level <- factor(
  mydata2$Level,
  levels = c("Forest", "Grass", "Farm", "Water", "Unused", "Road", "Buiding"),
  labels = c("Forest", "Grass", "Farm", "Water", "Unused", "Road", "Buiding")
)
mydata2$Layer <- factor(
  mydata2$Layer,
  levels = c("Layer1", "Layer2", "Layer3"),
  labels = c("Da Tong River", "Huang Shui River", "Huang He")
)
str(mydata2)

LE <- unique(mydata2$Level)
LA <- unique(mydata2$Layer)
VAR <- unique(mydata2$Nutrient)

# Part 3. ANOVA and LSD group letters
result_layer <- data.frame()
for (le in 1:7) {
  data_LE <- mydata2[mydata2$Level == LE[le], ]
  for (var in 1:1) {
    data_VAR <- data_LE[data_LE$Nutrient == VAR[var], ]
    aov <- aov(Soil.nutrient ~ Layer, data_VAR)
    lsd <- LSD.test(aov, "Layer")["groups"] %>%
      data.frame() %>%
      mutate(
        Nutrient = VAR[var],
        Level = LE[le]
      )
    lsd$Layer <- rownames(lsd)
    result_layer <- rbind(result_layer, lsd)
  }
}

result_level <- data.frame()
for (la in 1:3) {
  data_LA <- mydata2[mydata2$Layer == LA[la], ]
  for (var in 1:1) {
    data_VAR <- data_LA[data_LA$Nutrient == VAR[var], ]
    aov <- aov(Soil.nutrient ~ Level, data_VAR)
    lsd <- LSD.test(aov, "Level")["groups"] %>%
      data.frame() %>%
      mutate(
        Nutrient = VAR[var],
        Layer = LA[la]
      )
    lsd$Level <- rownames(lsd)
    result_level <- rbind(result_level, lsd)
  }
}

result_level$groups.groups <- toupper(result_level$groups.groups)
colnames(result_level)[1:2] <- c("mean", "level_group")
colnames(result_layer)[1:2] <- c("mean_1", "layer_group")
result <- merge(result_level, result_layer, by = c("Nutrient", "Level", "Layer"))
result$group <- paste0(result$level_group, result$layer_group)

max_data <- aggregate(
  mydata2[, 5],
  by = list(mydata2$Level, mydata2$Layer, mydata2$Nutrient),
  max
) %>% data.frame()
colnames(max_data) <- c("Level", "Layer", "Nutrient", "max")
result <- merge(result, max_data, by = c("Nutrient", "Layer", "Level"))

# Part 4. Faceted boxplots with LSD letters
P1 <-
  ggplot(mydata2, aes(x = Level, y = Soil.nutrient, fill = Layer)) +
  geom_boxplot(position = position_dodge(0.85)) +
  stat_summary(
    fun = "mean",
    shape = 0,
    color = "black",
    size = 0.2,
    position = position_dodge(0.85)
  ) +
  labs(x = "Elevation", y = "Soil.nutrient") +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 20)
  ) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(legend.position = "bottom") +
  theme(
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black")
  ) +
  theme(text = element_text(size = 25, family = "serif")) +
  geom_text(
    data = result,
    aes(x = Level, y = max, label = group, group = Layer),
    position = position_dodge(0.85),
    show.legend = FALSE,
    vjust = -0.2,
    family = "serif",
    size = 6
  ) +
  scale_fill_manual(values = c("#7B4D8A", "#63B2AD", "#FDEF68")) +
  labs(
    x = "  ",
    y = "Persent",
    fill = " "
  )

P1

# Part 5. Adjust y-axis scales and export figure
P2 <- P1 +
  facetted_pos_scales(
    y = list(
      Nutrient == "SOC" ~ scale_y_continuous(
        limits = c(0, 0.5),
        breaks = seq(0, 85, 20)
      )
    )
  )

P2

ggsave("LULC_2022.pdf", width = 17.2974, height = 11.2014)
