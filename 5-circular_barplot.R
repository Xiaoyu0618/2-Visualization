# Part 1. Load required packages
library(tidyverse)
library(viridis)
library(xlsx)
library(ggplot2)
library(ggpubr)
library(dplyr)

# Part 2. Read and view data
setwd("C:\\Users\\yu\\Desktop")
data <- read.xlsx("clcd1213 .xlsx", sheetName="f5circle")
head(data)

# Part 3. Data transformation
data <- data %>% gather(key = "observation", value="value", -c(1,2))

empty_bar <- 2
nObsType <- nlevels(as.factor(data$observation))
to_add <- data.frame(matrix(NA, empty_bar*nlevels(data$group)*nObsType, ncol(data)))
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar*nObsType)
data <- rbind(data, to_add)
data <- data %>% arrange(group, individual)
data$id <- rep(seq(1, nrow(data)/nObsType), each=nObsType)

# Part 4. Labels for each bar
label_data <- data %>%
  dplyr::group_by(id, individual) %>%
  dplyr::summarize(tot=sum(value))

number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) / number_of_bar
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# Part 5. Baseline segment settings
base_data <- data %>% group_by(group) %>%
  summarize(start=min(id), end=max(id) - empty_bar) %>% rowwise() %>%
  mutate(title=mean(c(start, end)))

grid_data <- base_data
grid_data$end <- grid_data$end[c(nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Part 6. Circular stacked barplot
p <- ggplot(data) +      
  geom_hline(yintercept = c(0, 0.25, 0.5, 0.75, 1), lty="solid",
             color = c("black", rep("grey80", 4)), linewidth = 0.8) +
  geom_bar(aes(x=as.factor(id), y=value, fill=observation),
           stat="identity", alpha=1) +
  scale_fill_manual(values = c(
    "#228B22","#70AD47","#C5E0B4",
    "#FFE699","#FDEF68","#ED7D31","#FF0000"
  )) +
  geom_segment(data=grid_data,
               aes(x=end, y=c(0,0.25,0.5,0.75,1),
                   xend=start, yend=c(0,0.25,0.5,0.75,1)),
               colour="grey", alpha=1, size=0.3, inherit.aes = FALSE) +
  ggplot2::annotate("text",
                    x=rep(max(data$id),5),
                    y=c(0,0.25,0.5,0.75,1),
                    label=c("0","0.25","0.5","0.75","1"),
                    color="grey", size=6, fontface="bold", hjust=1) +
  ylim(-0.4, max(label_data$tot, na.rm=TRUE)) +
  theme_minimal()

p

p2 <- p +
  theme(legend.position="none",
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.grid=element_blank(),
        plot.margin=unit(rep(-1,4), "cm")) +
  coord_polar() +
  geom_text(data=label_data,
            aes(x=id, y=tot, label=individual, hjust=hjust),
            color="black", fontface="bold", alpha=0.6,
            size=8, angle=label_data$angle,
            inherit.aes=FALSE)

p2

p3 <- p2 +
  geom_text(data=base_data,
            aes(x=title, y=-0.2, label=group,color=group),
            hjust=c(1,0,0,0,1,1),
            size=7, alpha=0.8,
            fontface="bold", inherit.aes=FALSE)

p3

p3 + geom_text(data=base_data,
               aes(x=title, y=-1, label=group),
               hjust=c(1,1,0,0),
               angle=c(335,250,135,60,30,0),
               colour="black", alpha=0.8,
               size=4, fontface="bold",
               inherit.aes=FALSE)
