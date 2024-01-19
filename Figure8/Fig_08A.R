##Fig.8A
rm(list=ls())

library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(gridExtra)

fruit <- read.csv("citrus_fruit_data_3_replicate.csv", encoding = "UTF-8")
fruit$monthweek <- paste(fruit$researched_month, fruit$researched_week, sep = "-")
fruit$researched_at <- as.Date(fruit$researched_at, format='%Y-%m-%d')
fruit$monthweek <- as.factor(fruit$monthweek)
fruit$position <- factor(fruit$position, levels = c("high", "middle", "low"))
head(fruit)
glimpse(fruit)

farm_iab <- fruit[fruit$farm_id == "iab", ]

##Sugar content
dang_sel <- farm_iab |> group_by(monthweek) |> 
  summarise(average = mean(brix))
#head(dang_sel)
#dang_sel$average

#head(farm_iab)
farm_iab$farm_id <- farm_iab$farm_id |> str_replace("i", "I")
names(farm_iab)[4] <- c("Position")

ddo <- ggplot(farm_iab, aes(x=monthweek, y=brix, fill = Position)) +
  geom_violin(aes(color=Position), trim = FALSE, position=position_dodge(0.5), alpha=0.5) +
  geom_boxplot(aes(color=Position), width = 0.15, position=position_dodge(0.5), alpha=0.5) +
  theme_bw() +
  theme(legend.position = c(0.9, 0.15), plot.title = element_text(size=10),
        legend.background = element_rect(fill='transparent')) +
  labs(title="",
       x = "Month-week", 
       y = "Sugar content (°Bx)") +
  scale_y_continuous(limits = c(7, 18), breaks=seq(7, 17, 2)) +
  scale_x_discrete(labels = c('10-3', '10-4', '11-1', '11-2'))

#stat_compare_means(aes(group = Position), label = "p.signif", label.y = dang_sel$average+5) +
#geom_violin(aes(color=farm_id), trim = FALSE, position=position_dodge(0.5), alpha=0.5) +
#  geom_boxplot(aes(color=farm_id), width = 0.15, position=position_dodge(0.5), alpha=0.5) +


##Fruit size
size_sel <- farm_iab |> group_by(monthweek) |> 
  summarise(average = mean(size))
#head(size_sel)


siz <- ggplot(farm_iab, aes(x=monthweek, y=size, fill = Position)) +
  geom_violin(aes(color=Position), trim = FALSE, position=position_dodge(0.5), alpha=0.5) +
  geom_boxplot(aes(color=Position), width = 0.15, position=position_dodge(0.5), alpha=0.5) +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(size=10),
        legend.background = element_rect(fill='transparent')) +
  labs(title="",
       x = "Month-week", 
       y = "Fruit size (mm)") +
  scale_y_continuous(limits = c(40, 80), breaks=seq(40, 80, 10)) +
  scale_x_discrete(labels = c('10-3', '10-4', '11-1', '11-2'))

#stat_compare_means(aes(group = Position), label = "p.signif", label.y = size_sel$average+18) +

#by( farm_iab$brix, farm_iab$monthweek, summary )
#by( farm_iab$size, farm_iab$monthweek, summary )

jpeg( "Figures/Fig8A.jpg", width = 800, height = 400, quality = 100 )
grid.arrange(ddo, siz, nrow=1)
dev.off()