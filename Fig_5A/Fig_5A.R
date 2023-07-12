##Fig.5A

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
head(dang_sel)
dang_sel$average

head(farm_iab)
farm_iab$farm_id <- farm_iab$farm_id |> str_replace("i", "I")
names(farm_iab)[4] <- c("Position")

ddo <- ggplot(farm_iab, aes(x=monthweek, y=brix, fill = Position)) +
  geom_boxplot(aes(fill=Position), alpha=0.8, outlier.shape = NA) +
  geom_jitter(aes(color=Position), alpha=0.5, shape=16, position=position_jitter(0.2)) +
  theme_bw() +
  theme(legend.position = c(0.9, 0.15), plot.title = element_text(size=10),
        legend.background = element_rect(fill='transparent')) +
  labs(title="Citrus orchard: Iab",
       x = "Week-month", 
       y = "Sugar content (°brix)") +
  scale_y_continuous(limits = c(7, 18), breaks=seq(7, 17, 2)) +
  stat_compare_means(aes(group = Position), label = "p.signif", label.y = dang_sel$average+5) +
  scale_x_discrete(labels = c('3-Oct', '4-Oct', '1-Nov', '2-Nov'))

##Fruit size
size_sel <- farm_iab |> group_by(monthweek) |> 
  summarise(average = mean(size))
head(size_sel)


siz <- ggplot(farm_iab, aes(x=monthweek, y=size, fill = Position)) +
  geom_boxplot(aes(fill=Position), alpha=0.8, outlier.shape = NA) +
  geom_jitter(aes(color=Position), alpha=0.5, shape=16, position=position_jitter(0.2)) +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(size=10),
        legend.background = element_rect(fill='transparent')) +
  labs(title="",
       x = "Week-month", 
       y = "Fruit size (mm)") +
  scale_y_continuous(limits = c(40, 80), breaks=seq(40, 80, 10)) +
  stat_compare_means(aes(group = Position), label = "p.signif", label.y = size_sel$average+18) +
  scale_x_discrete(labels = c('3-Oct', '4-Oct', '1-Nov', '2-Nov'))


grid.arrange(ddo, siz, nrow=1)
