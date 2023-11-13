##Fig.3
#You may need to install the Korean language pack.

rm(list=ls())
library(dplyr)
library(ggplot2)
library(lubridate)
library(hrbrthemes)
library(ggridges)
library(tidyverse)
library(stringr)

practice <- read.csv("citrus_practice.csv", encoding = "UTF-8")
glimpse(practice)

practice$acted_at <- as.Date(practice$acted_at)
practice$month <- month(practice$acted_at)
practice <- practice %>% filter(type != "관수")
practice$type <- str_replace(practice$type, "적과", "Thinning")
practice$type <- str_replace(practice$type, "피복", "Mulching")
practice$type <- str_replace(practice$type, "방제", "Spraying")
practice$type <- str_replace(practice$type, "시비", "Fertilization")
practice$type <- str_replace(practice$type, "전정", "Pruning")
practice$type <- factor(practice$type, levels = c("Pruning", "Fertilization", "Spraying", "Mulching", "Thinning"))

ggplot(practice, aes(y=as.factor(type), x=month, fill=type)) +
  geom_density_ridges(alpha=0.7, scale=1, jittered_points=TRUE, aes(point_color=type)) +
  theme_ridges() +
  theme_bw() +
  scale_x_continuous(breaks=seq(1, 12, 2), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme(legend.position = "none") +
  labs(x = "Month", y = "Agricultural practice (No. of trials)")

