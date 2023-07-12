##Fig.4D

rm(list=ls())
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(gridExtra)
united <- read.csv("fruit_raw_grade.csv")
substr(united$farm_id, 1, 1) <- toupper(substr(united$farm_id, 1, 1))

united$mday <- ifelse(mday(united$researched_at) <= 10, 'early',
                      ifelse(mday(united$researched_at) <= 20, 'mid', 'late'))
united$mmday <- paste0(united$mday, "-", united$researched_month)
united$mmday <- factor(united$mmday, levels = c("mid-10", "late-10", "early-11", "mid-11", "late-11"))


select <- united |>
  filter(farm_id %in% c("Hab", "Iab")) |>
  filter(mmday %in% c("mid-10", "late-10", "early-11"))
select$mmday <- str_replace(select$mmday, "early-10", "early-Oct")
select$mmday <- str_replace(select$mmday, "mid-10", "mid-Oct")
select$mmday <- str_replace(select$mmday, "late-10", "late-Oct")
select$mmday <- str_replace(select$mmday, "early-11", "early-Nov")
select$mmday <- factor(select$mmday, levels = c("mid-Oct", "late-Oct", "early-Nov"))
View(select)
str(select)
select$farm_id <- as.factor(select$farm_id)


dang_select <- select |> group_by(mmday) |>
  summarise(average = mean(brix))
dang_select$average

dang <- ggplot(select, aes(x=mmday, y= brix)) +
  theme_bw() +
  geom_boxplot(aes(fill=farm_id), alpha=0.8, outlier.shape = NA) +
  theme(legend.position = "none", legend.title = element_blank()) +
  geom_jitter(aes(color=farm_id), shape=16, position=position_jitter(0.2), alpha=0.5) +
  labs(x = "Week-month", y = "Sugar content (°brix)") +
  ylim(7, 18) +
  stat_compare_means(aes(group = farm_id), label = "p.signif", label.y = dang_select$average+6)


ku_select <- select |> group_by(mmday) |>
  summarise(average = mean(size))
ku_select$average

kui <- ggplot(select, aes(x=mmday, y= size)) +
  theme_bw() +
  geom_boxplot(aes(fill=farm_id), alpha=0.8, outlier.shape = NA) +
  theme(legend.position = "none", legend.title = element_blank()) +
  geom_jitter(aes(color=farm_id), shape=16, position=position_jitter(0.2), alpha=0.5) +
  labs(x = "Week-month", y = "Fruit size (mm)") +
  ylim(40, 80) +
  stat_compare_means(aes(group = farm_id), label = "p.signif", label.y = ku_select$average+25)


grid.arrange(dang, kui, nrow=1)
