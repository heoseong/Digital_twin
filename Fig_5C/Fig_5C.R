##Fig.5C

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
raw <- read.csv("raw_data.csv")
fruit_raw <- merge(fruit, raw, by = c("farm_id"))


fruit_gungcheon <- fruit_raw %>% filter(cultivar == "궁천")  #궁천 is the cultivar name in Korean. You may need to install the Korean language pack.
tail(fruit_gungcheon)

fruit_gungcheon$class <- case_when(
  fruit_gungcheon$size < 49 ~ "too_small",
  fruit_gungcheon$size < 54 & fruit_gungcheon$brix >= 10 ~ "2S",
  fruit_gungcheon$size < 59 & fruit_gungcheon$brix >= 10 ~ "S",
  fruit_gungcheon$size < 63 & fruit_gungcheon$brix >= 10 ~ "M",
  fruit_gungcheon$size < 67 & fruit_gungcheon$brix >= 10 ~ "L",
  fruit_gungcheon$size <= 71 & fruit_gungcheon$brix >= 10 ~ "2L",
  TRUE ~ "too_large"
)

fruit_gungcheon$class <- factor(fruit_gungcheon$class, levels = c("too_small", "2S", "S", "M", "L", "2L", "too_large"))

fruit_gungcheon$grade <- case_when(
  fruit_gungcheon$class %in% c("2S", "L", "2L") ~ "salable",
  fruit_gungcheon$class %in% c("too_small", "too_large") ~ "unsalable",
  fruit_gungcheon$class %in% c("S", "M") ~ "prime grade"
)

fruit_gungcheon$grade <- factor(fruit_gungcheon$grade, levels = c("unsalable", "salable", "prime grade"))
head(fruit_gungcheon)


##########Only Iab
iabman <- fruit_gungcheon |> filter(farm_id == "iab")
View(iabman)
iabman$monthweek <- factor(iabman$monthweek, levels = c("10-3", "10-4", "11-1", "11-2"))

iabman2 <- iabman %>% group_by(monthweek, grade) %>% tally()
head(iabman2)

ggplot(iabman2, aes(x=monthweek, y=n, fill=grade)) +
  geom_col(position="fill") + theme_bw() +
  theme(legend.position = "right") +
  labs(x='Week-month', y="Percentage (%)", fill="Grade") +
  scale_x_discrete(labels = c('3-Oct', '4-Oct', '1-Nov', '2-Nov'))
