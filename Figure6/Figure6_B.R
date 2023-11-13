##Fig.4B
rm(list=ls())

library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)

practice <- read.csv("practice_groupby.csv")
practice$type <- str_replace(practice$type, "thinning", "Thinning")
practice$type <- str_replace(practice$type, "mulching", "Mulching")
practice$type <- str_replace(practice$type, "pest_control", "Spraying")
practice$type <- str_replace(practice$type, "fertilization", "Fertilization")
practice$type <- str_replace(practice$type, "pruning", "Pruning")

substr(practice$farm_id, 1, 1) <- toupper(substr(practice$farm_id, 1, 1))

agri_prac <- practice |> 
  filter(farm_id %in% c("Hab", "Iab")) |> 
  select(-X)
head(agri_prac)
str(agri_prac)
agri_prac$month <- factor(agri_prac$month)
agri_prac$type <- factor(agri_prac$type, levels = c("Pruning", "Fertilization", "Spraying", "Mulching", "Thinning"))
# agri_prac$month <- str_replace(agri_prac$month, "1", "Jan")
# agri_prac$month <- str_replace(agri_prac$month, "2", "Feb")
# agri_prac$month <- str_replace(agri_prac$month, "3", "Mar")
# agri_prac$month <- str_replace(agri_prac$month, "4", "Apr")
# agri_prac$month <- str_replace(agri_prac$month, "5", "May")
# agri_prac$month <- str_replace(agri_prac$month, "6", "Jun")
# agri_prac$month <- str_replace(agri_prac$month, "7", "Jul")
# agri_prac$month <- str_replace(agri_prac$month, "8", "Aug")
# agri_prac$month <- str_replace(agri_prac$month, "9", "Sep")
# agri_prac$month <- str_replace(agri_prac$month, "10", "Oct")
# agri_prac$month <- str_replace(agri_prac$month, "11", "Nov")
# agri_prac$month <- str_replace(agri_prac$month, "12", "Dec")
# 
# agri_prac$month <- factor(agri_prac$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

ggplot() +
  geom_col(data=agri_prac, aes(x=month, y=count, group=type, fill=farm_id)) +
  facet_wrap(~type, nrow =1) +
  theme_bw() +
  theme(legend.position = "none", legend.title = element_blank()) +
  labs(x = "Month", y="No. of trials") +
#  scale_x_continuous(limits = c(2.5, 9, 1)) +
  scale_y_discrete(limits = c(0, 2, 1)) +
  coord_cartesian(expand = FALSE)
