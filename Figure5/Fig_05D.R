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

fruit_gungcheon <- subset( fruit_raw, cultivar == fruit_raw$cultivar[1] )
#fruit_gungcheon <- fruit_raw %>% filter(cultivar == "궁천")  #궁천 is the cultivar name in Korean. You may need to install the Korean language pack.
#tail(fruit_gungcheon)

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
#View(iabman)
iabman$monthweek <- factor(iabman$monthweek, levels = c("10-3", "10-4", "11-1", "11-2"))

iabman2 <- iabman %>% group_by(monthweek, grade) %>% tally()
head(iabman2)

iabgrim <- ggplot(iabman2, aes(x=monthweek, y=n, fill=grade)) +
  geom_col(position="fill") + theme_bw() +
  ggtitle("Mandarin orchard: Iab") +
  theme(legend.position = "none", plot.title = element_text(size=10),) +
  labs(titile="Mandarin orchard: Iab",
       x='Month-week', y="Proportion", fill="Grade")
#  scale_x_discrete(labels = c('3-Oct', '4-Oct', '1-Nov', '2-Nov'))


##########Only Hab
habman <- fruit_gungcheon |> filter(farm_id == "hab")
#View(habman)
habman$monthweek <- factor(habman$monthweek, levels = c("10-3", "10-5", "11-1", "11-2", "11-3", "11-4"))

#table(habman$monthweek)
#table(iabman$monthweek)

habman2 <- habman %>% group_by(monthweek, grade) %>% tally()
head(habman2)

habgrim <- ggplot(habman2, aes(x=monthweek, y=n, fill=grade)) +
  geom_col(position="fill") + theme_bw() +
  ggtitle("Mandarin orchard: Hab") +
  theme(legend.position = c(0.2, 0.8), plot.title = element_text(size=10),
        legend.background = element_rect(fill="transparent") ) +
  labs(titile="Mandarin orchard: Hab", 
       x='Month-week', y="Proportion", fill="Grade")
#  scale_x_discrete(labels = c('3-Oct', '4-Oct', '1-Nov', '2-Nov'))
# theme(legend.position = "none", plot.title = element_text(size=8),
#       legend.key = element_rect(colour="transparent", fill="transparent"),
#       legend.background=element_rect(fill= alpha("white", 0.5)))

jpeg( "Figures/Fig5D.jpg", width = 700, height = 350, quality = 100 )
grid.arrange(habgrim, iabgrim, nrow=1)
dev.off()