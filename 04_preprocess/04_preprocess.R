##04_preprocess

library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(dplyr)

options(warn=-1)
load("../03_integrated_total_jeju/03_jeju_data.rdata")
head(apt_price, 2)
apt_price <- apt_price %>% select(-silicate)
head(apt_price)
bjd_code <- read.csv("Jeju_sigun_code.csv", encoding="UTF-8")
bjd_code
soil <- merge(apt_price, bjd_code, by="BJD_Code")
sapply(soil, function(x) {sum(is.na(x))})
toyang <- soil
head(toyang)
table(is.na(toyang))
sapply(toyang, function(x) {sum(is.na(x))})
toyang <- na.omit(toyang)
table(is.na(toyang))

toyang <- toyang %>% mutate(month = str_sub(day, start=5, end=6))
toyang <- toyang %>% mutate(days = str_sub(day, start=7, end=8))
toyang <- toyang %>% select(-day)
toyang <- toyang %>% rename(day=days)
toyang <- toyang %>% rename(code=BJD_Code)
toyang <- toyang %>% relocate(c(month, day), .after=year)

head(toyang$acid, 2)

toyang <- as.data.frame(apply(toyang, 2, str_trim))
head(toyang$acid)

sapply(toyang, class)
toyang$acid <- as.numeric(toyang$acid)
toyang$phosphate <- as.numeric(toyang$phosphate)
toyang$OM <- as.numeric(toyang$OM)
toyang$Mg <- as.numeric(toyang$Mg)
toyang$K <- as.numeric(toyang$K)
toyang$Ca <- as.numeric(toyang$Ca)
toyang$EC <- as.numeric(toyang$EC)

str(toyang)


toyang <- toyang %>%  mutate(ymd=make_date(year, month, day))
toyang$ym <- floor_date(toyang$ymd, "year")
head(toyang, 2)

toyang$cnt <- 1
head(toyang)

#dir.create("../04_preprocess")
save(toyang, file = "../04_preprocess/04_preprocess_Jeju.rdata")
write.csv(toyang, "../04_preprocess/04_preprocess_Jeju.csv")




