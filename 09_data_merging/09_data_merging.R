##09_data_merging

#####soil_selected_farm.csv
rm(list=ls())
library(dplyr)
library(tidyverse)
load("./04_preprocess/04_preprocess_Jeju.rdata")    
load("./05_geocoding/05_Jeju_geocoding.rdata")
fruit <- toyang[toyang$type == "4", ]  #Select only fruit orchards
View(fruit)
write.csv(fruit, "./09_data_merging/soil_data_for_orchard.csv")

raw <- read.csv("./09_data_merging/raw_data.csv")
View(raw)

fruit_farm_geodata <- inner_join(fruit, raw, 
                                 by = c("jibun" = "address")) # 결합
fruit_farm_geodata <- na.omit(fruit_farm_geodata)   # 결측치 제거
View(fruit_farm_geodata)
write.csv(fruit_farm_geodata, "./09_data_merging/soil_data_for_citrus_farm_list_integration.csv")

soil_selected_farm <- fruit_farm_geodata %>% 
  filter(farm_id %in% c("iab", "nab", "qab", "hab")) %>% 
  filter(year %in% c("2021", "2022"))

View(soil_selected_farm) ## Manually selected citrus farm based on this file



##### practice_groupby.csv
library(lubridate)
practice <- read.csv("./09_data_merging/4. citrus_practice.csv", encoding = "UTF-8")
head(practice)
View(practice)  
glimpse(practice)

practice$acted_at <- as.Date(practice$acted_at)
practice$month <- month(practice$acted_at)
practice <- practice %>% filter(type != "관수")
library(stringr)
practice$type <- str_replace(practice$type, "적과", "thinning")
practice$type <- str_replace(practice$type, "피복", "mulching")
practice$type <- str_replace(practice$type, "방제", "pest_control")
practice$type <- str_replace(practice$type, "시비", "fertilization")
practice$type <- str_replace(practice$type, "전정", "pruning")

practice$type <- factor(practice$type, levels = c("pruning", "fertilization", "pest_control", "mulching", "thinning"))

practice$day <- day(practice$acted_at)
practice$week <- ceiling((practice$day) / 7)
head(practice)
practice$monthweek <- paste0(practice$month, "-", practice$week)

prac_data = practice %>%  
  arrange(month) %>% 
  group_by(farm_id, type, month) %>% 
  summarise(count = n()
  )

View(prac_data)
write.csv(prac_data, "./09_data_merging/practice_groupby.csv")



##### env_raw_merge.csv
raw <- read.csv("./09_data_merging/raw_data.csv", encoding = "UTF-8")
head(raw)

citrus_env <- read.csv("./09_data_merging/3. citrus_environment2.csv", encoding = "UTF-8")
tail(citrus_env)
glimpse(citrus_env)
citrus_env$sensing_at <- as.Date(citrus_env$sensing_at, format='%Y-%m-%d')
str(citrus_env)

env_raw <- inner_join(citrus_env, raw, 
                      by = c("farm_id"))

env_raw <- na.omit(env_raw)   # 결측치 제거
View(env_raw)
write.csv(env_raw, "./09_data_merging/env_raw_merge.csv")



##### fruit_raw_merge.csv
rm(list=ls())
fruit_position <- read.csv("./09_data_merging/5. citrus_fruit_data_3_replicate.csv", encoding = "UTF-8")
raw <- read.csv("./09_data_merging/raw_data.csv", encoding = "UTF-8")

fruit_raw <- inner_join(fruit_position, raw, 
                      by = c("farm_id"))

fruit_raw <- na.omit(fruit_raw)
View(fruit_raw)
write.csv(fruit_raw, "./09_data_merging/fruit_raw_merge.csv")



##### fruit_raw_grade.csv
rm(list=ls())
fruit <- read.csv("./09_data_merging/fruit_raw_merge.csv", encoding = "UTF-8")
View(fruit)
fruit$week <- paste0(fruit$researched_month, "-", fruit$researched_week)
glimpse(fruit)
head(fruit)
fruit$class <- case_when(
  fruit$size < 49 ~ "too_small",
  fruit$size < 54 & fruit$brix >= 10 ~ "2S",
  fruit$size < 59 & fruit$brix >= 10 ~ "S",
  fruit$size < 63 & fruit$brix >= 10 ~ "M",
  fruit$size < 67 & fruit$brix >= 10 ~ "L",
  fruit$size <= 71 & fruit$brix >= 10 ~ "2L",
  TRUE ~ "too_large"
)
fruit$class <- factor(fruit$class, levels = c("too_small", "2S", "S", "M", "L", "2L", "too_large"))

fruit$grade <- case_when(
  fruit$class %in% c("2S", "L", "2L") ~ "salable",
  fruit$class %in% c("too_small", "too_large") ~ "unsalable",
  fruit$class %in% c("S", "M") ~ "prime grade"
)

fruit$grade <- factor(fruit$grade, levels = c("unsalable", "salable", "prime grade"))
View(fruit)
write.csv(fruit, "./09_data_merging/fruit_raw_grade.csv")