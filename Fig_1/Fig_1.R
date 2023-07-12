## Fig.1 map

rm(list=ls())
library(dplyr)
library(tidyverse)
load("./04_preprocess/04_preprocess_Jeju.rdata")    
load("./05_geocoding/05_Jeju_geocoding.rdata")
fruit <- toyang[toyang$type == "4", ]
head(fruit)
write.csv(fruit, "./Fig_1/과수원_토양_data.csv")

fruitj <- left_join(fruit, juso_geocoding, 
                    by = c("jibun" = "apt_juso"))
fruitj <- na.omit(fruitj)   
head(fruitj)

library(sp)  
library(sf)  

coordinates(fruitj) <- ~coord_x + coord_y    
proj4string(fruitj) <- "+proj=longlat +datum=WGS84 +no_defs"
fruitj <- st_as_sf(fruitj)     

library(terra)
library(tmap)
jeju <- st_read("./Jeju_shp/jeju.shp")

ggplot() +
  geom_sf(data=jeju) +
  geom_sf(data=fruitj, size=1, shape=21, fill="grey", stroke = 0.1) +
  theme_bw() +
  ylim(33.1, 33.6)











