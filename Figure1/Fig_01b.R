rm(list=ls())

library(dplyr)
library(tidyverse)
load("./04_preprocess_Jeju.rdata")    
load("./05_Jeju_geocoding.rdata")
fruit <- toyang[toyang$type == "4", ]
head(fruit)
write.csv(fruit, "./Orchard_soil_data.csv")

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
jeju <- st_read("./jeju.shp")
citrus <- read.csv("./citrus_farm_fruit_geodata.csv")
citrus_sf <- st_as_sf(citrus, coords=c("longitude", "latitude"), crs=4326)

ggplot() +
  geom_sf(data=jeju, fill = "white") +
  geom_sf(data=fruitj, size=2, shape=21, fill="grey", stroke = 0.1) +
  geom_sf(data=citrus_sf, size=2.5, shape=21, fill="green", stroke = 1) +
  theme_bw() +
  ylim(33.1, 33.56)
