## 06_geodataframe

rm(list=ls())
load("../04_preprocess/04_preprocess_Jeju.rdata")
load("../05_geocoding/05_Jeju_geocoding.rdata")
head(toyang)
head(juso_geocoding)


# Merge address and coordinates
library(dplyr)   # install.packages('dplyr')
apt_price <- left_join(toyang, juso_geocoding, 
                       by = c("jibun" = "apt_juso"))
apt_price <- na.omit(apt_price)
View(apt_price)


# Create geodataframe
library(sp)
coordinates(apt_price) <- ~coord_x + coord_y
proj4string(apt_price) <- "+proj=longlat +datum=WGS84 +no_defs"
library(sf)  
apt_price <- st_as_sf(apt_price)    

plot(apt_price$geometry, axes = T, pch = 1)     
library(leaflet)   # install.packages('leaflet')
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data=apt_price[1:1000,], label=~dong) 

library(terra)
library(tmap)
jeju <- st_read("../Jeju_shp/jeju.shp")
#st_transform(terra::crs(jeju))

tm_shape(jeju) +
  tm_borders() +
  tm_shape(apt_price) +
  tm_dots() 

# Save geodataframe
#dir.create("../06_geodataframe")
save(apt_price, file="./06_geodataframe/06_geodataframe.rdata")
write.csv(apt_price, "./06_geodataframe/06_geodataframe.csv")


