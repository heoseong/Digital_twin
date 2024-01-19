rm(list=ls())

load("./06_geodataframe.rdata") 
library(sf)   
ddang <- st_sf(apt_price)
grid <- st_read("./grid_jeju.shp")   
grid <- st_make_valid(grid)
grid <- st_sf(grid)

a <- st_intersects(ddang, grid)
apt_grid <-st_join(ddang, grid, join = st_intersects)  
head(apt_grid)

apt_price <- apt_grid
acid_high <- aggregate(apt_price$acid, by=list(apt_price$fid), mean)

colnames(acid_high) <- c("fid", "acid") 

acid_high <- merge(grid, acid_high,  by="fid")  

library(ggplot2)
library(dplyr) 
library(tmap)

ggplot()+
  geom_sf(data=grid) +
  geom_sf(data=acid_high, aes(fill = acid)) +
  scale_fill_gradient(low = "white", high = "red") + theme_bw()
