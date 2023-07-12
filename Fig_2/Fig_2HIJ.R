## Fig.2HIJ

# Data merge
library(dplyr)
library(tidyverse)
env <- read.csv("citrus_environment2.csv", header = TRUE)
raw <- read.csv("raw_data.csv", header = TRUE)
temp <- merge(env, raw, by = "farm_id")
temp$monthweek <- paste(temp$month, temp$week, sep = "-")
str(temp)
temp$monthweek <- as.factor(temp$monthweek)

# Extracting data for the second week of November 
temp_11_2 <- temp |> filter(monthweek == "11-2")

# Creating a geodataframe
library(sp)
library(sf)

coordinates(temp_11_2) <- ~longitude + latitude
proj4string(temp_11_2) <- "+proj=longlat +datum=WGS84 +no_defs"
temp_11_2 <- st_as_sf(temp_11_2)

# Map visualization
library(terra)
library(tmap)
jeju <- st_read("jeju.shp")

ggplot() +
  geom_sf(data=jeju) +
  geom_sf(data=temp_11_2, size=2, shape=23, fill="skyblue") +
  theme_bw() +
  ylim(33.1, 33.6)

# Data joining
grid <- st_read("grid_jeju.shp")  # Create 1km grid files of Jeju Island using QGIS
grid <- st_make_valid(grid)
grid <- st_sf(grid)

temp_grid <-st_join(temp_11_2, grid, join = st_intersects)
head(temp_grid)

grid_t <- aggregate(temp_grid$temp, by=list(temp_grid$fid), mean)
grid_h <- aggregate(temp_grid$humi, by=list(temp_grid$fid), mean)
grid_p <- aggregate(temp_grid$press, by=list(temp_grid$fid), mean)

colnames(grid_t) <- c("fid", "temperature")
colnames(grid_h) <- c("fid", "humidity")
colnames(grid_p) <- c("fid", "pressure")
head(grid_t)

temp_high <- merge(grid, grid_t,  by="fid")
humi_high <- merge(grid, grid_h,  by="fid") 
press_high <- merge(grid, grid_p,  by="fid") 

library(ggplot2)
library(dplyr) 
library(tmap)

env_border <-st_join(temp_11_2, jeju, join = st_intersects)
head(env_border)

temp_high <- aggregate(env_border$temp, by=list(env_border$EMD_CD), mean)
humi_high <- aggregate(env_border$humi, by=list(env_border$EMD_CD), mean)
press_high <- aggregate(env_border$press, by=list(env_border$EMD_CD), mean)

colnames(temp_high) <- c("EMD_CD", "temperature")
colnames(humi_high) <- c("EMD_CD", "humidity")
colnames(press_high) <- c("EMD_CD", "pressure")

temp_high2 <- merge(jeju, temp_high, by="EMD_CD")
humi_high2 <- merge(jeju, humi_high,  by="EMD_CD") 
press_high2 <- merge(jeju, press_high,  by="EMD_CD") 

temp2_high_sp <- as(st_geometry(temp_high2), "Spatial")
humi2_high_sp <- as(st_geometry(humi_high2), "Spatial")
press2_high_sp <- as(st_geometry(press_high2), "Spatial")


##### 1. Temperature ##################################################################
x_temp <- coordinates(temp2_high_sp)[,1]
y_temp <- coordinates(temp2_high_sp)[,2] 

l1_acid <- bbox(temp2_high_sp)[1,1] - (bbox(temp2_high_sp)[1,1]*0.001)
l2_acid <- bbox(temp2_high_sp)[1,2] + (bbox(temp2_high_sp)[1,2]*0.001)
l3_acid <- bbox(temp2_high_sp)[2,1] - (bbox(temp2_high_sp)[2,1]*0.001)
l4_acid <- bbox(temp2_high_sp)[2,2] + (bbox(temp2_high_sp)[1,1]*0.001)


library(spatstat)
win_acid <- owin(xrange=c(l1_acid,l2_acid), yrange=c(l3_acid,l4_acid))
plot(win_acid) 

p_acid <- ppp(x_temp, y_temp, window=win_acid)

d_acid <- density.ppp(p_acid, weights=temp_high$temperature, 
                      sigma = bw.diggle(p_acid), 
                      kernel = 'gaussian')  
plot(d_acid)

d_acid[d_acid < quantile(d_acid)[4] + (quantile(d_acid)[4]*0.1)] <- NA

library(raster)
raster_high_acid <- raster(d_acid)
plot(raster_high_acid)
range(d_acid)


bnd <- st_read("jeju.shp") 
raster_high_acid <- crop(raster_high_acid, extent(bnd))
crs(raster_high_acid) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 + towgs84=0,0,0")

plot(raster_high_acid)
plot(bnd, col=NA, border = "grey", add=TRUE)

plot(raster_high_acid)
plot(grid, col=NA, border = "grey", add=TRUE)

bound <- fortify(bnd)
raster_high_acid_transform <- as.data.frame(raster_high_acid, xy = TRUE) |> na.omit()   

ggplot(raster_high_acid_transform) +
  geom_raster(aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c() +
  geom_sf(data=bound, fill = NA) +
  theme_bw() +
  labs(fill = "Temperature") +
  theme(legend.position = c(0.87, 0.23), legend.title = element_text(color="black"),
        legend.key.size = unit(0.4, 'cm')) +
  ylab("") + xlab("")


##### 2. Humidity ##################################################################
x_humi <- coordinates(humi2_high_sp)[,1] 
y_humi <- coordinates(humi2_high_sp)[,2] 

l1_acid <- bbox(humi2_high_sp)[1,1] - (bbox(humi2_high_sp)[1,1]*0.001)
l2_acid <- bbox(humi2_high_sp)[1,2] + (bbox(humi2_high_sp)[1,2]*0.001)
l3_acid <- bbox(humi2_high_sp)[2,1] - (bbox(humi2_high_sp)[2,1]*0.001)
l4_acid <- bbox(humi2_high_sp)[2,2] + (bbox(humi2_high_sp)[1,1]*0.001)

win_acid <- owin(xrange=c(l1_acid,l2_acid), yrange=c(l3_acid,l4_acid)) 
plot(win_acid)      

p_acid <- ppp(x_humi, y_humi, window=win_acid)  

d_acid <- density.ppp(p_acid, weights=humi_high$humidity, 
                      sigma = bw.diggle(p_acid), 
                      kernel = 'gaussian')  
plot(d_acid)   

d_acid[d_acid < quantile(d_acid)[4] + (quantile(d_acid)[4]*0.1)] <- NA  

raster_high_acid <- raster(d_acid) 
plot(raster_high_acid)
range(d_acid)

bnd <- st_read("jeju.shp")    
raster_high_acid <- crop(raster_high_acid, extent(bnd))      
crs(raster_high_acid) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 + towgs84=0,0,0") 

plot(raster_high_acid) 
plot(bnd, col=NA, border = "grey", add=TRUE)

plot(raster_high_acid)
plot(grid, col=NA, border = "grey", add=TRUE)

bound <- fortify(bnd)
raster_high_acid_transform <- as.data.frame(raster_high_acid, xy = TRUE) |> na.omit()   

ggplot(raster_high_acid_transform) +
  geom_raster(aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c() +
  geom_sf(data=bound, fill = NA) +
  theme_bw() +
  labs(fill = "Humidity") +
  theme(legend.position = c(0.9, 0.23), legend.title = element_text(color="black"),
        legend.key.size = unit(0.4, 'cm')) +
  ylab("") + xlab("")


##### 3. Air pressure ##################################################################
x_press <- coordinates(press2_high_sp)[,1] 
y_press <- coordinates(press2_high_sp)[,2] 

l1_acid <- bbox(press2_high_sp)[1,1] - (bbox(press2_high_sp)[1,1]*0.001) 
l2_acid <- bbox(press2_high_sp)[1,2] + (bbox(press2_high_sp)[1,2]*0.001)
l3_acid <- bbox(press2_high_sp)[2,1] - (bbox(press2_high_sp)[2,1]*0.001)
l4_acid <- bbox(press2_high_sp)[2,2] + (bbox(press2_high_sp)[1,1]*0.001)

win_acid <- owin(xrange=c(l1_acid,l2_acid), yrange=c(l3_acid,l4_acid))
plot(win_acid)      

p_acid <- ppp(x_press, y_press, window=win_acid)  

d_acid <- density.ppp(p_acid, weights=press_high$pressure, 
                      sigma = bw.diggle(p_acid), 
                      kernel = 'gaussian')  
plot(d_acid) 

d_acid[d_acid < quantile(d_acid)[4] + (quantile(d_acid)[4]*0.1)] <- NA  

raster_high_acid <- raster(d_acid) 
plot(raster_high_acid)
range(d_acid)

bnd <- st_read("../R_QGIS/Jeju/Jeju_shp/jeju.shp")  
raster_high_acid <- crop(raster_high_acid, extent(bnd))     
crs(raster_high_acid) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 + towgs84=0,0,0")

plot(raster_high_acid) 
plot(bnd, col=NA, border = "grey", add=TRUE)

plot(raster_high_acid)
plot(grid, col=NA, border = "grey", add=TRUE)

bound <- fortify(bnd)
raster_high_acid_transform <- as.data.frame(raster_high_acid, xy = TRUE) |> na.omit()   

ggplot(raster_high_acid_transform) +
  geom_raster(aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c() +
  geom_sf(data=bound, fill = NA) +
  theme_bw() +
  labs(fill = "air pressure") +
  theme(legend.position = c(0.87, 0.23), legend.title = element_text(color="black"),
        legend.key.size = unit(0.4, 'cm')) +
  ylab("") + xlab("")
