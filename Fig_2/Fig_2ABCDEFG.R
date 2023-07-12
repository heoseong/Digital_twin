##Fig.2A~G

rm(list=ls())
load("../06_geodataframe/06_geodataframe.rdata")  
library(sf) 
ddang <- st_sf(apt_price)
border <- st_read("../Jeju_shp/jeju.shp")
border <- st_make_valid(border)
border <- st_sf(border)

apt_border <-st_join(ddang, border, join = st_intersects)
head(apt_border)
write.csv(apt_border, "./07_map/soil_border_joined_not_grid.csv")
save(apt_border, file="./07_map/soil_border_joined_not_grid.rdata")
apt_price <- apt_border
acid_high <- aggregate(apt_price$acid, by=list(apt_price$EMD_CD), mean) 
phosphate_high <- aggregate(apt_price$phosphate, by=list(apt_price$EMD_CD), mean)
OM_high <- aggregate(apt_price$OM, by=list(apt_price$EMD_CD), mean)
Mg_high <- aggregate(apt_price$Mg, by=list(apt_price$EMD_CD), mean)
K_high <- aggregate(apt_price$K, by=list(apt_price$EMD_CD), mean)
Ca_high <- aggregate(apt_price$Ca, by=list(apt_price$EMD_CD), mean)
EC_high <- aggregate(apt_price$EC, by=list(apt_price$EMD_CD), mean)

colnames(acid_high) <- c("EMD_CD", "acid") 
colnames(phosphate_high) <- c("EMD_CD", "phosphate")
colnames(OM_high) <- c("EMD_CD", "OM")
colnames(Mg_high) <- c("EMD_CD", "Mg")
colnames(K_high) <- c("EMD_CD", "K")
colnames(Ca_high) <- c("EMD_CD", "Ca")
colnames(EC_high) <- c("EMD_CD", "EC")
head(EC_high, 2)    

acid_high2 <- merge(border, acid_high, by="EMD_CD")
phosphate_high2 <- merge(border, phosphate_high,  by="EMD_CD") 
OM_high2 <- merge(border, OM_high,  by="EMD_CD") 
Mg_high2 <- merge(border, Mg_high,  by="EMD_CD") 
K_high2 <- merge(border, K_high,  by="EMD_CD") 
Ca_high2 <- merge(border, Ca_high,  by="EMD_CD") 
EC_high2 <- merge(border, EC_high,  by="EMD_CD") 

library(ggplot2) 
library(dplyr)   
library(tmap)

####acid_point graph#################
library(gridExtra)
gr1 <- ggplot() +
  theme_bw() +
  geom_sf(data=border) +
  geom_sf(data=apt_border, aes(color = acid)) +
  scale_fill_distiller(name = "acid") +
  ylim(33.1, 33.6) +
  theme(legend.position = c(0.9, 0.23),
        legend.key.size=unit(0.3, 'cm'))

gr2 <- ggplot() +
  theme_bw() +
  geom_sf(data=border) +
  geom_sf(data=apt_border, aes(color = phosphate)) +
  scale_fill_distiller(name = "phosphate") +
  ylim(33.1, 33.6) +
  theme(legend.position = c(0.9, 0.23),
        legend.key.size=unit(0.3, 'cm'))

gr3 <- ggplot() +
  theme_bw() +
  geom_sf(data=border) +
  geom_sf(data=apt_border, aes(color = OM)) +
  scale_fill_distiller(name = "OM") +
  ylim(33.1, 33.6) +
  theme(legend.position = c(0.9, 0.23),
        legend.key.size=unit(0.3, 'cm'))

gr4 <- ggplot() +
  theme_bw() +
  geom_sf(data=border) +
  geom_sf(data=apt_border, aes(color = K)) +
  scale_fill_distiller(name = "K") +
  ylim(33.1, 33.6) +
  theme(legend.position = c(0.9, 0.23),
        legend.key.size=unit(0.3, 'cm'))

gr5 <- ggplot() +
  theme_bw() +
  geom_sf(data=border) +
  geom_sf(data=apt_border, aes(color = Ca)) +
  scale_fill_distiller(name = "Ca") +
  ylim(33.1, 33.6) +
  theme(legend.position = c(0.9, 0.23),
        legend.key.size=unit(0.3, 'cm'))

gr6 <- ggplot() +
  theme_bw() +
  geom_sf(data=border) +
  geom_sf(data=apt_border, aes(color = Mg)) +
  scale_fill_distiller(name = "Mg") +
  ylim(33.1, 33.6) +
  theme(legend.position = c(0.9, 0.23),
        legend.key.size=unit(0.3, 'cm'))

gr7 <- ggplot() +
  theme_bw() +
  geom_sf(data=border) +
  geom_sf(data=apt_border, aes(color = EC)) +
  scale_fill_distiller(name = "EC") +
  ylim(33.1, 33.6) +
  theme(legend.position = c(0.9, 0.23),
        legend.key.size=unit(0.3, 'cm'))

grid.arrange(gr1, gr2, gr3, gr4, gr5, gr6, gr7, nrow=4, ncol=2)

#####################################
library(sp) 
acid2_high_sp <- as(st_geometry(acid_high2), "Spatial")    
phosphate2_high_sp <- as(st_geometry(phosphate_high2), "Spatial")
OM2_high_sp <- as(st_geometry(OM_high2), "Spatial")
K2_high_sp <- as(st_geometry(K_high2), "Spatial")
Ca2_high_sp <- as(st_geometry(Ca_high2), "Spatial")
Mg2_high_sp <- as(st_geometry(Mg_high2), "Spatial")
EC2_high_sp <- as(st_geometry(EC_high2), "Spatial")


##### 1. acid ##################################################################
x_acid <- coordinates(acid2_high_sp)[,1]  
y_acid <- coordinates(acid2_high_sp)[,2] 

l1_acid <- bbox(acid2_high_sp)[1,1] - (bbox(acid2_high_sp)[1,1]*0.001) 
l2_acid <- bbox(acid2_high_sp)[1,2] + (bbox(acid2_high_sp)[1,2]*0.001)
l3_acid <- bbox(acid2_high_sp)[2,1] - (bbox(acid2_high_sp)[2,1]*0.001)
l4_acid <- bbox(acid2_high_sp)[2,2] + (bbox(acid2_high_sp)[1,1]*0.001)


library(spatstat)  
win_acid <- owin(xrange=c(l1_acid,l2_acid), yrange=c(l3_acid,l4_acid)) 
plot(win_acid)         

p_acid <- ppp(x_acid, y_acid, window=win_acid)  

d_acid <- density.ppp(p_acid, weights=acid_high$acid,
                      sigma = bw.diggle(p_acid), 
                      kernel = 'gaussian')  
plot(d_acid) 

d_acid[d_acid < quantile(d_acid)[4] + (quantile(d_acid)[4]*0.1)] <- NA  
library(raster)    
raster_high_acid <- raster(d_acid) 
plot(raster_high_acid)
range(d_acid)



bnd <- st_read("../Jeju_shp/jeju.shp")   
raster_high_acid <- crop(raster_high_acid, extent(bnd))     
crs(raster_high_acid) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 + towgs84=0,0,0") 

plot(raster_high_acid)  
plot(bnd, col=NA, border = "grey", add=TRUE)


plot(raster_high_acid)
plot(grid, col=NA, border = "grey", add=TRUE)


bound <- fortify(bnd)
raster_high_acid_transform <- as.data.frame(raster_high_acid, xy = TRUE) |> na.omit()   

gq1 <- ggplot(raster_high_acid_transform) +
  geom_raster(aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c() +
  geom_sf(data=bound, fill = NA) +
  theme_bw() +
  labs(fill = "Acidity") +
  theme(legend.position = c(0.9, 0.23), legend.title = element_text(color="black"),
        legend.key.size = unit(0.4, 'cm')
  ) +
  ylab("") + xlab("")

####################################################

##### 2. phosphate ##################################################################
x_phosphate <- coordinates(phosphate2_high_sp)[,1] 
y_phosphate <- coordinates(phosphate2_high_sp)[,2] 

l1_phosphate <- bbox(phosphate2_high_sp)[1,1] - (bbox(phosphate2_high_sp)[1,1]*0.001) 
l2_phosphate <- bbox(phosphate2_high_sp)[1,2] + (bbox(phosphate2_high_sp)[1,2]*0.001)
l3_phosphate <- bbox(phosphate2_high_sp)[2,1] - (bbox(phosphate2_high_sp)[2,1]*0.001)
l4_phosphate <- bbox(phosphate2_high_sp)[2,2] + (bbox(phosphate2_high_sp)[1,1]*0.001)


library(spatstat)  
win_phosphate <- owin(xrange=c(l1_phosphate,l2_phosphate), yrange=c(l3_phosphate,l4_phosphate))
plot(win_phosphate)        

p_phosphate <- ppp(x_phosphate, y_phosphate, window=win_phosphate) 

d_phosphate <- density.ppp(p_phosphate, weights=phosphate_high$phosphate,
                           sigma = bw.diggle(p_phosphate), 
                           kernel = 'gaussian')  
plot(d_phosphate)  

d_phosphate[d_phosphate < quantile(d_phosphate)[4] + (quantile(d_phosphate)[4]*0.1)] <- NA   
library(raster)     
raster_high_phosphate <- raster(d_phosphate)  
plot(raster_high_phosphate)
range(d_phosphate)


bnd <- st_read("../Jeju_shp/jeju.shp")   
raster_high_phosphate <- crop(raster_high_phosphate, extent(bnd))    
crs(raster_high_phosphate) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 + towgs84=0,0,0") 

plot(raster_high_phosphate) 
plot(bnd, col=NA, border = "grey", add=TRUE)


bound <- fortify(bnd)
raster_high_phosphate_transform <- as.data.frame(raster_high_phosphate, xy = TRUE) |> na.omit()   

gq2 <- ggplot(raster_high_phosphate_transform) +
  geom_raster(aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c() +
  geom_sf(data=bound, fill = NA) +
  theme_bw() +
  labs(fill = "Available phosphate") +
  theme(legend.position = c(0.9, 0.23), legend.title = element_text(color="black"),
        legend.key.size = unit(0.4, 'cm')) +
  ylab("") + xlab("")

####################################################


##### 3. OM ##################################################################
x_OM <- coordinates(OM2_high_sp)[,1] 
y_OM <- coordinates(OM2_high_sp)[,2] 

l1_OM <- bbox(OM2_high_sp)[1,1] - (bbox(OM2_high_sp)[1,1]*0.001) 
l2_OM <- bbox(OM2_high_sp)[1,2] + (bbox(OM2_high_sp)[1,2]*0.001)
l3_OM <- bbox(OM2_high_sp)[2,1] - (bbox(OM2_high_sp)[2,1]*0.001)
l4_OM <- bbox(OM2_high_sp)[2,2] + (bbox(OM2_high_sp)[1,1]*0.001)


win_OM <- owin(xrange=c(l1_OM,l2_OM), yrange=c(l3_OM,l4_OM)) 
plot(win_OM)         

p_OM <- ppp(x_OM, y_OM, window=win_OM) 

d_OM <- density.ppp(p_OM, weights=OM_high$OM, 
                    sigma = bw.diggle(p_OM), 
                    kernel = 'gaussian')  
plot(d_OM) 

d_OM[d_OM < quantile(d_OM)[4] + (quantile(d_OM)[4]*0.1)] <- NA  

raster_high_OM <- raster(d_OM)  
plot(raster_high_OM)
range(d_OM)



bnd <- st_read("../Jeju_shp/jeju.shp")   
raster_high_OM <- crop(raster_high_OM, extent(bnd))      
crs(raster_high_OM) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 + towgs84=0,0,0") 

plot(raster_high_OM) 
plot(bnd, col=NA, border = "grey", add=TRUE)


bound <- fortify(bnd)
raster_high_OM_transform <- as.data.frame(raster_high_OM, xy = TRUE) |> na.omit()   

gq3 <- ggplot(raster_high_OM_transform) +
  geom_raster(aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c() +
  geom_sf(data=bound, fill = NA) +
  theme_bw()+
  labs(fill = "OM") +
  theme(legend.position = c(0.9, 0.23), legend.title = element_text(color="black"),
        legend.key.size = unit(0.4, 'cm')) +
  ylab("") + xlab("")
####################################################

##### 4. K ##################################################################
x_K <- coordinates(K2_high_sp)[,1] 
y_K <- coordinates(K2_high_sp)[,2] 

l1_K <- bbox(K2_high_sp)[1,1] - (bbox(K2_high_sp)[1,1]*0.001) 
l2_K <- bbox(K2_high_sp)[1,2] + (bbox(K2_high_sp)[1,2]*0.001)
l3_K <- bbox(K2_high_sp)[2,1] - (bbox(K2_high_sp)[2,1]*0.001)
l4_K <- bbox(K2_high_sp)[2,2] + (bbox(K2_high_sp)[1,1]*0.001)

win_K <- owin(xrange=c(l1_K,l2_K), yrange=c(l3_K,l4_K)) 
plot(win_K)         

p_K <- ppp(x_K, y_K, window=win_K)  

d_K <- density.ppp(p_K, weights=K_high$K, 
                   sigma = bw.diggle(p_K), 
                   kernel = 'gaussian')  
plot(d_K)   

d_K[d_K < quantile(d_K)[4] + (quantile(d_K)[4]*0.1)] <- NA   

raster_high_K <- raster(d_K)  
plot(raster_high_K)
range(d_K)


bnd <- st_read("../Jeju_shp/jeju.shp")    
raster_high_K <- crop(raster_high_K, extent(bnd))     
crs(raster_high_K) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 + towgs84=0,0,0") 

plot(raster_high_K) 
plot(bnd, col=NA, border = "grey", add=TRUE)


bound <- fortify(bnd)
raster_high_K_transform <- as.data.frame(raster_high_K, xy = TRUE) |> na.omit()   

gq4 <- ggplot(raster_high_K_transform) +
  geom_raster(aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c() +
  geom_sf(data=bound, fill = NA) +
  theme_bw()+
  labs(fill = "K") +
  theme(legend.position = c(0.9, 0.23), legend.title = element_text(color="black"),
        legend.key.size = unit(0.4, 'cm')) +
  ylab("") + xlab("")
####################################################

##### 5. Ca ##################################################################
x_Ca <- coordinates(Ca2_high_sp)[,1] 
y_Ca <- coordinates(Ca2_high_sp)[,2] 

l1_Ca <- bbox(Ca2_high_sp)[1,1] - (bbox(Ca2_high_sp)[1,1]*0.001) 
l2_Ca <- bbox(Ca2_high_sp)[1,2] + (bbox(Ca2_high_sp)[1,2]*0.001)
l3_Ca <- bbox(Ca2_high_sp)[2,1] - (bbox(Ca2_high_sp)[2,1]*0.001)
l4_Ca <- bbox(Ca2_high_sp)[2,2] + (bbox(Ca2_high_sp)[1,1]*0.001)

win_Ca <- owin(xrange=c(l1_Ca,l2_Ca), yrange=c(l3_Ca,l4_Ca))
plot(win_Ca)     

p_Ca <- ppp(x_Ca, y_Ca, window=win_Ca)  

d_Ca <- density.ppp(p_Ca, weights=Ca_high$Ca,
                    sigma = bw.diggle(p_Ca), 
                    kernel = 'gaussian')  
plot(d_Ca)  

d_Ca[d_Ca < quantile(d_Ca)[4] + (quantile(d_Ca)[4]*0.1)] <- NA  

raster_high_Ca <- raster(d_Ca)  
plot(raster_high_Ca)
range(d_Ca)


bnd <- st_read("../Jeju_shp/jeju.shp")    
raster_high_Ca <- crop(raster_high_Ca, extent(bnd))     
crs(raster_high_Ca) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 + towgs84=0,0,0")

plot(raster_high_Ca)  
plot(bnd, col=NA, border = "grey", add=TRUE)


bound <- fortify(bnd)
raster_high_Ca_transform <- as.data.frame(raster_high_Ca, xy = TRUE) |> na.omit()   

gq5 <- ggplot(raster_high_Ca_transform) +
  geom_raster(aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c() +
  geom_sf(data=bound, fill = NA) +
  theme_bw()+
  labs(fill = "Ca") +
  theme(legend.position = c(0.9, 0.23), legend.title = element_text(color="black"),
        legend.key.size = unit(0.4, 'cm')) +
  ylab("") + xlab("")
####################################################

##### 6. Mg ##################################################################
x_Mg <- coordinates(Mg2_high_sp)[,1] 
y_Mg <- coordinates(Mg2_high_sp)[,2] 

l1_Mg <- bbox(Mg2_high_sp)[1,1] - (bbox(Mg2_high_sp)[1,1]*0.001) 
l2_Mg <- bbox(Mg2_high_sp)[1,2] + (bbox(Mg2_high_sp)[1,2]*0.001)
l3_Mg <- bbox(Mg2_high_sp)[2,1] - (bbox(Mg2_high_sp)[2,1]*0.001)
l4_Mg <- bbox(Mg2_high_sp)[2,2] + (bbox(Mg2_high_sp)[1,1]*0.001)


win_Mg <- owin(xrange=c(l1_Mg,l2_Mg), yrange=c(l3_Mg,l4_Mg)) 
plot(win_Mg)         

p_Mg <- ppp(x_Mg, y_Mg, window=win_Mg)  

d_Mg <- density.ppp(p_Mg, weights=Mg_high$Mg, 
                    sigma = bw.diggle(p_Mg), 
                    kernel = 'gaussian')  
plot(d_Mg) 

d_Mg[d_Mg < quantile(d_Mg)[4] + (quantile(d_Mg)[4]*0.1)] <- NA   

raster_high_Mg <- raster(d_Mg)  
plot(raster_high_Mg)
range(d_Mg)


bnd <- st_read("../Jeju_shp/jeju.shp")   
raster_high_Mg <- crop(raster_high_Mg, extent(bnd))      
crs(raster_high_Mg) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 + towgs84=0,0,0") 

plot(raster_high_Mg)  
plot(bnd, col=NA, border = "grey", add=TRUE)


bound <- fortify(bnd)
raster_high_Mg_transform <- as.data.frame(raster_high_Mg, xy = TRUE) |> na.omit()   

gq6 <- ggplot(raster_high_Mg_transform) +
  geom_raster(aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c() +
  geom_sf(data=bound, fill = NA) +
  theme_bw()+
  labs(fill = "Mg") +
  theme(legend.position = c(0.9, 0.23), legend.title = element_text(color="black"),
        legend.key.size = unit(0.4, 'cm')) +
  ylab("") + xlab("")
####################################################

##### 7. EC ##################################################################
x_EC <- coordinates(EC2_high_sp)[,1]  
y_EC <- coordinates(EC2_high_sp)[,2] 

l1_EC <- bbox(EC2_high_sp)[1,1] - (bbox(EC2_high_sp)[1,1]*0.001) 
l2_EC <- bbox(EC2_high_sp)[1,2] + (bbox(EC2_high_sp)[1,2]*0.001)
l3_EC <- bbox(EC2_high_sp)[2,1] - (bbox(EC2_high_sp)[2,1]*0.001)
l4_EC <- bbox(EC2_high_sp)[2,2] + (bbox(EC2_high_sp)[1,1]*0.001)


win_EC <- owin(xrange=c(l1_EC,l2_EC), yrange=c(l3_EC,l4_EC)) 
plot(win_EC)       

p_EC <- ppp(x_EC, y_EC, window=win_EC)  

d_EC <- density.ppp(p_EC, weights=EC_high$EC, 
                    sigma = bw.diggle(p_EC), 
                    kernel = 'gaussian')  
plot(d_EC)  

d_EC[d_EC < quantile(d_EC)[4] + (quantile(d_EC)[4]*0.1)] <- NA  
raster_high_EC <- raster(d_EC)  
plot(raster_high_EC)
range(d_EC)

bnd <- st_read("../Jeju_shp/jeju.shp")   
raster_high_EC <- crop(raster_high_EC, extent(bnd))     
crs(raster_high_EC) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 + towgs84=0,0,0") 

plot(raster_high_EC) 
plot(bnd, col=NA, border = "grey", add=TRUE)


bound <- fortify(bnd)
raster_high_EC_transform <- as.data.frame(raster_high_EC, xy = TRUE) |> na.omit()   

gq7 <- ggplot(raster_high_EC_transform) +
  geom_raster(aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c() +
  geom_sf(data=bound, fill = NA) +
  theme_bw()+
  labs(fill = "EC") +
  theme(legend.position = c(0.9, 0.23), legend.title = element_text(color="black"),
        legend.key.size = unit(0.4, 'cm')) +
  ylab("") + xlab("")
####################################################

library(gridExtra)
grid.arrange(gq1, gq2, gq3, gq4, gq5, gq6, gq7, nrow=7, ncol=1)


#dir.create("07_map") 
save(raster_high_acid, file="./07_map/07_kde_high_acid.rdata")
save(raster_high_phosphate, file="./07_map/07_kde_high_phosphate.rdata") 
save(raster_high_OM, file="./07_map/07_kde_high_OM.rdata") 
save(raster_high_K, file="./07_map/07_kde_high_K.rdata") 
save(raster_high_Ca, file="./07_map/07_kde_high_Ca.rdata") 
save(raster_high_Mg, file="./07_map/07_kde_high_Mg.rdata") 
save(raster_high_EC, file="./07_map/07_kde_high_EC.rdata") 
############################################################################### 

