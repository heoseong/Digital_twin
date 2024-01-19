rm( list = ls() )

############
### DATA ###
############

data0 = read.csv( "citrus_farm_fruit_geodata.csv" )
data0$monthweek = paste0( data0$month, "-", data0$week )

env = read.csv( "env_raw_merge.csv" )
env$monthweek = paste0( env$month, "-", env$week )

citrus = read.csv( "citrus_fruit_data_3_replicate.csv" )
citrus$monthweek = paste0( citrus$researched_month, "-", citrus$researched_week )

soil0 = read.csv( "06_geodataframe.csv" )
soil0$julian = julian( as.Date( soil0$ymd, format = "%m/%d/%Y" ), origin = as.Date( "2000-01-01" ) )

prac = read.csv( "practice_groupby.csv" )

##################
### DATA MERGE ###
##################

env$id = paste0( env$farm_id, "-", env$monthweek )
temp_ave = by( env$temp, env$id, mean, na.rm = TRUE )
humi_ave = by( env$humi, env$id, mean, na.rm = TRUE )
press_ave = by( env$press, env$id, mean, na.rm = TRUE )
length(temp_ave); length(humi_ave); length(press_ave)
data.env = data.frame( id = names(temp_ave), temp_ave, humi_ave, press_ave )
rownames(data.env) = NULL
data.env$farm_id = substr( data.env$id, 1, 3 )
data.env$monthweek = substr( data.env$id, 5, 8 )
data.env = data.env[ , c( 5, 6, 2, 3, 4 ) ]
head(data.env)

citrus$tree_id = paste0( citrus$farm_id, "-", citrus$tag_no )
citrus$citrus_id = paste0( citrus$farm_id, "-", citrus$tag_no, "-", citrus$citrus_id )

data.fruit = citrus[ , c( "farm_id", "tree_id", "citrus_id", "monthweek", "researched_at", "position", "brix", "size" ) ]
data.fruit = merge( data.fruit, data.env, by = c( "farm_id", "monthweek" ), all = TRUE )

temp1 = data0$research_day
temp2 = as.Date( paste0( substr(temp1, 1, 4), "-", substr(temp1, 6, 7), "-", substr(temp1, 9, 10) ) )
data0$julian = julian( temp2, origin = as.Date( "2000-01-01" ) )

u1 = unique(data0$farm_id)
u2 = unique(data0$monthweek)
data.new = data.frame()

for (i in 1:length(u1) ) {
  for (j in 1:length(u2) ) {
    
    data.temp = subset( data0, farm_id == u1[i] & monthweek == u2[j] )
    j.temp = data.temp$julian[1]
    long.temp = data.temp$longitude[1]
    lati.temp = data.temp$latitude[1]
    
    soil.temp = subset( soil0, julian <= j.temp )
    soil.temp = soil.temp[ order(soil.temp$julian, decreasing = TRUE), ]
    soil.temp = subset( soil.temp, julian %in% unique(soil.temp$julian)[1:5] )
    
    dist.temp = sqrt( ( soil.temp$longitude - long.temp ) ^ 2 + ( soil.temp$latitude - lati.temp ) ^ 2 )
    
    soil.temp2 = soil.temp[ order(dist.temp, decreasing = FALSE), ]
    soil.temp2 = soil.temp2[1:10,]
    
    data.temp2 = data.frame( farm_id = u1[i], monthweek = u2[j],
                             acid = mean( soil.temp2$acid, na.rm = T ),
                             phosphate = mean( soil.temp2$phosphate, na.rm = T ),
                             OM = mean( soil.temp2$OM, na.rm = T ),
                             Mg = mean( soil.temp2$Mg, na.rm = T ),
                             K = mean( soil.temp2$K, na.rm = T ),
                             Ca = mean( soil.temp2$Ca, na.rm = T ),
                             EC = mean( soil.temp2$EC, na.rm = T ) )
    
    data.new = rbind( data.new, data.temp2 )
    
  }
}

colnames(data.new) = c( "farm_id", "monthweek", colnames(soil0)[7:13] )
data.new2 = merge( data.new, data0, by = c( "farm_id", "monthweek" ), all = TRUE )
data.new3 = data.new2[ , c( "farm_id", "monthweek", "acid", "phosphate", "OM", "Mg", "K", "Ca", "EC", "latitude", "longitude", "area", "location" ) ]
data.new3$id = paste0( data.new3$farm_id, "-", data.new3$monthweek )
data.new4 = data.new3[ duplicated(data.new3$id) == FALSE, ]
data.new4 = data.new4[,-dim(data.new4)[2]]
data.new5 = merge( data.fruit, data.new4, by = c( "farm_id", "monthweek" ), all = TRUE )

data.new5$temp2 = cut( data.new5$temp_ave, breaks = c( 0, 10.4, 11.5, 12.6, Inf ) )
data.new5$humi2 = cut( data.new5$humi_ave, breaks = c( 0, 52.6, 56.0, 61.3, Inf ) )
data.new5$press2 = cut( data.new5$press_ave, breaks = c( 0, 3.3, 5.1, 6.6, Inf ) )
data.new5$acid2 = cut( data.new5$acid, breaks = c( 0, summary(data.new5$acid)[c(2,3,5)], Inf ) )
data.new5$phosphate2 = cut( data.new5$phosphate, breaks = c( 0, summary(data.new5$phosphate)[c(2,3,5)], Inf ) )
data.new5$OM2 = cut( data.new5$OM, breaks = c( 0, summary(data.new5$OM)[c(2,3,5)], Inf ) )
data.new5$Mg2 = cut( data.new5$Mg, breaks = c( 0, summary(data.new5$Mg)[c(2,3,5)], Inf ) )
data.new5$K2 = cut( data.new5$K, breaks = c( 0, summary(data.new5$K)[c(2,3,5)], Inf ) )
data.new5$Ca2 = cut( data.new5$Ca, breaks = c( 0, summary(data.new5$Ca)[c(2,3,5)], Inf ) )
data.new5$EC2 = cut( data.new5$EC, breaks = c( 0, summary(data.new5$EC)[c(2,3,5)], Inf ) )

u.type = sort( unique(prac$type) )
u.farm = sort( unique(prac$farm_id) )

prac2 = matrix( NA, nrow = length(u.farm), ncol = length(u.type) )

for (i in 1:length(u.farm) ) {
  for (j in 1:length(u.type) ) {
    temp = subset( prac, farm_id == u.farm[i] & type == u.type[j] )
    prac2[i,j] = sum(temp$count)
  }
}

prac2 = data.frame(prac2)
colnames(prac2) = u.type
prac2$farm_id = u.farm

data.new6 = merge( data.new5, prac2, by = "farm_id", all = TRUE )
data.new6$management_total = data.new6$fertilization + data.new6$mulching + data.new6$pest_control + data.new6$pruning + data.new6$thinning

data.new7 = subset( data.new6, monthweek %in% c( "10-3", "10-4", "10-5", "11-1", "11-2", "11-3", "11-4" ) )
#save( data.new7, file = "data.RData" )

################
### FIGURE 4 ###
################

data.temp = data.new7

mu1 = mean( data.temp$brix, na.rm = TRUE ); sigma1 = sd( data.temp$brix, na.rm = TRUE )
mu2 = mean( data.temp$size, na.rm = TRUE ); sigma2 = sd( data.temp$size, na.rm = TRUE )

###
###### ACID
###

jpeg( "Figures/Fig4A.jpg", width = 500, height = 400, quality = 100 )

par( mfrow = c(1,1) )
par( mar = c(5.1, 4.1, 4.1, 2.1*2) ) 

x = data.temp$acid
brix = data.temp$brix; brix = as.numeric( scale(brix) )
size = data.temp$size; size = as.numeric( scale(size) )
index = complete.cases(x) & complete.cases(brix) & complete.cases(size)
x = x[index]; brix = brix[index]; size = size[index]

plot( 1, 1, col = "white", axes = FALSE,
      xlim = range(x), ylim = c(-1,1), 
      xlab = "", ylab = "" )
lines( smooth.spline( x, brix, df = 3 ), col = 2, lwd = 2, lty = 1 )
lines( smooth.spline( x, size, df = 3 ), col = 4, lwd = 2, lty = 2 )
axis( 1 ); axis( 1, c(-100,99999) )
axis( 2, seq( -5, 5, 0.5 ), las = 2, 
      labels = formatC( mu1 + seq( -5, 5, 0.5 ) * sigma1, format = "f", digit = 1 ) )
axis( 4, seq( -5, 5, 0.5 ), las = 2,
      labels = formatC( mu2 + seq( -5, 5, 0.5 ) * sigma2, format = "f", digit = 1 ) )
axis( 3, c( -100, 1000 ) )
mtext( side = 1, line = 3, text = "pH" )
mtext( side = 2, line = 3, text = "Sugar Content (°Bx)" )
mtext( side = 4, line = 3, text = "Fruit Size (mm)" )
legend( "topleft", col = c( 2, 4 ), lwd = c( 1.5, 1.5 ), lty = c( 1, 2 ),
        legend = c("Sugar content", "Fruit size"), bty = "n" )

dev.off()

###
###### PHOSPHATE
###

jpeg( "Figures/Fig4B.jpg", width = 500, height = 400, quality = 100 )

par( mfrow = c(1,1) )
par( mar = c(5.1, 4.1, 4.1, 2.1*2) ) 

x = data.temp$phosphate
brix = data.temp$brix; brix = as.numeric( scale(brix) )
size = data.temp$size; size = as.numeric( scale(size) )
index = complete.cases(x) & complete.cases(brix) & complete.cases(size)
x = x[index]; brix = brix[index]; size = size[index]

plot( 1, 1, col = "white", axes = FALSE,
      xlim = range(x), ylim = c(-1,1), 
      xlab = "", ylab = "" )
lines( smooth.spline( x, brix, df = 3 ), col = 2, lwd = 2, lty = 1 )
lines( smooth.spline( x, size, df = 3 ), col = 4, lwd = 2, lty = 2 )
axis( 1 ); axis( 1, c(-100,99999) )
axis( 2, seq( -5, 5, 0.5 ), las = 2, 
      labels = formatC( mu1 + seq( -5, 5, 0.5 ) * sigma1, format = "f", digit = 1 ) )
axis( 4, seq( -5, 5, 0.5 ), las = 2,
      labels = formatC( mu2 + seq( -5, 5, 0.5 ) * sigma2, format = "f", digit = 1 ) )
axis( 3, c( -100, 1000 ) )
mtext( side = 1, line = 3, text = expression('Av. P'[2]*'O'[4]) )
mtext( side = 2, line = 3, text = "Sugar Content (°Bx)" )
mtext( side = 4, line = 3, text = "Fruit Size (mm)" )
legend( "topleft", col = c( 2, 4 ), lwd = c( 1.5, 1.5 ), lty = c( 1, 2 ),
        legend = c("Sugar content", "Fruit size"), bty = "n" )

dev.off()

###
###### ORGANIC MATTER
###

jpeg( "Figures/Fig4C.jpg", width = 500, height = 400, quality = 100 )

par( mfrow = c(1,1) )
par( mar = c(5.1, 4.1, 4.1, 2.1*2) ) 

x = data.temp$OM
brix = data.temp$brix; brix = as.numeric( scale(brix) )
size = data.temp$size; size = as.numeric( scale(size) )
index = complete.cases(x) & complete.cases(brix) & complete.cases(size)
x = x[index]; brix = brix[index]; size = size[index]

plot( 1, 1, col = "white", axes = FALSE,
      xlim = range(x), ylim = c(-1,1), 
      xlab = "", ylab = "" )
lines( smooth.spline( x, brix, df = 3 ), col = 2, lwd = 2, lty = 1 )
lines( smooth.spline( x, size, df = 3 ), col = 4, lwd = 2, lty = 2 )
axis( 1 ); axis( 1, c(-100,99999) )
axis( 2, seq( -5, 5, 0.5 ), las = 2, 
      labels = formatC( mu1 + seq( -5, 5, 0.5 ) * sigma1, format = "f", digit = 1 ) )
axis( 4, seq( -5, 5, 0.5 ), las = 2,
      labels = formatC( mu2 + seq( -5, 5, 0.5 ) * sigma2, format = "f", digit = 1 ) )
axis( 3, c( -100, 1000 ) )
mtext( side = 1, line = 3, text = "OM" )
mtext( side = 2, line = 3, text = "Sugar Content (°Bx)" )
mtext( side = 4, line = 3, text = "Fruit Size (mm)" )
legend( "topleft", col = c( 2, 4 ), lwd = c( 1.5, 1.5 ), lty = c( 1, 2 ),
        legend = c("Sugar content", "Fruit size"), bty = "n" )

dev.off()

###
###### MAGNESIUM
###

jpeg( "Figures/Fig4D.jpg", width = 500, height = 400, quality = 100 )

par( mfrow = c(1,1) )
par( mar = c(5.1, 4.1, 4.1, 2.1*2) ) 

x = data.temp$Mg
brix = data.temp$brix; brix = as.numeric( scale(brix) )
size = data.temp$size; size = as.numeric( scale(size) )
index = complete.cases(x) & complete.cases(brix) & complete.cases(size)
x = x[index]; brix = brix[index]; size = size[index]

plot( 1, 1, col = "white", axes = FALSE,
      xlim = range(x), ylim = c(-1,1), 
      xlab = "", ylab = "" )
lines( smooth.spline( x, brix, df = 3 ), col = 2, lwd = 2, lty = 1 )
lines( smooth.spline( x, size, df = 3 ), col = 4, lwd = 2, lty = 2 )
axis( 1 ); axis( 1, c(-100,99999) )
axis( 2, seq( -5, 5, 0.5 ), las = 2, 
      labels = formatC( mu1 + seq( -5, 5, 0.5 ) * sigma1, format = "f", digit = 1 ) )
axis( 4, seq( -5, 5, 0.5 ), las = 2,
      labels = formatC( mu2 + seq( -5, 5, 0.5 ) * sigma2, format = "f", digit = 1 ) )
axis( 3, c( -100, 1000 ) )
mtext( side = 1, line = 3, text = "Exch. Mg" )
mtext( side = 2, line = 3, text = "Sugar Content (°Bx)" )
mtext( side = 4, line = 3, text = "Fruit Size (mm)" )
legend( "topleft", col = c( 2, 4 ), lwd = c( 1.5, 1.5 ), lty = c( 1, 2 ),
        legend = c("Sugar content", "Fruit size"), bty = "n" )

dev.off()

###
###### POTASSIUM
###

jpeg( "Figures/Fig4E.jpg", width = 500, height = 400, quality = 100 )

par( mfrow = c(1,1) )
par( mar = c(5.1, 4.1, 4.1, 2.1*2) ) 

x = data.temp$K
brix = data.temp$brix; brix = as.numeric( scale(brix) )
size = data.temp$size; size = as.numeric( scale(size) )
index = complete.cases(x) & complete.cases(brix) & complete.cases(size)
x = x[index]; brix = brix[index]; size = size[index]

plot( 1, 1, col = "white", axes = FALSE,
      xlim = range(x), ylim = c(-1,1), 
      xlab = "", ylab = "" )
lines( smooth.spline( x, brix, df = 3 ), col = 2, lwd = 2, lty = 1 )
lines( smooth.spline( x, size, df = 3 ), col = 4, lwd = 2, lty = 2 )
axis( 1 ); axis( 1, c(-100,99999) )
axis( 2, seq( -5, 5, 0.5 ), las = 2, 
      labels = formatC( mu1 + seq( -5, 5, 0.5 ) * sigma1, format = "f", digit = 1 ) )
axis( 4, seq( -5, 5, 0.5 ), las = 2,
      labels = formatC( mu2 + seq( -5, 5, 0.5 ) * sigma2, format = "f", digit = 1 ) )
axis( 3, c( -100, 1000 ) )
mtext( side = 1, line = 3, text = "Exch. K" )
mtext( side = 2, line = 3, text = "Sugar Content (°Bx)" )
mtext( side = 4, line = 3, text = "Fruit Size (mm)" )
legend( "topleft", col = c( 2, 4 ), lwd = c( 1.5, 1.5 ), lty = c( 1, 2 ),
        legend = c("Sugar content", "Fruit size"), bty = "n" )

dev.off()

###
###### CALCIUM
###

jpeg( "Figures/Fig4F.jpg", width = 500, height = 400, quality = 100 )

par( mfrow = c(1,1) )
par( mar = c(5.1, 4.1, 4.1, 2.1*2) ) 

x = data.temp$Ca
brix = data.temp$brix; brix = as.numeric( scale(brix) )
size = data.temp$size; size = as.numeric( scale(size) )
index = complete.cases(x) & complete.cases(brix) & complete.cases(size)
x = x[index]; brix = brix[index]; size = size[index]

plot( 1, 1, col = "white", axes = FALSE,
      xlim = range(x), ylim = c(-1,1), 
      xlab = "", ylab = "" )
lines( smooth.spline( x, brix, df = 3 ), col = 2, lwd = 2, lty = 1 )
lines( smooth.spline( x, size, df = 3 ), col = 4, lwd = 2, lty = 2 )
axis( 1 ); axis( 1, c(-100,99999) )
axis( 2, seq( -5, 5, 0.5 ), las = 2, 
      labels = formatC( mu1 + seq( -5, 5, 0.5 ) * sigma1, format = "f", digit = 1 ) )
axis( 4, seq( -5, 5, 0.5 ), las = 2,
      labels = formatC( mu2 + seq( -5, 5, 0.5 ) * sigma2, format = "f", digit = 1 ) )
axis( 3, c( -100, 1000 ) )
mtext( side = 1, line = 3, text = "Exch. Ca" )
mtext( side = 2, line = 3, text = "Sugar Content (°Bx)" )
mtext( side = 4, line = 3, text = "Fruit Size (mm)" )
legend( "topleft", col = c( 2, 4 ), lwd = c( 1.5, 1.5 ), lty = c( 1, 2 ),
        legend = c("Sugar content", "Fruit size"), bty = "n" )

dev.off()

###
###### ELECTRICAL CONDUCTIVITY
###

jpeg( "Figures/Fig4G.jpg", width = 500, height = 400, quality = 100 )

par( mfrow = c(1,1) )
par( mar = c(5.1, 4.1, 4.1, 2.1*2) ) 

x = data.temp$EC
brix = data.temp$brix; brix = as.numeric( scale(brix) )
size = data.temp$size; size = as.numeric( scale(size) )
index = complete.cases(x) & complete.cases(brix) & complete.cases(size)
x = x[index]; brix = brix[index]; size = size[index]

plot( 1, 1, col = "white", axes = FALSE,
      xlim = range(x), ylim = c(-1,1), 
      xlab = "", ylab = "" )
lines( smooth.spline( x, brix, df = 3 ), col = 2, lwd = 2, lty = 1 )
lines( smooth.spline( x, size, df = 3 ), col = 4, lwd = 2, lty = 2 )
axis( 1 ); axis( 1, c(-100,99999) )
axis( 2, seq( -5, 5, 0.5 ), las = 2, 
      labels = formatC( mu1 + seq( -5, 5, 0.5 ) * sigma1, format = "f", digit = 1 ) )
axis( 4, seq( -5, 5, 0.5 ), las = 2,
      labels = formatC( mu2 + seq( -5, 5, 0.5 ) * sigma2, format = "f", digit = 1 ) )
axis( 3, c( -100, 1000 ) )
mtext( side = 1, line = 3, text = "EC" )
mtext( side = 2, line = 3, text = "Sugar Content (°Bx)" )
mtext( side = 4, line = 3, text = "Fruit Size (mm)" )
legend( "topleft", col = c( 2, 4 ), lwd = c( 1.5, 1.5 ), lty = c( 1, 2 ),
        legend = c("Sugar content", "Fruit size"), bty = "n" )

dev.off()

###
###### TEMPERATURE
###

jpeg( "Figures/Fig4H.jpg", width = 500, height = 400, quality = 100 )

par( mfrow = c(1,1) )
par( mar = c(5.1, 4.1, 4.1, 2.1*2) ) 

x = data.temp$temp_ave
brix = data.temp$brix; brix = as.numeric( scale(brix) )
size = data.temp$size; size = as.numeric( scale(size) )
index = complete.cases(x) & complete.cases(brix) & complete.cases(size)
x = x[index]; brix = brix[index]; size = size[index]

plot( 1, 1, col = "white", axes = FALSE,
      xlim = range(x), ylim = c(-1,1), 
      xlab = "", ylab = "" )
lines( smooth.spline( x, brix, df = 3 ), col = 2, lwd = 2, lty = 1 )
lines( smooth.spline( x, size, df = 3 ), col = 4, lwd = 2, lty = 2 )
axis( 1 ); axis( 1, c(-100,99999) )
axis( 2, seq( -5, 5, 0.5 ), las = 2, 
      labels = formatC( mu1 + seq( -5, 5, 0.5 ) * sigma1, format = "f", digit = 1 ) )
axis( 4, seq( -5, 5, 0.5 ), las = 2,
      labels = formatC( mu2 + seq( -5, 5, 0.5 ) * sigma2, format = "f", digit = 1 ) )
axis( 3, c( -100, 1000 ) )
mtext( side = 1, line = 3, text = "Temperature" )
mtext( side = 2, line = 3, text = "Sugar Content (°Bx)" )
mtext( side = 4, line = 3, text = "Fruit Size (mm)" )
legend( "topleft", col = c( 2, 4 ), lwd = c( 1.5, 1.5 ), lty = c( 1, 2 ),
        legend = c("Sugar content", "Fruit size"), bty = "n" )

dev.off()

###
###### HUMIDITY
###

jpeg( "Figures/Fig4I.jpg", width = 500, height = 400, quality = 100 )

par( mfrow = c(1,1) )
par( mar = c(5.1, 4.1, 4.1, 2.1*2) ) 

x = data.temp$humi_ave
brix = data.temp$brix; brix = as.numeric( scale(brix) )
size = data.temp$size; size = as.numeric( scale(size) )
index = complete.cases(x) & complete.cases(brix) & complete.cases(size)
x = x[index]; brix = brix[index]; size = size[index]

plot( 1, 1, col = "white", axes = FALSE,
      xlim = range(x), ylim = c(-1,1), 
      xlab = "", ylab = "" )
lines( smooth.spline( x, brix, df = 3 ), col = 2, lwd = 2, lty = 1 )
lines( smooth.spline( x, size, df = 3 ), col = 4, lwd = 2, lty = 2 )
axis( 1 ); axis( 1, c(-100,99999) )
axis( 2, seq( -5, 5, 0.5 ), las = 2, 
      labels = formatC( mu1 + seq( -5, 5, 0.5 ) * sigma1, format = "f", digit = 1 ) )
axis( 4, seq( -5, 5, 0.5 ), las = 2,
      labels = formatC( mu2 + seq( -5, 5, 0.5 ) * sigma2, format = "f", digit = 1 ) )
axis( 3, c( -100, 1000 ) )
mtext( side = 1, line = 3, text = "Humidity" )
mtext( side = 2, line = 3, text = "Sugar Content" )
mtext( side = 4, line = 3, text = "Fruit Size (mm)" )
legend( "topleft", col = c( 2, 4 ), lwd = c( 1.5, 1.5 ), lty = c( 1, 2 ),
        legend = c("Sugar content", "Fruit size"), bty = "n" )

dev.off()

###
###### AIR PRESSURE
###

jpeg( "Figures/Fig4J.jpg", width = 500, height = 400, quality = 100 )

par( mfrow = c(1,1) )
par( mar = c(5.1, 4.1, 4.1, 2.1*2) ) 

x = data.temp$press_ave
brix = data.temp$brix; brix = as.numeric( scale(brix) )
size = data.temp$size; size = as.numeric( scale(size) )
index = complete.cases(x) & complete.cases(brix) & complete.cases(size)
x = x[index]; brix = brix[index]; size = size[index]

plot( 1, 1, col = "white", axes = FALSE,
      xlim = range(x), ylim = c(-1,1), 
      xlab = "", ylab = "" )
lines( smooth.spline( x, brix, df = 3 ), col = 2, lwd = 2, lty = 1 )
lines( smooth.spline( x, size, df = 3 ), col = 4, lwd = 2, lty = 2 )
axis( 1 ); axis( 1, c(-100,99999) )
axis( 2, seq( -5, 5, 0.5 ), las = 2, 
      labels = formatC( mu1 + seq( -5, 5, 0.5 ) * sigma1, format = "f", digit = 1 ) )
axis( 4, seq( -5, 5, 0.5 ), las = 2,
      labels = formatC( mu2 + seq( -5, 5, 0.5 ) * sigma2, format = "f", digit = 1 ) )
axis( 3, c( -100, 1000 ) )
mtext( side = 1, line = 3, text = "Air Pressure" )
mtext( side = 2, line = 3, text = "Sugar Content (°Bx)" )
mtext( side = 4, line = 3, text = "Fruit Size (mm)" )
legend( "topleft", col = c( 2, 4 ), lwd = c( 1.5, 1.5 ), lty = c( 1, 2 ),
        legend = c("Sugar content", "Fruit size"), bty = "n" )

dev.off()