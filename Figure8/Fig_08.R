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
### FIGURE 8 ###
################

u = sort( as.character( unique(data.new7$farm_id) ) )
u2 = paste0( toupper( substr( u, 1, 1 ) ), substr( u, 2, 3 ) )
m = length(u)

###
###### BRIX
###

jpeg( "Figures/Fig8A.jpg", width = 500, height = 500, quality = 100 )

par( mfrow = c(1,1) )
par( mar = c(5.1, 4.1, 4.1, 2.1) ) 

plot( 1, 1, xlim = c(0,7), ylim = c(8,14), axes = FALSE,
      xlab = "Month-week", ylab = "Sugar Content (°Bx)" )
axis( 2, seq( 0, 20, 1 ), las = 2 )
axis( 1, c( -100, 1:7, 100 ), 
      labels = c( "", c( "10-3", "10-4", "10-5", "11-1", "11-2", "11-3", "11-4", "" ) ) )
axis( 3, c( -100, 100 ) )
axis( 4, c( -100, 100 ) )

for (i in 1:m ) {
  data.temp = subset( data.new7, farm_id == u[i] )
  x = as.numeric( as.factor(data.temp$monthweek) )
  y = data.temp$brix
  temp = by( y, x, mean, na.rm = TRUE)
  lines( as.numeric( names(temp) ), as.numeric(temp), col = rainbow(m + 3)[i] )
  
}

legend( "topleft", lty = 1, col = rainbow(m + 3)[1:27], legend = u2, bty = "n", cex = 0.9 )

dev.off()

###
###### SIZE
###

jpeg( "Figures/Fig8B.jpg", width = 500, height = 500, quality = 100 )

par( mfrow = c(1,1) )
par( mar = c(5.1, 4.1, 4.1, 2.1) ) 

plot( 1, 1, xlim = c(0,7), ylim = c(35,75), axes = FALSE,
      xlab = "Month-week", ylab = "Fruit Size (mm)" )
axis( 2, seq( 0, 100, 5 ), las = 2 )
axis( 1, c( -100, 1:7, 100 ), 
      labels = c( "", c( "10-3", "10-4", "10-5", "11-1", "11-2", "11-3", "11-4", "" ) ) )
axis( 3, c( -100, 100 ) )
axis( 4, c( -100, 100 ) )

for (i in 1:m ) {
  data.temp = subset( data.new7, farm_id == u[i] )
  x = as.numeric( as.factor(data.temp$monthweek) )
  y = data.temp$size
  temp = by( y, x, mean, na.rm = TRUE)
  if ( length( unique(x) ) >= 4 ) {
    lines( as.numeric( names(temp) ), as.numeric(temp), col = rainbow(m + 3)[i] )
  }
}

#legend( "topright", lty = 1, col = rainbow(m + 3)[1:27], legend = u2, bty = "n", cex = 0.9 )

dev.off()