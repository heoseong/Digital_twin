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
### FIGURE 3 ###
################

load( "06_geodataframe.rdata" )  
library(sf) 
border = st_read( "jeju.shp" )
border = st_make_valid(border)
border = st_sf(border)

temp = unlist( border$geometry )
x = temp[ temp > 100 ]
y = temp[ temp < 100 ]

###
###### OCTOBER WEEK 4-5
###

jpeg( "Figures/Fig3_1.jpg", width = 500, height = 400, quality = 100 )

par( mfrow = c(1,1) )
par( mar = c(5.1, 4.1*1.2, 4.1*0.5, 2.1) ) 

plot( x, y, cex = 0.1, col = 1, 
      xlim = c(126.15,127), ylim = c(33.1,33.575),
      xlab = "", ylab = "", axes = FALSE )
axis( 1, seq( 125, 128, 0.2 ), labels = paste0( seq( 125, 128, 0.2 ), " °E" ) )
axis( 2, seq( 33, 34, 0.1 ), labels = paste0( seq( 33, 34, 0.1 ), " °N" ), las = 2 )
axis( 3, c( 125, 128 ) )
axis( 4, c( 33, 34 ) )

c.names = c( "farm_id", "brix", "size", "latitude", "longitude" )
temp = subset( data.new7, monthweek %in% c( "10-4", "10-5" ) )[,c.names]
temp1 = by( temp$brix, temp$farm_id, mean, na.rm = TRUE )
temp2 = by( temp$size, temp$farm_id, mean, na.rm = TRUE )
temp3 = data.frame( farm_id = names(temp1),
                    brix_ave = as.numeric(temp1), 
                    size_ave = as.numeric(temp2) )
temp3 = temp3[ complete.cases(temp3), ]
temp3$brix_col = ifelse( temp3$brix_ave < 10.5, 1, 19 )

temp4 = data0[ complete.cases(data0), ]
temp4 = temp4[ duplicated(temp4$farm_id) == FALSE, ]
temp4 = temp4[ , c( "farm_id", "latitude", "longitude" ) ]
temp5 = temp.oct45 = merge( temp3, temp4, by = "farm_id" )
temp5$brix_col = as.numeric(temp5$brix_col)

par( new = TRUE )
plot( temp5$longitude, temp5$latitude,
      xlim = c(126.15,127), ylim = c(33.1,33.575),
      xlab = "", ylab = "", axes = FALSE,
      pch = temp5$brix_col, col = "orange1", cex = temp5$size_ave / 100 * 3 )

# text( x = 126.9, y = 33.2, labels = "Sugar content", font = 1, cex = 0.9 )
# legend( x = 126.8, y = 33.195, pch = c( 1, 19 ), pt.cex = 1.5, col = "orange1",
#         legend = c( "< 10.5 °Brix", "\u2265 10.5 °Brix" ), bty = "n", cex = 0.9 )
# 
# text( x = 126.6, y = 33.2, adj = 0, labels = "Fruit size", font = 1, cex = 0.9 )
# legend( x = 126.6, y = 33.195, pch = 19, 
#         pt.cex = c( 40, 50, 60, 70 ) / 100 * 3, col = "orange1",
#         legend = c( "40 mm", "50 mm", "60 mm", "70 mm" ), bty = "n", cex = 0.9 )

mtext( side = 3, adj = 0.05, line = -2, text = "Oct 4-5", font = 2 )

dev.off()

###
###### NOVEMBER WEEK 1-2
###

jpeg( "Figures/Fig3_2.jpg", width = 500, height = 400, quality = 100 )

par( mfrow = c(1,1) )
par( mar = c(5.1, 4.1*1.2, 4.1*0.5, 2.1) ) 

plot( x, y, cex = 0.1, col = 1, 
      xlim = c(126.15,127), ylim = c(33.1,33.575),
      xlab = "", ylab = "", axes = FALSE )
axis( 1, seq( 125, 128, 0.2 ), labels = paste0( seq( 125, 128, 0.2 ), " °E" ) )
axis( 2, seq( 33, 34, 0.1 ), labels = paste0( seq( 33, 34, 0.1 ), " °N" ), las = 2 )
axis( 3, c( 125, 128 ) )
axis( 4, c( 33, 34 ) )

c.names = c( "farm_id", "brix", "size", "latitude", "longitude" )
temp = subset( data.new7, monthweek %in% c( "11-1", "11-2" ) )[,c.names]
temp1 = by( temp$brix, temp$farm_id, mean, na.rm = TRUE )
temp2 = by( temp$size, temp$farm_id, mean, na.rm = TRUE )
temp3 = data.frame( farm_id = names(temp1),
                    brix_ave = as.numeric(temp1), 
                    size_ave = as.numeric(temp2) )
temp3 = temp3[ complete.cases(temp3), ]
temp3$brix_col = ifelse( temp3$brix_ave < 10.5, 1, 19 )

temp4 = data0[ complete.cases(data0), ]
temp4 = temp4[ duplicated(temp4$farm_id) == FALSE, ]
temp4 = temp4[ , c( "farm_id", "latitude", "longitude" ) ]
temp5 = temp.oct45 = merge( temp3, temp4, by = "farm_id" )
temp5$brix_col = as.numeric(temp5$brix_col)

par( new = TRUE )
plot( temp5$longitude, temp5$latitude,
      xlim = c(126.15,127), ylim = c(33.1,33.575),
      xlab = "", ylab = "", axes = FALSE,
      pch = temp5$brix_col, col = "orange1", cex = temp5$size_ave / 100 * 3 )

# text( x = 126.9, y = 33.2, labels = "Sugar content", font = 1, cex = 0.9 )
# legend( x = 126.8, y = 33.195, pch = c( 1, 19 ), pt.cex = 1.5, col = "orange1",
#         legend = c( "< 10.5 °Brix", "\u2265 10.5 °Brix" ), bty = "n", cex = 0.9 )
# 
# text( x = 126.6, y = 33.2, adj = 0, labels = "Fruit size", font = 1, cex = 0.9 )
# legend( x = 126.6, y = 33.195, pch = 19, 
#         pt.cex = c( 40, 50, 60, 70 ) / 100 * 3, col = "orange1",
#         legend = c( "40 mm", "50 mm", "60 mm", "70 mm" ), bty = "n", cex = 0.9 )

mtext( side = 3, adj = 0.05, line = -2, text = "Nov 1-2", font = 2 )

dev.off()

###
###### NOVEMBER WEEK 3-4
###

jpeg( "Figures/Fig3_3.jpg", width = 500, height = 400, quality = 100 )

par( mfrow = c(1,1) )
par( mar = c(5.1, 4.1*1.2, 4.1*0.5, 2.1) ) 

plot( x, y, cex = 0.1, col = 1, 
      xlim = c(126.15,127), ylim = c(33.1,33.575),
      xlab = "", ylab = "", axes = FALSE )
axis( 1, seq( 125, 128, 0.2 ), labels = paste0( seq( 125, 128, 0.2 ), " °E" ) )
axis( 2, seq( 33, 34, 0.1 ), labels = paste0( seq( 33, 34, 0.1 ), " °N" ), las = 2 )
axis( 3, c( 125, 128 ) )
axis( 4, c( 33, 34 ) )

c.names = c( "farm_id", "brix", "size", "latitude", "longitude" )
temp = subset( data.new7, monthweek %in% c( "11-3", "11-4" ) )[,c.names]
temp1 = by( temp$brix, temp$farm_id, mean, na.rm = TRUE )
temp2 = by( temp$size, temp$farm_id, mean, na.rm = TRUE )
temp3 = data.frame( farm_id = names(temp1),
                    brix_ave = as.numeric(temp1), 
                    size_ave = as.numeric(temp2) )
temp3 = temp3[ complete.cases(temp3), ]
temp3$brix_col = ifelse( temp3$brix_ave < 10.5, 1, 19 )

temp4 = data0[ complete.cases(data0), ]
temp4 = temp4[ duplicated(temp4$farm_id) == FALSE, ]
temp4 = temp4[ , c( "farm_id", "latitude", "longitude" ) ]
temp5 = temp.oct45 = merge( temp3, temp4, by = "farm_id" )
temp5$brix_col = as.numeric(temp5$brix_col)

par( new = TRUE )
plot( temp5$longitude, temp5$latitude,
      xlim = c(126.15,127), ylim = c(33.1,33.575),
      xlab = "", ylab = "", axes = FALSE,
      pch = temp5$brix_col, col = "orange1", cex = temp5$size_ave / 100 * 3 )

# text( x = 126.9, y = 33.2, labels = "Sugar content", font = 1, cex = 0.9 )
# legend( x = 126.8, y = 33.195, pch = c( 1, 19 ), pt.cex = 1.5, col = "orange1",
#         legend = c( "< 10.5 °Brix", "\u2265 10.5 °Brix" ), bty = "n", cex = 0.9 )
# 
# text( x = 126.6, y = 33.2, adj = 0, labels = "Fruit size", font = 1, cex = 0.9 )
# legend( x = 126.6, y = 33.195, pch = 19, 
#         pt.cex = c( 40, 50, 60, 70 ) / 100 * 3, col = "orange1",
#         legend = c( "40 mm", "50 mm", "60 mm", "70 mm" ), bty = "n", cex = 0.9 )

mtext( side = 3, adj = 0.05, line = -2, text = "Nov 3-4", font = 2 )

dev.off()
