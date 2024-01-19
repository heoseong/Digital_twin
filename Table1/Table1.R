rm( list = ls() )

### comparison between hab and iab

### location and soil components

soil <- read.csv("soil_selected_farm.csv", header = T)
soil <- subset( soil, farm_id %in% c("hab", "iab") )
soil <- soil[ order(soil$farm_id), ]
temp1 = soil[ , c( "latitude", "longitude", 
                   "phosphate", "K", "Ca", "Mg", "acid", "OM", "EC" ) ]
temp1 = t(temp1)
temp1 = data.frame(temp1)

### agricultural practice

practice <- read.csv("practice_groupby.csv")
practice <- practice |> filter(farm_id %in% c("hab", "iab"))  

p1_hab = subset( practice, farm_id == "hab" & type == "pruning" )$month
p1_iab = subset( practice, farm_id == "iab" & type == "pruning" )$month

p2_hab = subset( practice, farm_id == "hab" & type == "fertilization" )$month
p2_iab = subset( practice, farm_id == "iab" & type == "fertilization" )$month

p3_hab = subset( practice, farm_id == "hab" & type == "pest_control" )$month
p3_iab = subset( practice, farm_id == "iab" & type == "pest_control" )$month

p4_hab = subset( practice, farm_id == "hab" & type == "mulching" )$month
p4_iab = subset( practice, farm_id == "iab" & type == "mulching" )$month

p5_hab = subset( practice, farm_id == "hab" & type == "thinning" )$month
p5_iab = subset( practice, farm_id == "iab" & type == "thinning" )$month

temp2 = rbind( c( p1_hab, p1_iab ),
               c( paste( p2_hab, collapse = ","), paste( p2_iab, collapse = ",") ),
               c( p3_hab, p3_iab ),
               c( p4_hab, p4_iab ),
               c( p5_hab, p5_iab ) )
temp2 = data.frame(temp2)
rownames(temp2) = c( "pruning", "fertilization", "spraying", "mulching", "thinning"  )

### fruit quality

fruit = read.csv("citrus_fruit_data_3_replicate.csv", encoding = "UTF-8")
fruit$monthweek = paste(fruit$researched_month, fruit$researched_week, sep = "-")

grade = read.csv("fruit_raw_grade.csv")
grade$monthweek = paste(grade$researched_month, grade$researched_week, sep = "-")

fruit_hab = subset( fruit, farm_id == "hab")
fruit_hab1 = subset( fruit_hab, monthweek %in% c( "10-2", "10-3" ) )
fruit_hab2 = subset( fruit_hab, monthweek %in% c( "10-4", "10-5" ) )
fruit_hab3 = subset( fruit_hab, monthweek %in% c( "11-1" ) )

grade_hab = subset( grade, farm_id == "hab")
grade_hab1 = subset( grade_hab, monthweek %in% c( "10-2", "10-3" ) )
grade_hab2 = subset( grade_hab, monthweek %in% c( "10-4", "10-5" ) )
grade_hab3 = subset( grade_hab, monthweek %in% c( "11-1" ) )

a1 = paste0( formatC( mean(fruit_hab1$brix), format = "f", digits = 1 ),
             " (", formatC( sd(fruit_hab1$brix), format = "f", digits = 2 ), ")" )

a2 = paste0( formatC( mean(fruit_hab2$brix), format = "f", digits = 1 ),
             " (", formatC( sd(fruit_hab2$brix), format = "f", digits = 2 ), ")" )

a3 = paste0( formatC( mean(fruit_hab3$brix), format = "f", digits = 1 ),
             " (", formatC( sd(fruit_hab3$brix), format = "f", digits = 2 ), ")" )

a4 = paste0( formatC( mean(fruit_hab1$size), format = "f", digits = 1 ),
             " (", formatC( sd(fruit_hab1$size), format = "f", digits = 2 ), ")" )

a5 = paste0( formatC( mean(fruit_hab2$size), format = "f", digits = 1 ),
             " (", formatC( sd(fruit_hab2$size), format = "f", digits = 2 ), ")" )

a6 = paste0( formatC( mean(fruit_hab3$size), format = "f", digits = 1 ),
             " (", formatC( sd(fruit_hab3$size), format = "f", digits = 2 ), ")" )

a7 = paste0( formatC( mean( grade_hab1$class %in% c( "S", "M" ) ) * 100, format = "f", digits = 1 ) )
a8 = paste0( formatC( mean( grade_hab2$class %in% c( "S", "M" ) ) * 100, format = "f", digits = 1 ) )
a9 = paste0( formatC( mean( grade_hab3$class %in% c( "S", "M" ) ) * 100, format = "f", digits = 1 ) )

a10 = paste0( formatC( mean( grade_hab1$grade == "unsalable" ) * 100, format = "f", digits = 1 ) )
a11 = paste0( formatC( mean( grade_hab2$grade == "unsalable" ) * 100, format = "f", digits = 1 ) )
a12 = paste0( formatC( mean( grade_hab3$grade == "unsalable" ) * 100, format = "f", digits = 1 ) )

frq_hab = c( a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12 )

fruit_iab = subset( fruit, farm_id == "iab")
fruit_iab1 = subset( fruit_iab, monthweek %in% c( "10-2", "10-3" ) )
fruit_iab2 = subset( fruit_iab, monthweek %in% c( "10-4", "10-5" ) )
fruit_iab3 = subset( fruit_iab, monthweek %in% c( "11-1" ) )

grade_iab = subset( grade, farm_id == "iab")
grade_iab1 = subset( grade_iab, monthweek %in% c( "10-2", "10-3" ) )
grade_iab2 = subset( grade_iab, monthweek %in% c( "10-4", "10-5" ) )
grade_iab3 = subset( grade_iab, monthweek %in% c( "11-1" ) )

b1 = paste0( formatC( mean(fruit_iab1$brix), format = "f", digits = 1 ),
             " (", formatC( sd(fruit_iab1$brix), format = "f", digits = 2 ), ")" )

b2 = paste0( formatC( mean(fruit_iab2$brix), format = "f", digits = 1 ),
             " (", formatC( sd(fruit_iab2$brix), format = "f", digits = 2 ), ")" )

b3 = paste0( formatC( mean(fruit_iab3$brix), format = "f", digits = 1 ),
             " (", formatC( sd(fruit_iab3$brix), format = "f", digits = 2 ), ")" )

b4 = paste0( formatC( mean(fruit_iab1$size), format = "f", digits = 1 ),
             " (", formatC( sd(fruit_iab1$size), format = "f", digits = 2 ), ")" )

b5 = paste0( formatC( mean(fruit_iab2$size), format = "f", digits = 1 ),
             " (", formatC( sd(fruit_iab2$size), format = "f", digits = 2 ), ")" )

b6 = paste0( formatC( mean(fruit_iab3$size), format = "f", digits = 1 ),
             " (", formatC( sd(fruit_iab3$size), format = "f", digits = 2 ), ")" )

b7 = formatC( mean( grade_iab1$class %in% c( "S", "M" ) ) * 100, format = "f", digits = 1 )
b8 = formatC( mean( grade_iab2$class %in% c( "S", "M" ) ) * 100, format = "f", digits = 1 )
b9 = formatC( mean( grade_iab3$class %in% c( "S", "M" ) ) * 100, format = "f", digits = 1 )

b10 = formatC( mean( grade_iab1$grade == "unsalable" ) * 100, format = "f", digits = 1 )
b11 = formatC( mean( grade_iab2$grade == "unsalable" ) * 100, format = "f", digits = 1 )
b12 = formatC( mean( grade_iab3$grade == "unsalable" ) * 100, format = "f", digits = 1 )

frq_iab = c( b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12 )

temp3 = data.frame( frq_hab, frq_iab )
rownames(temp3) = paste0( rep( c( "Sugar", "Size", "Prime", "Unsalable" ), each = 3 ),
                          rep( " ", 12 ),
                          rep( c( "Mid-Oct: ", "Late-Oct: ", "Early-Nov: " ), time = 4 ) )

### combine

colnames(temp1) = colnames(temp2) = colnames(temp3) = c( "Hab", "Iab" )
table1 = rbind( temp1, temp2, temp3 )
table1
