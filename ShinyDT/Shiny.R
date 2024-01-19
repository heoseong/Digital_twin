library(shiny)

load("data.RData")
data0 = data.new7
rm(data.new7)
data0 = data0[ is.na(data0$farm_id) == FALSE, ]
data0 = data0[ is.na(data0$tree_id) == FALSE, ]
data0$tag = as.numeric( substr( data0$tree_id, 5, 8 ) )
data0 = data0[ order( data0$farm_id, data0$monthweek, data0$tag ), ]

##########
### ui ###
##########

ui = fluidPage(

  titlePanel( "Agricultural Digital Twin (Demo)" ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput( inputId = "orchard",
                   label = "Select an orchard:",
                   choices = sort( unique(data0$farm_id) ) ),
  
      actionButton( "run", "Submit" ),
  
      selectInput( inputId = "tree",
                   label = "Select a tree:",
                   choices = c() ),
      
      actionButton( "do", "Submit" ) 
      
      ), ### end of sidebarPanel
  
    mainPanel(
    
      tabsetPanel(
      
        tabPanel( "Map", plotOutput( "Map", width = "500px", height = "400px" ) ),
        tabPanel( "Soil", plotOutput( "Soil", width = "600px", height = "800px" ) ),
        tabPanel( "Weather", plotOutput( "Weather", width = "300px", height = "600px" ) ),
        tabPanel( "Agricultural Practice", verbatimTextOutput( "Practice" ), ),
        tabPanel( "Sugar Content Distribution", plotOutput( "Fruit1", width = "800px", height = "400px" ) ),
        tabPanel( "Fruit Size Distribution", plotOutput( "Fruit2", width = "800px", height = "400px" ) ),
        tabPanel( "Sugar Content History", plotOutput( "History1", width = "800px", height = "400px" ) ),
        tabPanel( "Fruit Size History", plotOutput( "History2", width = "800px", height = "400px" ) )
      
      ) ### end of tabsetPanel
      
    ) ### end of mainPanel
    
  ) ### end of sidebarLayout
  
)

##############
### server ###
##############

server = function(input, output, session) {
  
  Data0 = reactive( {
    
    load("data.RData")
    data0 = data.new7
    rm(data.new7)
    data0 = data0[ is.na(data0$farm_id) == FALSE, ]
    data0 = data0[ is.na(data0$tree_id) == FALSE, ]
    data0$tag = as.numeric( substr( data0$tree_id, 5, 8 ) )
    data0 = data0[ order( data0$farm_id, data0$monthweek, data0$tag ), ]
    return(data0)
    
  } )
  
  g = reactiveValues()
  
  ### select orchard and open options for tree
  
  observeEvent( input$run, {
    
    data0 = Data0()
    data1 = subset( data0, farm_id == input$orchard ) ### subset the orchard selected
    
    updateSelectInput( session, 
                       inputId = "tree", 
                       label = "Select a tree:",
                       choices = unique(data1$tree_id) )
    
    g$data1 = data1
    
  } ) ### end of observeEvent (after selecting orchard)
  
  ### tree selected and execute
  
  observeEvent( input$do, {
    
    data1 = isolate(g$data1)
    data2 = subset( data1, tree_id == input$tree ) ### subset the orchard selected
    
    orchard = input$orchard
    tree = input$tree

    n2 = dim(data2)[1]
    week = data2[n2,]$monthweek ### the most recent week in the data
    
    #####################
    ### DATA CLEANING ###
    #####################
    
    ###
    ###### SOIL DATA
    ###
    
    soil0 = data0[ , c( "farm_id", "monthweek", "acid", "phosphate", "OM", "Mg", "K", "Ca", "EC" ) ]
    soil0 = subset( soil0, monthweek == week )
    soil0 = soil0[ complete.cases(soil0) == TRUE, ]
    soil0 = soil0[ duplicated(soil0$farm_id) == FALSE, ]
    soil1 = subset( soil0, farm_id == orchard )
    
    ###
    ###### WEATHER DATA
    ###
    
    weather0 = data0[ , c( "farm_id", "monthweek", "temp_ave", "humi_ave", "press_ave" ) ]
    weather0 = subset( weather0, monthweek == week )
    weather0 = weather0[ complete.cases(weather0) == TRUE, ]
    weather0 = weather0[ duplicated(weather0$farm_id) == FALSE, ]
    weather1 = subset( weather0, farm_id == orchard )
    
    ###
    ###### AGRICULTURAL PRACTICE DATA
    ###
    
    practice0 = data0[ , c( "farm_id", "fertilization", "mulching", "pest_control", "pruning", "thinning" ) ]
    practice0 = practice0[ complete.cases(practice0) == TRUE, ]
    practice0 = practice0[ duplicated(practice0$farm_id) == FALSE, ]
    practice1 = subset( practice0, farm_id == orchard )
    
    ###
    ###### FRUIT QUALITY DATA
    ###
    
    fruit0 = data0[ , c( "farm_id", "monthweek", "tree_id", "brix", "size" ) ]
    fruit0 = subset( fruit0, monthweek == week )
    fruit0 = fruit0[ complete.cases(fruit0) == TRUE, ]
    fruit1 = subset( fruit0, farm_id == orchard )
    fruit2 = subset( fruit0, tree_id == tree )
    
    ###
    ###### GEO DATA
    ###
    
    geo0 = data0[ , c( "farm_id", "latitude", "longitude" ) ]
    geo0 = geo0[ complete.cases(geo0) == TRUE, ]
    geo0 = geo0[ duplicated(geo0$farm_id) == FALSE, ]
    geo1 = subset( geo0, farm_id == orchard )
    
    ###########
    ### MAP ###
    ###########
    
    output$Map = renderPlot( {

      load( "06_geodataframe.RData" )
      library(sf)
      border = st_read( "jeju.shp" )
      border = st_make_valid(border)
      border = st_sf(border)

      temp = unlist( border$geometry )
      x = temp[ temp > 100 ]
      y = temp[ temp < 100 ]

      par( mfrow = c(1,1) )
      par( mar = c(5.1, 4.1*1.2, 4.1, 2.1) )

      plot( x, y, cex = 0.1, col = 1,
            xlim = c(126.15,127), ylim = c(33.1,33.575),
            xlab = "", ylab = "", axes = FALSE,
            main = "Map of Jeju Island" )
      axis( 1, seq( 125, 128, 0.2 ), labels = paste0( seq( 125, 128, 0.2 ), " °E" ) )
      axis( 2, seq( 33, 34, 0.1 ), labels = paste0( seq( 33, 34, 0.1 ), " °N" ), las = 2 )
      axis( 3, c( 125, 128 ) )
      axis( 4, c( 33, 34 ) )

      par( new = TRUE )
      plot( latitude ~ longitude, data = geo0, pch = 19, cex = 1.5, col = 8,
            xlim = c(126.15,127), ylim = c(33.1,33.575),
            xlab = "", ylab = "", axes = FALSE )

      par( new = TRUE )
      plot( latitude ~ longitude, data = geo1, pch = 19, cex = 2, col = 2,
            xlim = c(126.15,127), ylim = c(33.1,33.575),
            xlab = "", ylab = "", axes = FALSE )

      legend( "bottomright", pch = c(19,19), cex = 1, col = c(2,8),
              legend = c( "Orchard selected", "Other orchards in Jeju Island" ), bty = "n" )

    } ) ### end of renderPlot
    
    ####################
    ### SOIL GRAPHIC ###
    ####################

    ### RDA Recommendations
    ### pH 5.5-6.5
    ### OM 110-150
    ### Av.P2O4 200-300
    ### K 0.5-1.3
    ### Ca 5.0-6.0
    ### Mg 1.5-2.0
    
    output$Soil = renderPlot( {
      
      par( mfrow = c( 4, 2 ) )
      par( mar = c( 5.1, 4.1, 4.1, 2.1 ) )
      
      ###
      ###### PHOSPHATE
      ###
      
      x1 = soil1$phosphate
      x0 = soil0$phosphate
      mu = mean( x0, na.rm = TRUE )
      sigma = sd( x0, na.rm = TRUE )
      z0 = ( x0 - mu ) / sigma
      z1 = ( x1 - mu ) / sigma
      f0 = density(z0)
      plot( f0, col = "white", xlab = expression('Av. P'[2]*'O'[4]), ylab = "", 
            main = "", axes = FALSE )
      
      a = 200; b = 300; delta = seq( a, b, (b - a) / 100 )
      abline( v = ( delta - mu ) / sigma, col = "grey90" )
      lines( f0$x, f0$y )
      
      axis( 1, -5:5, labels = formatC( -5:5 * sigma + mu, format = "f", digits = 1 ) )
      abline( v = z1, col = 2 )
      w = f0$y / sum(f0$y)
      percent = round( sum( w[ f0$x < z1 ] ) * 100 )
      mtext( side = 3, line = 1, at = z1, adj = 0.5, 
             text = paste0( percent, "%" ), col = 2 )

      ###
      ###### K
      ###
      
      x1 = soil1$K
      x0 = soil0$K
      mu = mean( x0, na.rm = TRUE )
      sigma = sd( x0, na.rm = TRUE )
      z0 = ( x0 - mu ) / sigma
      z1 = ( x1 - mu ) / sigma
      f0 = density(z0)
      plot( f0, col = "white", xlab = "Exch. K", ylab = "", 
            main = "", axes = FALSE )
      
      a = 0.5; b = 1.3; delta = seq( a, b, (b - a) / 100 )
      abline( v = ( delta - mu ) / sigma, col = "grey90" )
      lines( f0$x, f0$y )
      
      axis( 1, -5:5, labels = formatC( -5:5 * sigma + mu, format = "f", digits = 1 ) )
      abline( v = z1, col = 2 )
      w = f0$y / sum(f0$y)
      percent = round( sum( w[ f0$x < z1 ] ) * 100 )
      mtext( side = 3, line = 1, at = z1, adj = 0.5, 
             text = paste0( percent, "%" ), col = 2 )
      
      ###
      ###### Ca
      ###
      
      x1 = soil1$Ca
      x0 = soil0$Ca
      mu = mean( x0, na.rm = TRUE )
      sigma = sd( x0, na.rm = TRUE )
      z0 = ( x0 - mu ) / sigma
      z1 = ( x1 - mu ) / sigma
      f0 = density(z0)
      plot( f0, col = "white", xlab = "Exch. Ca", ylab = "", 
            main = "", axes = FALSE )
      
      a = 5; b = 6; delta = seq( a, b, (b - a) / 100 )
      abline( v = ( delta - mu ) / sigma, col = "grey90" )
      lines( f0$x, f0$y )
      
      axis( 1, -5:5, labels = formatC( -5:5 * sigma + mu, format = "f", digits = 1 ) )
      abline( v = z1, col = 2 )
      w = f0$y / sum(f0$y)
      percent = round( sum( w[ f0$x < z1 ] ) * 100 )
      mtext( side = 3, line = 1, at = z1, adj = 0.5, 
             text = paste0( percent, "%" ), col = 2 )
      
      ###
      ###### Mg
      ###
      
      x1 = soil1$Mg
      x0 = soil0$Mg
      mu = mean( x0, na.rm = TRUE )
      sigma = sd( x0, na.rm = TRUE )
      z0 = ( x0 - mu ) / sigma
      z1 = ( x1 - mu ) / sigma
      f0 = density(z0)
      plot( f0, col = "white", xlab = "Exch. Mg", ylab = "", 
            main = "", axes = FALSE )
      
      a = 1.5; b = 2; delta = seq( a, b, (b - a) / 100 )
      abline( v = ( delta - mu ) / sigma, col = "grey90" )
      lines( f0$x, f0$y )
      
      axis( 1, -5:5, labels = formatC( -5:5 * sigma + mu, format = "f", digits = 1 ) )
      abline( v = z1, col = 2 )
      w = f0$y / sum(f0$y)
      percent = round( sum( w[ f0$x < z1 ] ) * 100 )
      mtext( side = 3, line = 1, at = z1, adj = 0.5, 
             text = paste0( percent, "%" ), col = 2 )
      
      ###
      ###### ACid (pH)
      ###
      
      x1 = soil1$acid
      x0 = soil0$acid
      mu = mean( x0, na.rm = TRUE )
      sigma = sd( x0, na.rm = TRUE )
      z0 = ( x0 - mu ) / sigma
      z1 = ( x1 - mu ) / sigma
      f0 = density(z0)
      plot( f0, col = "white", xlab = "pH", ylab = "", 
            main = "", axes = FALSE )
      
      a = 5.5; b = 6.5; delta = seq( a, b, (b - a) / 100 )
      abline( v = ( delta - mu ) / sigma, col = "grey90" )
      lines( f0$x, f0$y )
      
      axis( 1, -5:5, labels = formatC( -5:5 * sigma + mu, format = "f", digits = 1 ) )
      abline( v = z1, col = 2 )
      w = f0$y / sum(f0$y)
      percent = round( sum( w[ f0$x < z1 ] ) * 100 )
      mtext( side = 3, line = 1, at = z1, adj = 0.5, 
             text = paste0( percent, "%" ), col = 2 )
      
      ###
      ###### ORGANIC MATTER (OM)
      ###
      
      x1 = soil1$OM
      x0 = soil0$OM
      mu = mean( x0, na.rm = TRUE )
      sigma = sd( x0, na.rm = TRUE )
      z0 = ( x0 - mu ) / sigma
      z1 = ( x1 - mu ) / sigma
      f0 = density(z0)
      plot( f0, col = "white", xlab = "OM", ylab = "", 
            main = "", axes = FALSE )
      
      a = 110; b = 150; delta = seq( a, b, (b - a) / 100 )
      abline( v = ( delta - mu ) / sigma, col = "grey90" )
      lines( f0$x, f0$y )
      
      axis( 1, -5:5, labels = formatC( -5:5 * sigma + mu, format = "f", digits = 1 ) )
      abline( v = z1, col = 2 )
      w = f0$y / sum(f0$y)
      percent = round( sum( w[ f0$x < z1 ] ) * 100 )
      mtext( side = 3, line = 1, at = z1, adj = 0.5, 
             text = paste0( percent, "%" ), col = 2 )
      
      ###
      ###### ELECTRICAL CONDUCTIVITY (EC)
      ###
      
      x1 = soil1$EC
      x0 = soil0$EC
      mu = mean( x0, na.rm = TRUE )
      sigma = sd( x0, na.rm = TRUE )
      z0 = ( x0 - mu ) / sigma
      z1 = ( x1 - mu ) / sigma
      f0 = density(z0)
      plot( f0, col = "white", xlab = "EC", ylab = "", 
            main = "", axes = FALSE )
      
      a = 1.5; b = 2; delta = seq( a, b, (b - a) / 100 )
      abline( v = ( delta - mu ) / sigma, col = "grey90" )
      lines( f0$x, f0$y )
      
      axis( 1, -5:5, labels = formatC( -5:5 * sigma + mu, format = "f", digits = 1 ) )
      abline( v = z1, col = 2 )
      w = f0$y / sum(f0$y)
      percent = round( sum( w[ f0$x < z1 ] ) * 100 )
      mtext( side = 3, line = 1, at = z1, adj = 0.5, 
             text = paste0( percent, "%" ), col = 2 )
      
      ###
      ###### LEGEND
      ###
      
      plot( 1, 1, col = "white", xlab = "", ylab = "", axes = FALSE )
      legend( "topleft", lty = 1, lwd = 12, col = "grey90", 
              legend = "RDA Recommendation", bty = "n", cex = 1.5 )
      
    } ) ### end of renderPlot

    #######################
    ### WEATHER GRAPHIC ###
    #######################
    
    output$Weather = renderPlot( {
      
      par( mfrow = c( 3, 1 ) )
      par( mar = c( 5.1, 4.1, 4.1, 2.1 ) )
      
      ###
      ###### TEMPERATURE
      ###
      
      x1 = weather1$temp_ave
      x0 = weather0$temp_ave
      mu = mean( x0, na.rm = TRUE )
      sigma = sd( x0, na.rm = TRUE )
      z0 = ( x0 - mu ) / sigma
      z1 = ( x1 - mu ) / sigma
      f0 = density(z0)
      plot( f0, xlab = "Temperature", ylab = "", 
            main = "", axes = FALSE )
      axis( 1, -5:5, labels = formatC( -5:5 * sigma + mu, format = "f", digits = 1 ) )
      abline( v = z1, col = 2 )
      w = f0$y / sum(f0$y)
      percent = round( sum( w[ f0$x < z1 ] ) * 100 )
      mtext( side = 3, line = 1, at = z1, adj = 0.5, 
             text = paste0( percent, "%" ), col = 2 )
      
      ###
      ###### HUMIDITY
      ###
      
      x1 = weather1$humi_ave
      x0 = weather0$humi_ave
      mu = mean( x0, na.rm = TRUE )
      sigma = sd( x0, na.rm = TRUE )
      z0 = ( x0 - mu ) / sigma
      z1 = ( x1 - mu ) / sigma
      f0 = density(z0)
      plot( f0, xlab = "Humidity", ylab = "", 
            main = "", axes = FALSE )
      axis( 1, -5:5, labels = formatC( -5:5 * sigma + mu, format = "f", digits = 1 ) )
      abline( v = z1, col = 2 )
      w = f0$y / sum(f0$y)
      percent = round( sum( w[ f0$x < z1 ] ) * 100 )
      mtext( side = 3, line = 1, at = z1, adj = 0.5, 
             text = paste0( percent, "%" ), col = 2 )
      
      ###
      ###### AIR PRESSURE
      ###
      
      x1 = weather1$press_ave
      x0 = weather0$press_ave
      mu = mean( x0, na.rm = TRUE )
      sigma = sd( x0, na.rm = TRUE )
      z0 = ( x0 - mu ) / sigma
      z1 = ( x1 - mu ) / sigma
      f0 = density(z0)
      plot( f0, xlab = "Air Pressure", ylab = "", 
            main = "", axes = FALSE )
      axis( 1, -5:5, labels = formatC( -5:5 * sigma + mu, format = "f", digits = 1 ) )
      abline( v = z1, col = 2 )
      w = f0$y / sum(f0$y)
      percent = round( sum( w[ f0$x < z1 ] ) * 100 )
      mtext( side = 3, line = 1, at = z1, adj = 0.5, 
             text = paste0( percent, "%" ), col = 2 )
    
    } ) ### end of renderPlot

    #############################
    ### AGRICULTURAL PRACTICE ###
    #############################
    
    output$Practice = renderPrint( {
      
      a1 = paste0( "Period: ", "January and October, 2021" )
      a2 = paste0( "Number of reference orchards in Jeju Island: ", dim(practice0)[1] )
      
      ###
      ###### FERTILIZATION
      ###
      
      temp0 = practice0$fertilization
      temp1 = practice1$fertilization
      
      b1 = paste0( "Agricultural practice: ", "Fertilization" )
      b2 = paste0( "Minimum: ", min(temp0) )
      b3 = paste0( "Maximum: ", max(temp0) )
      b4 = paste0( "Average: ", formatC( mean(temp0), format = "f", digit = 1 ) )
      b5 = paste0( "Median: ", formatC( median(temp0), format = "f", digit = 0 ) )
      b6 = paste0( "Orchard selected: ", temp1, " time(s)" )
      b7 = paste0( "Percentile: ", round( mean( temp0 <= temp1 ) * 100 ), "% of the orchards did as frequent as this orchard or less." )
      
      ###
      ###### MULCHING
      ###
      
      temp0 = practice0$mulching
      temp1 = practice1$mulching
      
      c1 = paste0( "Agricultural practice: ", "Mulching" )
      c2 = paste0( "Minimum: ", min(temp0) )
      c3 = paste0( "Maximum: ", max(temp0) )
      c4 = paste0( "Average: ", formatC( mean(temp0), format = "f", digit = 1 ) )
      c5 = paste0( "Median: ", formatC( median(temp0), format = "f", digit = 0 ) )
      c6 = paste0( "Orchard selected: ", temp1, " time(s)" )
      c7 = paste0( "Percentile: ", round( mean( temp0 <= temp1 ) * 100 ), "% of the orchards did as frequent as this orchard or less." )
      
      ###
      ###### SPRAYING
      ###
      
      temp0 = practice0$pest_control
      temp1 = practice1$pest_control
      
      d1 = paste0( "Agricultural practice: ", "Spraying" )
      d2 = paste0( "Minimum: ", min(temp0) )
      d3 = paste0( "Maximum: ", max(temp0) )
      d4 = paste0( "Average: ", formatC( mean(temp0), format = "f", digit = 1 ) )
      d5 = paste0( "Median: ", formatC( median(temp0), format = "f", digit = 0 ) )
      d6 = paste0( "Orchard selected: ", temp1, " time(s)" )
      d7 = paste0( "Percentile: ", round( mean( temp0 <= temp1 ) * 100 ), "% of the orchards did as frequent as this orchard or less." )
      
      ###
      ###### PRUNING
      ###
      
      temp0 = practice0$pruning
      temp1 = practice1$pruning
      
      e1 = paste0( "Agricultural practice: ", "Pruning" )
      e2 = paste0( "Minimum: ", min(temp0) )
      e3 = paste0( "Maximum: ", max(temp0) )
      e4 = paste0( "Average: ", formatC( mean(temp0), format = "f", digit = 1 ) )
      e5 = paste0( "Median: ", formatC( median(temp0), format = "f", digit = 0 ) )
      e6 = paste0( "Orchard selected: ", temp1, " time(s)" )
      e7 = paste0( "Percentile: ", round( mean( temp0 <= temp1 ) * 100 ), "% of the orchards did as frequent as this orchard or less." )
      
      ###
      ###### THINNING
      ###
      
      temp0 = practice0$thinning
      temp1 = practice1$thinning
      
      f1 = paste0( "Agricultural practice: ", "Thinning" )
      f2 = paste0( "Minimum: ", min(temp0) )
      f3 = paste0( "Maximum: ", max(temp0) )
      f4 = paste0( "Average: ", formatC( mean(temp0), format = "f", digit = 1 ) )
      f5 = paste0( "Median: ", formatC( median(temp0), format = "f", digit = 0 ) )
      f6 = paste0( "Orchard selected: ", temp1, " time(s)" )
      f7 = paste0( "Percentile: ", round( mean( temp0 <= temp1 ) * 100 ), "% of the orchards did as frequent as this orchard or less." )
      
      report = paste( "\n", a1, "\n", a2, "\n", "\n", 
                            b1, "\n", b2, "\n", b3, "\n", b4, "\n", b5, "\n", b6, "\n", b7, "\n", "\n", 
                            c1, "\n", c2, "\n", c3, "\n", c4, "\n", c5, "\n", c6, "\n", c7, "\n", "\n", 
                            d1, "\n", d2, "\n", d3, "\n", d4, "\n", d5, "\n", d6, "\n", d7, "\n", "\n", 
                            e1, "\n", e2, "\n", e3, "\n", e4, "\n", e5, "\n", e6, "\n", e7, "\n", "\n", 
                            f1, "\n", f2, "\n", f3, "\n", f4, "\n", f5, "\n", f6, "\n", f7, "\n", "\n" )
      
      cat(report)
      
    } ) ### end of renderText

    #####################
    ### FRUIT QUALITY ###
    #####################
    
    ###
    ###### SUGAR CONTENT
    ###
    
    output$Fruit1 = renderPlot( {
      
      par( mfrow = c( 1, 2 ) )
      par( mar = c( 5.1 * 1.75, 4.1, 4.1, 2.1 ) )
      
      ### INTER-ORCHARD (ORCHARD-LEVEL)
      
      u = sort( unique(fruit0$farm_id) )
      
      x1 = fruit1$brix
      x0 = fruit0$brix
      mu = mean( x0, na.rm = TRUE ); sigma = sd( x0, na.rm = TRUE )
      
      z0 = ( x0 - mu ) / sigma
      z1 = ( x1 - mu ) / sigma
      
      max.temp = rep( NA, length(u) )
      
      for (i in 1:length(u) ) {
        index = which( fruit0$farm_id == u[i] )
        max.temp[i] = max( density( z0[index], adjust = 3 )$y )
      }
      
      plot( 1, 1, col = "white", axes = FALSE, 
            xlim = c(-4,4), ylim = c( 0, max( max.temp, na.rm = TRUE ) * 1.2 ),
            xlab = "Sugar Content (°Bx)", ylab = "",
            main = "Inter-orchard Distribution (KDE)" )
      
      axis( 1, -5:5, labels = formatC( -5:5 * sigma + mu, format = "f", digits = 1 ) )
      
      
      for (i in 1:length(u) ) {
        index = which( fruit0$farm_id == u[i] )
        d.temp = density( z0[index], adjust = 3 )
        lines( d.temp, col = 8, lwd = 1 )
      }
      
      index = which( fruit0$farm_id == orchard )
      d.temp = density( z0[index], adjust = 3 )
      lines( d.temp, col = 2, lwd = 1 )
      
      legend( "topleft", lty = 1, col = c(2,8), bty = "n", 
              legend = c( paste0( "Orchard selected (", orchard, ")" ), "Other orchards in Jeju Island" ) )

      sen1 = paste0( "Estimate: The average sugar content in this orchard (", orchard, ") is" )
      sen2 = paste0( "higher than", round( mean( z0 <= mean(z1) ) * 100 ), "% of all mandarins produced in ", length(u), " orchards" ) 
      sen3 = paste0( "in Jeju Island recorded this week." )
      
      mtext( side = 1, line = 5, adj = 0.05, text = sen1 )
      mtext( side = 1, line = 6, adj = 0.05, text = sen2 )
      mtext( side = 1, line = 7, adj = 0.05, text = sen3 )
      
      ### INTRA-ORCHARD (TREE-LEVEL)
      
      u = sort( unique(fruit1$tree_id) )
      
      x1 = fruit2$brix
      x0 = fruit1$brix
      mu = mean( x0, na.rm = TRUE ); sigma = sd( x0, na.rm = TRUE )
      
      z0 = ( x0 - mu ) / sigma
      z1 = ( x1 - mu ) / sigma
      
      max.temp = rep( NA, length(u) )
      
      for (i in 1:length(u) ) {
        index = which( fruit1$tree_id == u[i] )
        max.temp[i] = max( density( z0[index], adjust = 3 )$y )
      }
      
      plot( 1, 1, col = "white", axes = FALSE, 
            xlim = c(-4,4), ylim = c( 0, max( max.temp, na.rm = TRUE ) * 1.2 ),
            xlab = "Sugar Content (°Bx)", ylab = "",
            main = "Intra-orchard Distribution (KDE)" )
      
      axis( 1, -5:5, labels = formatC( -5:5 * sigma + mu, format = "f", digits = 1 ) )
      
      for (i in 1:length(u) ) {
        index = which( fruit1$tree_id == u[i] )
        d.temp = density( z0[index], adjust = 3 )
        lines( d.temp, col = 8, lwd = 1 )
      }
      
      index = which( fruit1$tree_id == tree )
      d.temp = density( z0[index], adjust = 3 )
      lines( d.temp, col = 2, lwd = 1 )
      
      legend( "topleft", lty = 1, col = c(2,8), bty = "n", 
              legend = c( paste0( "Tree selected (", tree, ")" ), "Other trees in this orchard" ) )
      
      sen1 = paste0( "Estimate: The average sugar content in this tree (", tree, ") is" )
      sen2 = paste0( "higher than", round( mean( z0 <= mean(z1) ) * 100 ), "% of all mandarins produced in ", length(u), " trees" ) 
      sen3 = paste0( "in this orchard recorded this week." )

      mtext( side = 1, line = 5, adj = 0.05, text = sen1 )
      mtext( side = 1, line = 6, adj = 0.05, text = sen2 )
      mtext( side = 1, line = 7, adj = 0.05, text = sen3 )
      
    } ) ### end of renderPlot
        
    ###
    ###### FRUIT SIZE
    ###
    
    output$Fruit2 = renderPlot( {
      
      par( mfrow = c( 1, 2 ) )
      par( mar = c( 5.1 * 1.75, 4.1, 4.1, 2.1 ) )
      
      ### INTER-ORCHARD (ORCHARD-LEVEL)
      
      u = sort( unique(fruit0$farm_id) )
      
      x1 = fruit1$size
      x0 = fruit0$size
      mu = mean( x0, na.rm = TRUE ); sigma = sd( x0, na.rm = TRUE )
      
      z0 = ( x0 - mu ) / sigma
      z1 = ( x1 - mu ) / sigma
      
      max.temp = rep( NA, length(u) )
      
      for (i in 1:length(u) ) {
        index = which( fruit0$farm_id == u[i] )
        max.temp[i] = max( density( z0[index], adjust = 3 )$y )
      }
      
      plot( 1, 1, col = "white", axes = FALSE, 
            xlim = c(-4,4), ylim = c( 0, max( max.temp, na.rm = TRUE ) * 1.2 ),
            xlab = "Fruit size (mm)", ylab = "",
            main = "Inter-orchard Distribution (KDE)" )
      
      axis( 1, -5:5, labels = formatC( -5:5 * sigma + mu, format = "f", digits = 1 ) )
      
      
      for (i in 1:length(u) ) {
        index = which( fruit0$farm_id == u[i] )
        d.temp = density( z0[index], adjust = 3 )
        lines( d.temp, col = 8, lwd = 1 )
      }
      
      index = which( fruit0$farm_id == orchard )
      d.temp = density( z0[index], adjust = 3 )
      lines( d.temp, col = 2, lwd = 1 )
      
      legend( "topleft", lty = 1, col = c(2,8), bty = "n", 
              legend = c( paste0( "Orchard selected (", orchard, ")" ), "Other orchards in Jeju Island" ) )

      sen1 = paste0( "Estimate: The average fruit size in this orchard (", orchard, ") is" )
      sen2 = paste0( "bigger than", round( mean( z0 <= mean(z1) ) * 100 ), "% of all mandarins produced in ", length(u), " orchards" ) 
      sen3 = paste0( "in Jeju Island recorded this week ." )
      
      mtext( side = 1, line = 5, adj = 0.05, text = sen1 )
      mtext( side = 1, line = 6, adj = 0.05, text = sen2 )
      mtext( side = 1, line = 7, adj = 0.05, text = sen3 )
      
      ### INTRA-ORCHARD (TREE-LEVEL)
      
      u = sort( unique(fruit1$tree_id) )
      
      x1 = fruit2$size
      x0 = fruit1$size
      mu = mean( x0, na.rm = TRUE ); sigma = sd( x0, na.rm = TRUE ) * 2
      
      z0 = ( x0 - mu ) / sigma
      z1 = ( x1 - mu ) / sigma
      
      max.temp = rep( NA, length(u) )
      
      for (i in 1:length(u) ) {
        index = which( fruit1$tree_id == u[i] )
        max.temp[i] = max( density( z0[index], adjust = 3 )$y )
      }
      
      plot( 1, 1, col = "white", axes = FALSE, 
            xlim = c(-4,4), ylim = c( 0, max( max.temp, na.rm = TRUE ) * 1.2 ),
            xlab = "Fruit size (mm)", ylab = "",
            main = "Intra-orchard Distribution (KDE)" )
      
      axis( 1, -5:5, labels = formatC( -5:5 * sigma + mu, format = "f", digits = 1 ) )
      
      for (i in 1:length(u) ) {
        index = which( fruit1$tree_id == u[i] )
        d.temp = density( z0[index], adjust = 3 )
        lines( d.temp, col = 8, lwd = 1 )
      }
      
      index = which( fruit1$tree_id == tree )
      d.temp = density( z0[index], adjust = 3 )
      lines( d.temp, col = 2, lwd = 1 )
      
      legend( "topleft", lty = 1, col = c(2,8), bty = "n", 
              legend = c( paste0( "Tree selected (", tree, ")" ), "Other trees in this orchard" ) )
      
      sen1 = paste0( "Estimate: The average fruit size in this tree (", tree, ") is" )
      sen2 = paste0( "bigger than", round( mean( z0 <= mean(z1) ) * 100 ), "% of all mandarins produced in ", length(u), " trees" ) 
      sen3 = paste0( "in this orchard recorded this week ." )

      mtext( side = 1, line = 5, adj = 0.05, text = sen1 )
      mtext( side = 1, line = 6, adj = 0.05, text = sen2 )
      mtext( side = 1, line = 7, adj = 0.05, text = sen3 )
    
    } ) ### end of renderPlot
  
    ###############
    ### HISTORY ###
    ###############
    
    ###
    ###### SUGAR CONTENT 
    ###
    
    output$History1 = renderPlot( {
      
      par( mfrow = c( 1, 2 ) )
      par( mar = c( 5.1, 4.1 * 1.5, 4.1, 2.1 ) )
      
      ### ORCHARD-LEVEL
      
      y = data0$brix
      time.index = factor( data0$monthweek )
      time.num = as.numeric(time.index)
      farm_id = data0$farm_id
      mu = mean( y, na.rm = TRUE )
      sigma = sd( y, na.rm = TRUE )
      z = ( y - mu ) / sigma
      u = sort( unique(farm_id) )
      m = length(u)
      
      plot( 1, 1, col = "white", axes = FALSE, 
            xlim = range(time.num), ylim = c(-4,4),
            xlab = "Month-Week", ylab = "",
            main = "Weekly Average Trend (Orchard-level)" )
      axis( 1, 0:max(time.num), c( "", as.character( sort( unique(time.index) ) ) ) )
      axis( 2, -5:5, labels = formatC( -5:5 * sigma + mu, format = "f", digits = 1 ), las = 2 )
      mtext( side = 2, adj = 0.5, line = 3.5, text = "Sugar Content (°Bx)" )
      
      for (i in 1:m ) {
        index = which( farm_id == u[i] )
        y.temp = by( z[index], time.num[index], mean, na.rm = TRUE )
        x.temp = as.numeric( names(y.temp) )
        y.temp = as.numeric(y.temp)
        lines( x.temp, y.temp, col = 8 )  
      }
      
      index = which( farm_id == orchard )
      y.temp = by( z[index], time.num[index], mean, na.rm = TRUE )
      x.temp = as.numeric( names(y.temp) )
      y.temp = as.numeric(y.temp)
      par( new = TRUE )
      plot( x.temp, y.temp, type = "b", pch = 19, col = 2,
            xlim = range(time.num), ylim = c(-4,4), axes = FALSE,
            xlab = "", ylab = "" )  
      
      legend( "bottomright", lty = 1, col = c(2,8), bty = "n", 
              legend = c( paste0( "Orchard selected (", orchard, ")" ), "Other orchards in Jeju Island" ) )
      
      ### TREE-LEVEL
      
      data.temp = subset( data0, farm_id == orchard )
      y = data.temp$brix
      time.index = factor( data.temp$monthweek )
      time.num = as.numeric(time.index)
      tree_id = data.temp$tree_id
      z = ( y - mu ) / sigma
      u = sort( unique(tree_id) )
      m = length(u)
      
      plot( 1, 1, col = "white", axes = FALSE, 
            xlim = range(time.num), ylim = c(-4,4),
            xlab = "Month-Week", ylab = "",
            main = "Weekly Average Trend (Tree-level)" )
      axis( 1, 0:max(time.num), c( "", as.character( sort( unique(time.index) ) ) ) )
      axis( 2, -5:5, labels = formatC( -5:5 * sigma + mu, format = "f", digits = 1 ), las = 2 )
      mtext( side = 2, adj = 0.5, line = 3.5, text = "Sugar Content (°Bx)" )
      
      for (i in 1:m ) {
        index = which( tree_id == u[i] )
        y.temp = by( z[index], time.num[index], mean, na.rm = TRUE )
        x.temp = as.numeric( names(y.temp) )
        y.temp = as.numeric(y.temp)
        lines( x.temp, y.temp, col = 8 )  
      }
      
      index = which( tree_id == tree )
      y.temp = by( z[index], time.num[index], mean, na.rm = TRUE )
      x.temp = as.numeric( names(y.temp) )
      y.temp = as.numeric(y.temp)
      par( new = TRUE )
      plot( x.temp, y.temp, type = "b", pch = 19, col = 2,
            xlim = range(time.num), ylim = c(-4,4), axes = FALSE,
            xlab = "", ylab = "" )  
      
      legend( "bottomright", lty = 1, col = c(2,8), bty = "n", 
              legend = c( paste0( "Tree selected (", tree, ")" ), "Other trees in this orchard" ) )
    
    } ) ### end of renderPlot
    
    ###
    ###### FRUIT SIZE
    ###
    
    output$History2 = renderPlot( {
      
      par( mfrow = c( 1, 2 ) )
      par( mar = c( 5.1, 4.1 * 1.5, 4.1, 2.1 ) )
      
      ### ORCHARD-LEVEL
      
      y = data0$size
      time.index = factor( data0$monthweek )
      time.num = as.numeric(time.index)
      farm_id = data0$farm_id
      mu = mean( y, na.rm = TRUE )
      sigma = sd( y, na.rm = TRUE )
      z = ( y - mu ) / sigma
      u = sort( unique(farm_id) )
      m = length(u)
      
      plot( 1, 1, col = "white", axes = FALSE, 
            xlim = range(time.num), ylim = c(-4,4),
            xlab = "Month-Week", ylab = "",
            main = "Weekly Average Trend (Orchard-level)" )
      axis( 1, 0:max(time.num), c( "", as.character( sort( unique(time.index) ) ) ) )
      axis( 2, -5:5, labels = formatC( -5:5 * sigma + mu, format = "f", digits = 1 ), las = 2 )
      mtext( side = 2, adj = 0.5, line = 3.5, text = "Fruit Size (mm)" )
      
      for (i in 1:m ) {
        index = which( farm_id == u[i] )
        y.temp = by( z[index], time.num[index], mean, na.rm = TRUE )
        x.temp = as.numeric( names(y.temp) )
        y.temp = as.numeric(y.temp)
        lines( x.temp, y.temp, col = 8 )  
      }
      
      index = which( farm_id == orchard )
      y.temp = by( z[index], time.num[index], mean, na.rm = TRUE )
      x.temp = as.numeric( names(y.temp) )
      y.temp = as.numeric(y.temp)
      par( new = TRUE )
      plot( x.temp, y.temp, type = "b", pch = 19, col = 2,
            xlim = range(time.num), ylim = c(-4,4), axes = FALSE,
            xlab = "", ylab = "" )  
      
      legend( "bottomright", lty = 1, col = c(2,8), bty = "n", 
              legend = c( paste0( "Orchard selected (", orchard, ")" ), "Other orchards in Jeju Island" ) )
      
      ### TREE-LEVEL
      
      data.temp = subset( data0, farm_id == orchard )
      y = data.temp$size
      time.index = factor( data.temp$monthweek )
      time.num = as.numeric(time.index)
      tree_id = data.temp$tree_id
      z = ( y - mu ) / sigma
      u = sort( unique(tree_id) )
      m = length(u)
      
      plot( 1, 1, col = "white", axes = FALSE, 
            xlim = range(time.num), ylim = c(-4,4),
            xlab = "Month-Week", ylab = "",
            main = "Weekly Average Trend (Tree-level)" )
      axis( 1, 0:max(time.num), c( "", as.character( sort( unique(time.index) ) ) ) )
      axis( 2, -5:5, labels = formatC( -5:5 * sigma + mu, format = "f", digits = 1 ), las = 2 )
      mtext( side = 2, adj = 0.5, line = 3.5, text = "Fruit Size (mm)" )
      
      for (i in 1:m ) {
        index = which( tree_id == u[i] )
        y.temp = by( z[index], time.num[index], mean, na.rm = TRUE )
        x.temp = as.numeric( names(y.temp) )
        y.temp = as.numeric(y.temp)
        lines( x.temp, y.temp, col = 8 )  
      }
      
      index = which( tree_id == tree )
      y.temp = by( z[index], time.num[index], mean, na.rm = TRUE )
      x.temp = as.numeric( names(y.temp) )
      y.temp = as.numeric(y.temp)
      par( new = TRUE )
      plot( x.temp, y.temp, type = "b", pch = 19, col = 2,
            xlim = range(time.num), ylim = c(-4,4), axes = FALSE,
            xlab = "", ylab = "" )  
      
      legend( "bottomright", lty = 1, col = c(2,8), bty = "n", 
              legend = c( paste0( "Tree selected (", tree, ")" ), "Other trees in this orchard" ) )
      
    } ) ### end of renderPlot
    
  } ) ### end of observeEvent (after selecting tree)

}

################
### shinyApp ###
################

shinyApp(ui, server)