library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(graphics)
library(BH)




shinyServer(function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  map <- createLeafletMap(session, "map")

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(allData[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(allData,
      Start_latitude >= latRng[1] & Start_latitude <= latRng[2] &
        Start_longtitude >= lngRng[1] & Start_longtitude <= lngRng[2])
  })

  # Precalculate the breaks we'll need for the two histograms
#  centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks

#  output$histCentile <- renderPlot({
#    # If no zipcodes are in view, don't plot
#    if (nrow(zipsInBounds()) == 0)
#      return(NULL)

#    hist(zipsInBounds()$centile,
#      breaks = centileBreaks,
#      main = "SuperZIP score (visible zips)",
#      xlab = "Percentile",
#      xlim = range(allzips$centile),
#      col = '#00DD00',
#      border = 'white')
#  })

#  output$scatterCollegeIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
#    if (nrow(zipsInBounds()) == 0)
#      return(NULL)

#    print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
#  })

  # session$onFlushed is necessary to work around a bug in the Shiny/Leaflet
  # integration; without it, the addCircle commands arrive in the browser
  # before the map is created.
  session$onFlushed(once=TRUE, function() {
    paintObs <- observe({
      LevelBy <- input$Level
      LossBy <- input$Loss
      Date <- input$Date
      s_dateBy <- Date[1]
      e_dateBy <- Date[2]
      s_yearBy <- as.numeric(format(s_dateBy, format = "%Y"))
      e_yearBy <- as.numeric(format(e_dateBy, format = "%Y"))
      s_monthBy <- as.numeric(format(s_dateBy, format = "%m"))
      e_monthBy <- as.numeric(format(e_dateBy, format = "%m"))
      s_dayBy <- as.numeric(format(s_dateBy, format = "%d"))
      e_dayBy <- as.numeric(format(e_dateBy, format = "%d"))
      stateBy <- input$States

      partData <- subset(allData, Year >= s_yearBy & Year <= e_yearBy)
      partData2 <- subset(allData, Year == s_yearBy & Month < s_monthBy)
      partData3 <- subset(allData, Year == e_yearBy & Month > e_monthBy)
      partData4 <- subset(allData, Year == s_yearBy & Month == s_monthBy & Day < s_dayBy)
      partData5 <- subset(allData, Year == e_yearBy & Month == e_monthBy & Day > e_dayBy)
      partData <- setdiff(partData, partData2)
      partData <- setdiff(partData, partData3)
      partData <- setdiff(partData, partData4)
      partData <- setdiff(partData, partData5)
      if (!is.null(LossBy)){
        partData <- filter(partData, Loss %in% LossBy)
      }
      if (!is.null(LevelBy)){
        partData <- filter(partData, F.scale %in% LevelBy)
      }
      if (!is.null(stateBy)){
        partData <- filter(partData, State %in% stateBy)
      }

#      colors <- colors[match(zipdata$zipcode, allzips$zipcode)]

      # Clear existing circles before drawing
      sub_color = as.character(unlist(partData$level_color))
      map$clearShapes()
      # Draw in batches of 1000; makes the app feel a bit more responsive

      if (length(partData[,1]!=0)){
        chunksize <- 1000
        for (f in seq.int(1, nrow(partData), chunksize)) {
            to <- min(nrow(partData), f + chunksize - 1)
            zipchunk <- partData[f:to,]
        # Bug in Shiny causes this to error out when user closes browser
        # before we get here
            try(
            map$addCircle(
                zipchunk$Start_latitude, zipchunk$Start_longtitude,
                (zipchunk$Loss_new / 10) * 30000,
                zipchunk$Order,
                list(stroke=FALSE, fill=TRUE, fillOpacity=0.4),
                list(color = sub_color[f:to])
            )
            )
        }
      }


      output$hisLevel <- renderPlot({
        if (length(partData[,1])==0){
            return()
        }
        levelL  <- c(-1,0,1,2,3,4,5)
        name_level <- c("unkn.", "0", "1", "2", "3", "4","5")
        level_list = factor(partData$F_scale_new, levels = levelL, labels = name_level)
        subData1 = table(level_list)
        barplot(subData1, main = "Level of Tornados", xlab = "Level", ylab = "count", ylim = range(0,(max(subData1)*1.3)), col = '#00DD00', space = 0,)

      })

      output$hisLoss <- renderPlot({
        if (length(partData[,1])==0){
            sprintf("No tornado occurs in the selected range")
            return()
        }
        lossL  <- c(0,1,2,3,4,5,6,7,8,9)
        name_loss <- c(
            "unknown",
            "<$50",
            "$50-$500",
            "$500-$5,000",
            "$5,000-$50,000",
            "$50,000-$500,000",
            "$500,000-$5000,000",
            "$5000,000-$5,000,000",
            "$50,000,000-$500,000,000",
            "$5000,000,000"
        )
        loss_list = factor(partData$Loss_1996, levels = lossL, labels = name_loss)
        subData2 = table(loss_list)
        barplot(subData2, main = "Facility Loss caused by Tornados", xlab = "Loss", ylab = "count", ylim = range(0,(max(subData2)*1.3)), space = 0, beside = TRUE, col = heat.colors(10)[10:1], axisnames = FALSE)
        legend("topleft", name_loss, cex = 0.6, bty="n", fill = heat.colors(10)[10:1])

      })

      output$hisTotal <- renderText({
        if (length(partData[,1])==0){
            "No tornado occured in the selected range"
        }
        else{
            sprintf("%s tornados occured in the selected range.\r\nMaxium level is %s, and minium level is %s" ,nrow(partData),max(partData$F.scale), min(partData[partData$F_scale>=0,]$F.scale))
        }

      })















    })

    # TIL this is necessary in order to prevent the observer from
    # attempting to write to the websocket after the session is gone.
    session$onSessionEnded(paintObs$suspend)
  })

  # Show a popup at the given location
  showZipcodePopup <- function(Order, lat, lng) {
    selectedZip <- allData[allData$Order == Order,]


    content <- as.character(tagList(
      tags$h4("Level:", if(selectedZip$F.scale == -9){"Unknown"}else{as.integer(selectedZip$F.scale)}),
      tags$strong(HTML(sprintf("%s-%s-%s",
        selectedZip$Year, selectedZip$Month, selectedZip$Day
      ))), tags$br(),
      sprintf("Latitude: %s", selectedZip$Start_latitude), tags$br(),
      sprintf("Longtitude: %s", selectedZip$Start_longtitude), tags$br()
#      sprintf("Estimated Property Loss: %s", selectedZip$loss_label), tags$br(),
#      sprintf("Estimated Crop Loss: %s", if(selectedZip$C_loss<0.0005){"<$500"} else{dollar(selectedZip$C_loss*1000000)})
    ))
    map$showPopup(lat, lng, content)
  }

  # When map is clicked, show a popup with city info
  clickObs <- observe({
    map$clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()


    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })

  session$onSessionEnded(clickObs$suspend)


  ## Data Explorer ###########################################

#  observe({
#    cities <- if (is.null(input$states)) character(0) else {
#      filter(allData, State %in% input$states) %>%
#        `$`('City') %>%
#        unique() %>%
#        sort()
#    }
#    stillSelected <- isolate(input$cities[input$cities %in% cities])
#    updateSelectInput(session, "cities", choices = cities,
#      selected = stillSelected)
#  })

#  observe({
#    zipcodes <- if (is.null(input$states)) character(0) else {
#      cleantable %>%
#        filter(State %in% input$states,
#          is.null(input$cities) | City %in% input$cities) %>%
#        `$`('Zipcode') %>%
#        unique() %>%
#        sort()
#    }
#    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
#    updateSelectInput(session, "zipcodes", choices = zipcodes,
#      selected = stillSelected)
#  })
#
#  observe({
#    if (is.null(input$goto))
#      return()
#    isolate({
#      map$clearPopups()
#      dist <- 0.5
#      zip <- input$goto$zip
#      lat <- input$goto$lat
#      lng <- input$goto$lng
#      showZipcodePopup(zip, lat, lng)
#      map$fitBounds(lat - dist, lng - dist,
#        lat + dist, lng + dist)
#    })
#  })

#  output$ziptable <- renderDataTable({
#    cleantable %>%
#      filter(
#        Score >= input$minScore,
#        Score <= input$maxScore,
#        is.null(input$states) | State %in% input$states,
#        is.null(input$cities) | City %in% input$cities,
#        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
#      ) %>%
#      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
#  })
})
