library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(graphics)

# Main Body

# Choices for drop-downs
lossLevels <- c(0,1,2,3,4,5,6,7,8,9)
lossVars <- c(
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
levelLevels <- c(-9,0,1,2,3,4,5)
levelVars <- c("unknow", "0", "1", "2", "3", "4", "5")

Vars <- c(
    "Total Number" = "Total",
    "Level Distribution" = "Level_d",
    "Loss Distribution" = "Loss_d",
    "Compare States" = "Comp_d",
    "Zipcode Exlore" = "Zip_d"
)



shinyUI(navbarPage("Flood in U.S", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletMap("map", width="100%", height="100%",
        initialTileLayer = "//{s}.tiles.mapbox.com/v3/kayshen777.jog7f9ng/{z}/{x}/{y}.png",
#        "http://api.titles.mapbox.com/v4/kayshen777.jog7f9ng/{x}/{y}/{z}.png?access_token=pk.eyJ1Ijoia2F5c2hlbjc3NyIsImEiOiJEZzMtVnJZIn0.0WZvXIPztj9vsS4yJjQaCw",
        initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
        options=list(
          center = c(37.45, -93.85),
          zoom = 4,
          maxBounds = list(list(15.961329,-129.92981), list(52.908902,-56.80481)) # Show US only
        )
      ),

      absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
        top = 130, left = 280, right = "auto", bottom = "auto",
        width = 330, height = "auto",

        h2("Flood Explorer"),

#        dateRangeInput("Date", "Date Range", start = "1950-01-01", end = "2013-12-31", min = "1950-01-01", max = "2013-12-31", format = "yyyy-mm-dd"),
        dateRangeInput("Date", "Date Range", start = "1985-02-22", end = "2014-10-24", min = "1985-02-22", max = "2014-10-24", format = "yyyy-mm-dd"),
        selectInput("States", "States", c("All States"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE),
        selectInput("Level", "Level", c("All Level" = "", structure(levelLevels, names = levelVars)), multiple=TRUE),
        selectInput("Loss", "Loss", c("All Loss" = "", structure(lossLevels, names = lossVars)), multiple=TRUE)

#        selectInput("Summary", "Summary", Vars, selected = "Total")
#        conditionalPanel("input.Summary == 'Total'",
#          plotOutput("hisTotal", height = 300)
#        ),
#        conditionalPanel("input.Summary == 'Level_d'",
#          plotOutput("hisLevel", height = 300)
#        ),
#        conditionalPanel("input.Summary == 'Loss_d'",
#          plotOutput("hisLoss", height = 300)
#        )


#        plotOutput("histCentile", height = 200),
#        plotOutput("scatterCollegeIncome", height = 250)
      ),

      absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
        top = 130, left = "auto", right = 0, bottom = "auto",
        width = 330, height = "auto",

        h2("Summary"),

        selectInput("Summary", "Summary", Vars, selected = "Total"),
        conditionalPanel("input.Summary == 'Total'",
          textOutput("hisTotal")
        ),
        conditionalPanel("input.Summary == 'Level_d'",
          plotOutput("hisLevel", height = 300)
        ),
        conditionalPanel("input.Summary == 'Loss_d'",
          plotOutput("hisLoss", height = 300)
        ),
#        conditionalPanel("input.Summary == 'Zip_d'",
#          selectInput("Zipcode", "Zipcode", c("All Zipcode" = "", structure()))
#        ),
        conditionalPanel("input.Summary == 'Comp_d'",
          column(3,
            selectInput("select_State1", "First State", c("All States"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=FALSE)
          ),
          column(3,
            selectInput("select_State2", "Second State", c("All States"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=FALSE)
          ),

          plotOutput("hisComp", height = 300)
        )

      ),

      tags$div(id="cite",
        'Data is pulled from ', tags$em('the Storm Prediction Center\'s (SPC) historical tornado data file, 1950–2013'), ' '
      )
    )
  ),

#  tabPanel("Data explorer",
#    fluidRow(
#      column(3,
#        selectInput("State", "State", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
#      ),
#      column(3,
#        conditionalPanel("input.states",
#          selectInput("Longtitude", "Longtitude", c("All Longtitude"=""), multiple=TRUE)
#        )
#      ),
#      column(3,
#        conditionalPanel("input.states",
#          selectInput("Latitude", "Latitude", c("All Latitude"=""), multiple=TRUE)
#        )
#      ),
#      column(3,
#        conditionalPanel("input.states",
#          selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
#        )
#      )
#    ),
#    fluidRow(
#      column(1,
#        numericInput("minScore", "Min score", min=0, max=100, value=0)
#      ),
#      column(1,
#        numericInput("maxScore", "Max score", min=0, max=100, value=100)
#      )
#    ),
#    hr(),
#    dataTableOutput("ziptable")
#  ),

  conditionalPanel("false", icon("crosshair"))
))
