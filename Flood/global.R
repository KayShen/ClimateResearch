library(shiny)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(graphics)

#setwd("/Users/kshen4/Dropbox/Columbia/Climate Research/R_shinny/Flood")
allData <- read.csv("data/flood.csv", header = T)
allData$Start_latitude <- jitter(allData$Start_latitude)
allData$Start_longitude <- jitter(allData$Start_longitude)
allData$End_latitude <- jitter(allData$End_latitude)
allData$End_longitude <- jitter(allData$End_longitude)
F_scale_new = replace(allData$F.scale, allData$F.scale<0, -1)
allData = cbind(allData, F_scale_new)
Loss_1996 = allData$Loss
allData = cbind(allData, Loss_1996)


Loss_new = allData$Loss_1996+1
allData = cbind(allData, Loss_new)
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
Color_for_level <- c("#FEC44F","#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026")
level_color = factor(allData$F.scale, levels = levelLevels, labels = Color_for_level)

#cleantable <- allData %>%
#  select(
#    State = state.x,
#    Year = Year,
#    Month = Month,
#    Day = Day,
#    Level = F.scale,
#    Injuries = Injuries,
#    Fatalities = Fatalities,
#    Property_loss = loss.label,
#    crop_loss = C_loss,
#    Latitude = Start_latitude,
#    Longtitude = Start_longtitude,
#    Length = Length,
#    Width = Width
#  )


loss_label = factor(allData$Loss_1996, levels = lossLevels, labels = lossVars)
allData = cbind(allData, loss_label)
allData = cbind(allData, level_color)



#runApp("/Users/kshen4/Dropbox/Columbia/Climate Research/R_shinny/Flood")

