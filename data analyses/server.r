# Define server logic to summarize and view selected dataset ----
library(shiny)
library(shinydashboard)
library(nnet)
library(caret)



server = function(input, output) {
  load('losmodel.rda')
  
  output$prediction <- renderTable({
    input_data = as.data.frame(cbind("Temperature"=input$Temperature,"Weather"=input$Weather,
                   "LengthofDetectionkm"=input$LengthofDetectionkm, 
                   "Month"=input$Month, "DayofWeek" = input$DayofWeek, "Hour"=input$Hour,
                   "DetectedDevices"=input$DetectedDevices,
                   "Speedkmh"=input$Speedkmh, "ï..Description"=input$ï..Description,
                   "TotalTravelTimeseconds"=3600*input$LengthofDetectionkm/input$Speedkmh))
    print(input_data)
    
    if (input_data$Hour %in% c(0,1,2,3)){
      input_data$TimeofDay <- "Late Night"
    }
    if (input_data$Hour %in% c(4,5,6,7,8)){
      input_data$TimeofDay <- "Early Morning"
    }
    if (input_data$Hour %in% c(9,10,11)){
      input_data$TimeofDay <- "Late Morning"
    }
    if (input_data$Hour %in% c(12,13,14)){
      input_data$TimeofDay <- "Early Afternoon"
    }
    if (input_data$Hour %in% c(15,16,17)){
      input_data$TimeofDay <- "Late Afternoon"
    }
    if (input_data$Hour %in% c(18,19,20)){
      input_data$TimeofDay <- "Evening"
    }
    if (input_data$Hour %in% c(21,22,23)){
      input_data$TimeofDay <- "Late Evening"
    }
    
    
    #input_data$TotalTravelTimeseconds <- 70#input$LengthofDetectionkm/input$Speedkmh
    if (input_data$DayofWeek == "Sunday"){
      input_data$DayofWeek <- 1
    }
    if (input_data$DayofWeek == "Monday"){
      input_data$DayofWeek <- 2
    }
    if (input_data$DayofWeek == "Tuseday"){
      input_data$DayofWeek <- 3
    }
    if (input_data$DayofWeek == "Wednesday"){
      input_data$DayofWeek <- 4
    }
    if (input_data$DayofWeek == "Thursday"){
      input_data$DayofWeek <- 5
    }
    if (input_data$DayofWeek == "Friday"){
      input_data$DayofWeek <- 6
    }
    if (input_data$DayofWeek == "Saturday"){
      input_data$DayofWeek <- 7
    }
    
    
    print(input_data)
    
    input_data$ï..Description <- as.factor(input_data$ï..Description)
    input_data$Month <- as.factor(input_data$Month)
    input_data$Hour <- as.factor(input_data$Hour)
    input_data$DayofWeek <- as.factor(input_data$DayofWeek)
    input_data$TimeofDay <- as.factor(input_data$TimeofDay)
    input_data$Weather <- as.factor(input_data$Weather)
    input_data$LengthofDetectionkm <- as.numeric(input_data$LengthofDetectionkm)
    input_data$DetectedDevices <- as.numeric(input_data$DetectedDevices)
    input_data$Speedkmh <- as.numeric(input_data$Speedkmh)
    input_data$TotalTravelTimeseconds <- as.numeric(input_data$TotalTravelTimeseconds)
    input_data$Temperature <- as.numeric(input_data$Temperature)
    
  
    predict_testNN <- predict(losmodel, type="probs", newdata=input_data)
    
    los <- which.max(predict_testNN)
    print(los)
})
  output$histogram <- renderPlot({
    hist(faithful$eruptions, breaks = input$bins)
  })
  
}