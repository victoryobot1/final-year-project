# Define server logic to summarize and view selected dataset ----
library(shiny)
library(shinydashboard)
library(nnet)
library(caret)
library(DT)
library(TestDataImputation)
library(ggplot2)



server <- function(input, output) {
  load('losmodel.rda')
  traffic_raw <- read.csv("joint dataset.csv", header = TRUE, sep = ",")
  traffic_csv <- traffic_raw[,c(1,3,7,9,11,13,15,17,18,25,26,28)]
  
  
  output$prediction <- renderText({
    input_data = as.data.frame(cbind("Temperature"=input$Temperature,"Weather"=input$Weather,
                   "LengthofDetectionkm"=input$LengthofDetectionkm, 
                   "Month"=input$Month, "DayofWeek" = input$DayofWeek, "Hour"=input$Hour,
                   "DetectedDevices"=input$DetectedDevices,
                   "Speedkmh"=input$Speedkmh, "ï..Description"=input$ï..Description,
                   "TotalTravelTimeseconds"=3600*input$LengthofDetectionkm/input$Speedkmh))
    print(input_data)
    
    #Using Hour  value to derive TimeofDay variable
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
    
    #Transforming DayofWeek value to numeric type for the model
    if (input_data$DayofWeek == "Sunday"){
      input_data$DayofWeek <- 1
    }
    if (input_data$DayofWeek == "Monday"){
      input_data$DayofWeek <- 2
    }
    if (input_data$DayofWeek == "Tuesday"){
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
    
    #Transforming Month value to numeric for the model
    if (input_data$Month == "January"){
      input_data$Month <- 1
    }
    if (input_data$Month == "June"){
      input_data$Month <- 6
    }
    if (input_data$Month == "July"){
      input_data$Month <- 7
    }
    if (input_data$Month == "August"){
      input_data$Month <- 8
    }
    if (input_data$Month == "September"){
      input_data$Month <- 9
    }
    if (input_data$Month == "October"){
      input_data$Month <- 10
    }
    if (input_data$Month == "November"){
      input_data$Month <- 11
    }
    if (input_data$Month == "December"){
      input_data$Month <- 12
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
    if (los == 1){
      los <- "A"
    }
    if (los == 2){
      los <- "B"
    }
    if (los == 3){
      los <- "C"
    }
    if (los == 4){
      los <- "D"
    }
    if (los == 5){
      los <- "E"
    }
    if (los == 6){
      los <- "F"
    }
    print(los)
})
  output$trafficData = DT::renderDataTable({
    traffic_csv
  })
  observeEvent(input$submit, {
    traffic_csv_imput <- Listwise(traffic_csv, Mvalue="")
    
    #Setting cattegorical variables in the dataset as factor.
    traffic_csv_imput$ï..Description <- as.factor(traffic_csv_imput$ï..Description)
    traffic_csv_imput$Month <- as.factor(traffic_csv_imput$Month)
    traffic_csv_imput$Hour <- as.factor(traffic_csv_imput$Hour)
    traffic_csv_imput$DayofWeek <- as.factor(traffic_csv_imput$DayofWeek)
    traffic_csv_imput$TimeofDay <- as.factor(traffic_csv_imput$TimeofDay)
    traffic_csv_imput$Weather <- as.factor(traffic_csv_imput$Weather)
    
    table(traffic_csv_imput$LevelofService)
    
    #Set seed for reproducability in results  
    set.seed(1234)
    
    #Shuffle the data
    traffic_csv_imput <- traffic_csv_imput[sample(nrow(traffic_csv_imput)),]
    
    #####---------------Cross validation-------------------#######
    
    #setting percentage of train/test split 
    split <- floor(nrow(traffic_csv_imput)*input$ratio)
    traffic_csv_imputTrain <- traffic_csv_imput[0:split,]
    traffic_csv_imputTest <- traffic_csv_imput[(split+1):nrow(traffic_csv_imput),]
    table(traffic_csv_imputTrain$LevelofService)
    
    #create model
    losmodel_edit <- multinom(LevelofService~., data = traffic_csv_imputTrain,
                              maxit = 500, trace=T)
    #holder for comparison results cross validation
    compare_lists <- NULL
    
    #Compute Predictions
    predict_testNN <- predict(losmodel, type="probs", newdata=traffic_csv_imputTest)
    head(predict_testNN)
    
    nrows = nrow(predict_testNN)
    
    prediction_list <- NULL
    original_results <- NULL
    for(j in 1:nrows){
      prediction_list[[j]]<-which.max(predict_testNN[j,])
      original_results[[j]]<-traffic_csv_imputTest[j,12]
      j <- j+1
    }
    
    output$accuracy <- renderText({
      compare_lists <- mean(prediction_list == original_results)
      print(prediction_list)
      
      accuracy <- mean(compare_lists)
      print(accuracy)
    })
    
    output$plot <- renderPlot({

      plot(prediction_list,type = "o",col = "red", xlab = "Case", ylab = "LOS (A=1...F=6)", 
           main = "Actual (red) vs Predicted (blue)")
      
      lines(original_results, type = "o", col = "blue")
      })
  })
  
}