# Define server logic to summarize and view selected dataset ----
library(shiny)
library(shinydashboard)
library(nnet)
library(DT)


server <- function(input, output) {
  load('losmodel.rda')
  traffic_raw <- read.csv("Joint dataset.csv", header = TRUE, sep = ",")
  traffic_csv <- traffic_raw[,c(1,3,7,9,11,15,17,18,25,26,28)]
  traffic_csv_cleaned <- read.csv("Joint dataset - Cleaned.csv", header = TRUE, sep = ",")[,c(1,3,7,9,11,13,15,17,18,25,26,28)]
  
  observeEvent(input$submit2,{
    output$prediction <- renderText({
      input_data = as.data.frame(cbind("Temperature"=input$Temperature,"Weather"=input$Weather,
                                       "LengthofDetectionkm"=input$LengthofDetectionkm, 
                                       "Month"=input$Month, "DayofWeek" = input$DayofWeek, "Hour"=input$Hour,
                                       "DetectedDevices"=input$DetectedDevices,
                                       "Speedkmh"=input$Speedkmh, "ï..Description"=input$ï..Description,
                                       "TotalTravelTimeseconds"=3600*input$LengthofDetectionkm/input$Speedkmh))
      print(input_data)
      
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
      
      #tansforming Description variable
      if (input_data$ï..Description == "Northbound (Traffic toward Berger, Ikeja (Mainland))"){
        input_data$ï..Description <- "Northbound"
      }else{
        input_data$ï..Description <- "Southbound"
      }
      
      print(input_data)
      
      input_data$ï..Description <- as.factor(input_data$ï..Description)
      input_data$Month <- as.factor(input_data$Month)
      input_data$Hour <- as.factor(input_data$Hour)
      input_data$DayofWeek <- as.factor(input_data$DayofWeek)
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
  })

  output$trafficData = DT::renderDataTable({
    traffic_raw
  })
  observeEvent(input$submit, {
    #listwise deletion
    print('button clicked')
    #Setting categorical variables in the dataset as factor.
    traffic_csv_cleaned$Direction <- as.factor(traffic_csv_cleaned$Direction)
    traffic_csv_cleaned$Month <- as.factor(traffic_csv_cleaned$Month)
    traffic_csv_cleaned$Hour <- as.factor(traffic_csv_cleaned$Hour)
    traffic_csv_cleaned$DayofWeek <- as.factor(traffic_csv_cleaned$DayofWeek)
    traffic_csv_cleaned$Weather <- as.factor(traffic_csv_cleaned$Weather)
    print('done factors')
    table(traffic_csv_cleaned$LevelofService)
    
    #Set seed for reproducability in results  
    set.seed(123)
    
    #Shuffle the data
    traffic_csv_cleaned <- traffic_csv_cleaned[sample(nrow(traffic_csv_cleaned)),]
    print('done shuffle')
    #####---------------Cross validation-------------------#######
    
    #setting percentage of train/test split 
    split <- floor(nrow(traffic_csv_cleaned)*input$ratio)
    traffic_csv_cleanedTrain <- traffic_csv_cleaned[0:split,]
    traffic_csv_cleanedTest <- traffic_csv_cleaned[(split+1):nrow(traffic_csv_cleaned),]
    table(traffic_csv_cleanedTrain$LevelofService)
    
    print('done split')
    
    #create model
    losmodel_edit <- multinom(LevelofService~., data = traffic_csv_cleanedTrain,
                              maxit = 100, trace=T)
    print('done model')
    
    #holder for comparison results cross validation
    compare_lists <- NULL
    
    #Compute Predictions
    predict_testNN <- predict(losmodel_edit, type="probs", newdata=traffic_csv_cleanedTest)
    head(predict_testNN)
    
    nrows = nrow(predict_testNN)
    
    prediction_list <- NULL
    original_results <- NULL
    for(j in 1:nrows){
      prediction_list[[j]]<-which.max(predict_testNN[j,])
      original_results[[j]]<-traffic_csv_cleanedTest[j,12]
      j <- j+1
    }
    
    #confusion matrix
    conf_matrix <- table(original_results,prediction_list)
    colnames(conf_matrix) <- c("A","B","C","D","E","F")
    rownames(conf_matrix) <- c("A","B","C","D","E","F")
    conf_matrix
    
    output$accuracy <- renderText({
      compare_lists <- mean(prediction_list == original_results)
      print(prediction_list)
      
      accuracy <- mean(compare_lists)
      print(accuracy)
    })
    output$precision <- renderTable({
      precision <- data.frame(matrix(ncol=6,nrow=1))
      colnames(precision) <- c("A","B","C","D","E","F")
      for(i in 1:nrow(conf_matrix)){
        precision[1,i]=conf_matrix[i,i]/sum(conf_matrix[,i])
      }
      precision
    })
    output$recall <- renderTable({
      recall <- data.frame(matrix(ncol=6,nrow=1))
      colnames(recall) <- c("A","B","C","D","E","F")
      for(i in 1:nrow(conf_matrix)){
        recall[1,i]=conf_matrix[i,i]/sum(conf_matrix[i,])
      }
      recall
    })
    output$specificity <- renderTable({
      specificity <- data.frame(matrix(ncol=6,nrow=1))
      colnames(specificity) <- c("A","B","C","D","E","F")
      for(i in 1:nrow(conf_matrix)){
        specificity[1,i]=sum(conf_matrix[-i,-i])/(sum(conf_matrix[,i])-conf_matrix[i,i]+sum(conf_matrix[-i,-i]))
      }
      specificity
    })
    
    output$plot <- renderPlot({
      plot(prediction_list,type = "o",col = "red", xlab = "Case", ylab = "LOS (A=1...F=6)", 
           main = "Actual (red) vs Predicted (blue)")
      
      lines(original_results, type = "o", col = "blue")
      })
  })
  
}
