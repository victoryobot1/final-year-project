install.packages('caret')
install.packages('e1071')

library(nnet)
library(caret)




#Read in dataset
traffic_raw <- read.csv("joint dataset.csv", header = TRUE, sep = ",")#[,c(2,3,4,5)]
traffic_csv <- traffic_raw[,c(1,3,7,9,11,13,15,17,18,25,26,28)]
traffic_csv <- na.omit(traffic_csv)

traffic_csv$ï..Description <- as.factor(traffic_csv$ï..Description)
traffic_csv$Month <- as.factor(traffic_csv$Month)
traffic_csv$Hour <- as.factor(traffic_csv$Hour)
traffic_csv$DayofWeek <- as.factor(traffic_csv$DayofWeek)
traffic_csv$TimeofDay <- as.factor(traffic_csv$TimeofDay)
traffic_csv$Temperature <- as.factor(traffic_csv$Temperature)
traffic_csv$Weather <- as.factor(traffic_csv$Weather)

table(traffic_csv$LevelofService)
 
set.seed(1234)
traffic_csv <- traffic_csv[sample(nrow(traffic_csv)),]



#####---------------Cross validation-------------------#######

#setting percentage of train/test split 
split <- floor(nrow(traffic_csv)*0.75)
traffic_csvTrain <- traffic_csv[0:split,]
traffic_csvTest <- traffic_csv[(split+1):nrow(traffic_csv),]
table(traffic_csvTrain$LevelofService)

#create model
losmodel <- multinom(LevelofService~., data = traffic_csvTrain,
                    maxit = 500, trace=T)
print(losmodel)

#topModels <- varImp(losmodel)
#topModels$Variables <- row.names(topModels)
#topModels <- topModels[order(-topModels$Overall),]


#k-fold cross validation
k<-10

#holder for comparison results cross validation
compare_lists <- NULL

#Loop to run nn k-times
for(i in 1:k){

  #Compute Predictions
  predict_testNN <- predict(losmodel, type="probs", newdata=traffic_csvTest)
  head(predict_testNN)
    
  nrows = nrow(predict_testNN)
  
  prediction_list <- NULL
  original_results <- NULL
  for(j in 1:nrows){
    prediction_list[[j]]<-which.max(predict_testNN[j,])
    original_results[[j]]<-traffic_csvTest[j,12]
    j <- j+1
  }
  
  compare_lists[[i]] <- mean(prediction_list == original_results)
  i<-i+1
}

accuracy <- mean(compare_lists)
print(accuracy)
