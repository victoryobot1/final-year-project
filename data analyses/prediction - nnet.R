install.packages('caret')
install.packages('e1071')

library(nnet)
library(caret)




#Read in dataset
traffic_raw <- read.csv("3rdMainland Bridge_NB - prediction dataset - copy.csv", header = TRUE, sep = ",")[,c(2,3,4,5)]
traffic_csv <- traffic_raw#[c(1:100),]

traffic_csv$Day.of.Week <- as.factor(traffic_csv$Day.of.Week)
traffic_csv$Hour <- as.factor(traffic_csv$Hour)
traffic_csv$Weather <- as.factor(traffic_csv$Weather)
traffic_csv$Level.of.Service <- as.factor(traffic_csv$Level.of.Service)

table(traffic_csv$Level.of.Service)
 
set.seed(1234)
traffic_csv <- traffic_csv[sample(nrow(traffic_csv)),]



#####---------------Cross validation-------------------#######

#setting percentage of train/test split 
split <- floor(nrow(traffic_csv)*0.75)
traffic_csvTrain <- traffic_csv[0:split,]
traffic_csvTest <- traffic_csv[(split+1):nrow(traffic_csv),]
table(traffic_csvTrain$Level.of.Service)

#create model
losmodel <- multinom(Level.of.Service~., data = traffic_csvTrain,
                    maxit = 500, trace=T)
print(losmodel)

topModels <- varImp(losmodel)
topModels$Variables <- row.names(topModels)
topModels <- topModels[order(-topModels$Overall),]


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
    original_results[[j]]<-traffic_csvTest[j,4]
    j <- j+1
  }
  
  compare_lists[[i]] <- mean(prediction_list == original_results)
  i<-i+1
}

accuracy <- mean(compare_lists)
print(accuracy)
