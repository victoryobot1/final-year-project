install.packages('caret')
install.packages('e1071')
install.packages('TestDataImputation')

library(nnet)
library(caret)
library(TestDataImputation)


#Read in dataset
traffic_raw <- read.csv("joint dataset.csv", header = TRUE, sep = ",")#[,c(2,3,4,5)]
traffic_csv <- traffic_raw[,c(1,3,7,9,11,13,15,17,18,25,26,28)]

#Applying listwise deletion. (IMPUTATION)
traffic_csv <- Listwise(traffic_csv, Mvalue="")

#Setting cattegorical variables in the dataset as factor.
traffic_csv$ï..Description <- as.factor(traffic_csv$ï..Description)
traffic_csv$Month <- as.factor(traffic_csv$Month)
traffic_csv$Hour <- as.factor(traffic_csv$Hour)
traffic_csv$DayofWeek <- as.factor(traffic_csv$DayofWeek)
traffic_csv$TimeofDay <- as.factor(traffic_csv$TimeofDay)
traffic_csv$Weather <- as.factor(traffic_csv$Weather)

table(traffic_csv$LevelofService)

#Set seed for reproducability in results  
set.seed(1234)

#Shuffle the data
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

#Finding the most important variables
mostImportantVariables <- varImp(losmodel)
mostImportantVariables$Variables <- row.names(mostImportantVariables)
mostImportantVariables <- mostImportantVariables[order(-mostImportantVariables$Overall),]
print(head(mostImportantVariables))


#holder for comparison results cross validation
compare_lists <- NULL


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

compare_lists <- mean(prediction_list == original_results)


accuracy <- mean(compare_lists)
print(accuracy)


#Saving the model for use in Shiny app
save(losmodel, file = "losmodel.rda")
