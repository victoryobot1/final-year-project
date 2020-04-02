install.packages('caret')
install.packages('e1071')
install.packages('TestDataImputation')

library(nnet)
library(caret)
library(TestDataImputation)


#Read in dataset
traffic_raw <- read.csv("Joint dataset.csv", header = TRUE, sep = ",")
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
#Saving the model for use in Shiny app
save(losmodel, file = "losmodel.rda")


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


#####------------------Evaluation------------######
#classification accuracy
overall_accuracy <- mean(prediction_list == original_results)
print(overall_accuracy)

#confusion matrix
conf_matrix <- table(original_results,prediction_list)
colnames(conf_matrix) <- c("A","B","C","D","E","F")
rownames(conf_matrix) <- c("A","B","C","D","E","F")
conf_matrix

#precision for each Level of Service
precision <- data.frame(matrix(ncol=6,nrow=1))
colnames(precision) <- c("A","B","C","D","E","F")
for(i in 1:nrow(conf_matrix)){
  precision[1,i]=conf_matrix[i,i]/sum(conf_matrix[,i])
}
precision

#recall for each Level of Service
recall <- data.frame(matrix(ncol=6,nrow=1))
colnames(recall) <- c("A","B","C","D","E","F")
for(i in 1:nrow(conf_matrix)){
  recall[1,i]=conf_matrix[i,i]/sum(conf_matrix[i,])
}
recall

#specificity for each Level of Service
specificity <- data.frame(matrix(ncol=6,nrow=1))
colnames(specificity) <- c("A","B","C","D","E","F")
for(i in 1:nrow(conf_matrix)){
  specificity[1,i]=sum(conf_matrix[-i,-i])/(sum(conf_matrix[,i])-conf_matrix[i,i]+sum(conf_matrix[-i,-i]))
}
specificity


