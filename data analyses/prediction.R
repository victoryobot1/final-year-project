#install.packages('neuralnet')
library(neuralnet)
library(nnet)

#read in dataset
traffic_raw = read.csv("3rdMainland Bridge_NB - prediction dataset - copy.csv", header = TRUE, sep = ",")[,c(2,3,4,5)]



#########------------Data Prepartion-----------------############
#ENCODING AS A ONE HOT VECTOR MULTILABEL DATA. (due to neuralnet's hatred for categorical data smh)
traffic_csv = cbind(traffic_raw,
  class.ind(as.factor(traffic_raw$Day.of.Week)),
  class.ind(as.factor(traffic_raw$Hour)),
  class.ind(as.factor(traffic_raw$Weather)),
  class.ind(as.factor(traffic_raw$Level.of.Service))
  )

#Renaming the resultiing new columns due to the above encoding
names(traffic_csv) = 
  c(names(traffic_raw),
    #Day.of.Week
    "Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday",
    
    #Hour
    "`00:00`","`01:00`","`02:00`","`03:00`","`04:00`","`05:00`","`06:00`","`07:00`","`08:00`","`09:00`","`10:00`",
    "`11:00`","`12:00`","`13:00`","`14:00`","`15:00`","`16:00`","`17:00`","`18:00`","`19:00`","`20:00`",
    "`21:00`","`22:00`","`23:00`",
    
    #Weather
    "clear_sky","few_clouds","scattered_clouds","broken_clouds","overcast_clouds",
    "light_intensity_drizzle","drizzle","light_rain","moderate_rain","heavy_intensity_rain",
    "light_thunderstorm","thunderstorm","thunderstorm_with_light_rain","thunderstorm_with_rain",
    "thunderstorm_with_heavy_rain","mist","haze","fog","smoke",
    
    #Level.of.Service
    "A","B","C","D","E","F"
  )

traffic_csv = traffic_csv[5:60]
#Scaling
#scl <- function(x){
#traffic_csv[,1:50] <- scale(traffic_csv[,1:50])
#  (x - min(x))/(max(x)-min(x))
#}
#traffic_csv[,1:50] <- data.frame(lapply(traffic_csv[,1:50],scl))

head(traffic_csv)



#######----------Neural Network & Cross validation----------############
#Creating formula for the neural network
n <- colnames(traffic_csv)
n
f <- as.formula(paste("A+B+C+D+E+F~",
                      paste(n[!n%in% c("A","B","C","D","E","F")],
                            collapse = " + ")))
f
set.seed(500)
#k-fold cross validation
k<-1000

#Set train-Test split proportions
proportion <- 0.95

#reduction in sample size for testing
traffic_csv_test = traffic_csv[c(1:1000),]

#holder for comparison results cross validation
compare_lists <- NULL

#Loop to run nn k-times
#for(i in 1:k){
  index <- sample(1:nrow(traffic_csv_test),
                  round(proportion*nrow(traffic_csv_test)))
  train_cv <- traffic_csv_test[index,]
  test_cv <- traffic_csv_test[-index,]
  nn_cv <- neuralnet(f, data =train_cv, rep = 1, 
                     linear.output = FALSE, threshold=0.001, stepmax = 1e+5)
  plot(nn_cv)
  
  #Compute Predictions
  predict_testNN <- predict(nn_cv, test_cv[, 1:50])
  #predict_testNN
  nrows = nrow(predict_testNN)
  #table(test_cv$F, apply(pr.nn, 1, which.max))
  prediction_list <- NULL
  original_results <- NULL
  for(j in 1:nrows){
    prediction_list[[j]]<-which.max(predict_testNN[j,])
    original_results[[j]]<-which.max(test_cv[,c(51:56)][j,])
    j <- j+1
  }
  
  compare_lists[[i]] <- mean(prediction_list == original_results)
  i<-i+1
#}

accuracy <- mean(compare_lists)
print(accuracy)