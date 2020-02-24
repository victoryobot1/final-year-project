#install.packages('neuralnet')
library(neuralnet)
#install.packages('ggplot2')
#library(ggplot2)
library(nnet)

#read in dataset
traffic_raw = read.csv("3rdMainland Bridge_NB - prediction dataset - copy.csv", header = TRUE, sep = ",")[,c(2,3,4,5)]
#traffic_csv = scale(traffic_csv[,c(2,3,4,5)])


#ENCODING AS A ONE HOT VECTOR MULTILABEL DATA
traffic_csv = cbind(traffic_raw,
  class.ind(as.factor(traffic_raw$Day.of.Week)),
  class.ind(as.factor(traffic_raw$Hour)),
  class.ind(as.factor(traffic_raw$Weather)),
  class.ind(as.factor(traffic_raw$Level.of.Service))
  )


names(traffic_csv) = 
  c(names(traffic_raw),
    #Day.of.Week
    "Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday",
    
    #Hour
    "00:00","01:00","02:00","03:00","04:00","05:00","6:00","7:00","8:00","9:00","10:00",
    "11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00",
    "21:00","22:00","23:00",
    
    #Weather
    "clear_sky","few_clouds","scattered_clouds","broken_clouds","overcast_clouds",
    "light_intensity drizzle","drizzle","light_rain","moderate_rain","heavy_intensity rain",
    "light_thunderstorm","thunderstorm","thunderstorm_with_light_rain","thunderstorm_with_rain",
    "thunderstorm_with_heavy_rain","mist","haze","fog","smoke",
    
    #Level.of.Service
    "A","B","C","D","E","F"
  )

traffic_csv = traffic_csv[5:60]
#head(traffic_csv)


n <- names(traffic_csv)


#BUG
f <- as.formula(paste("A+B+C+D+E+F~",
                     paste(n[1:50],
                           collapse = " + ")))

f <- as.formula(paste("A+B+C+D+E+F~",
                      paste(n[!n%in% c("A","B","C","D","E","F")],
                            collapse = " + ")))
#BUG

f






#Divide dataset into variables and Level of Service. (predictors and values we want to predict)
traffic_predictors = traffic_csv[,c(1,2,3)]
traffic_los = traffic_csv[,4]

#Setting up training set
traffic_predictors_train = traffic_predictors[1:5000,]
traffic_los_train = traffic_los[1:5000]
#Setting up test set
traffic_predictors_test = traffic_predictors[5001:5596,]
traffic_los_test = traffic_los[5001:5596]

#Fit neural network
set.seed(2) #used to reproduce results
NN = neuralnet(traffic_los_train~., traffic_predictors_train, hidden = c(5,3), threshold = 0.001, stepmax = 1e+05, linear.output = FALSE)
plot(NN)

#Testing the neural network
predict_testNN = predict(NN, traffic_predictors_test)
predict_testNN
#Calculating accuracy
ncorrect = sum(predict_testNN == traffic_los_test)
n = length(traffic_los_test)
n
accuracy = ncorrect/n
print(accuracy)

