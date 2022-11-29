library(MASS)

data <- Boston

#normalize data(2 means only for columns)
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)

scaled_data <- scale(data, center = mins, scale = maxs - mins)
scaled <- as.data.frame(scaled_data)
head(scaled) 

#split data
library(caret)
set.seed(101)
split <- createDataPartition(scaled$medv, p = 0.7, list = F)
train <- scaled[split, ]
test <- scaled[-split, ]
head(train)

#build model
library(neuralnet)
n <- names(train)
f <- as.formula(paste('medv ~', paste(n[!n %in% 'medv'], collapse = '+')))
f

nn <- neuralnet(f, data = train, hidden = c(5, 3), linear.output = T)
plot(nn)

predicted_nn_value <- compute(nn, test[1:13]) 
true_pred <- predicted_nn_value$net.result*(max(data$medv)-min(data$medv))+
              min(data$medv)
test_r <- (test$medv)*(max(data$medv)-min(data$medv)) + min(data$medv)

MSE_nn <- sum((test_r - true_pred)^2)/nrow(test)
MSE_nn

error_df <- data.frame(test_r, true_pred)
head(error_df)

library(ggplot2)
ggplot(error_df, aes(test_r, true_pred))+
  geom_point() + stat_smooth()
  



