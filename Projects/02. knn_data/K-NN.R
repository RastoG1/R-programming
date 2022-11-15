#install.packages('ISLR',repos = 'http://cran.us.r-project.org')

library(ISLR)

str(Caravan)
summary(Caravan$Purchase)
any(is.na(Caravan))

var(Caravan[,1])
var(Caravan[,2])

hist(Caravan[, 1])
hist(Caravan[,2])

# save the Purchase column in a separate variable(not num)
purchase = Caravan[,86]

# Standarize the dataset using "scale()" R function
standardized_Caravan = scale(Caravan[,-86])

var(standardized.Caravan[,1])
var(standardized.Caravan[,2])

# First 1000 rows for test set
test_index = 1:1000
test_data = standardized.Caravan[test_index,]
test_purchase = purchase[test_index]

# Rest of data for training
train_data = standardized_Caravan[-test_index,]
train_purchase = purchase[-test_index]

library(class)

set.seed(101)
predicted_purchase = knn(train_data,test_data,train_purchase,k=1)
head(predicted_purchase)

mean(test_purchase != predicted_purchase)

predicted_purchase = NULL
error_rate = NULL

for(i in 1:20){
  set.seed(101)
  predicted_purchase = knn(train_data,test_data,train_purchase,k=i)
  error_rate[i] = mean(test_purchase != predicted_purchase)
}

print(error_rate)

library(ggplot2)

k_values <- 1:20
error_df <- data.frame(error_rate,k_values)
error_df
ggplot(error_df,aes(x=k_values,y=error_rate)) + geom_point()+ geom_line(lty="dotted",color='red')





