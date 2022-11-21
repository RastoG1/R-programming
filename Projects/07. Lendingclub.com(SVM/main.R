#using data from lendingclub.com from 2007-2010 and be trying to classify and predict 
#whether or not the borrower paid back their loan in full.


loans <- read.csv('loan_data.csv')
str(loans)

#convert to categorical
loans$credit.policy <- factor(loans$credit.policy)
loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid) 

str(loans)

#EDA
library(ggplot2)
ggplot(loans, aes(fico)) + geom_histogram(aes(fill = not.fully.paid), color = "black", bins = 40, alpha = 0.5)+
  scale_fill_manual(values = c("green", "red"))

ggplot(loans, aes(x = factor(purpose)))+geom_bar(aes(fill = not.fully.paid), position = 'dodge')

ggplot(loans, aes(int.rate, fico)) + geom_point(aes(color = not.fully.paid, alpha = 0.3)) + theme_bw()


#Train test split
library(caret)
set.seed(101)
sample <- createDataPartition(loans$not.fully.paid, p = 0.7, list = F)
train <- loans[sample,]
test <- loans[-sample,]

#train SVM
library(e1071)
model <- svm(not.fully.paid ~ ., data = train)
summary(model)
model$gamma

predicted_values <- predict(model, test[1:13])
table(predicted_values, test$not.fully.paid)

#tuning model
tuned_results <- tune(svm, train.x = not.fully.paid ~ .,data = train, kernel = 'radial',
                      ranges = list(cost=c(100, 200), gamma = c(0.1)))
summary(tuned_results)

# best parameters:
#   cost gamma
#   100   0.1
tuned_model <- svm(not.fully.paid ~., data = train, cost = 100, gamma = 0.1)
summary(tuned_model)
tuned_predictions <- predict(tuned_model, test[1:13])
table(tuned_predictions, test$not.fully.paid)
