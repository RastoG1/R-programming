

df <- read.csv('bank_note_data.csv')
df <- as.data.frame(df)
head(df)
str(df)
summary(df)

#split data
library(caTools)
set.seed(101)
split <- sample.split(df$Class, SplitRatio = 0.7)
train <- df[split == T,]
test <- df[split == F,]
str(train)
str(test)

#model
library(neuralnet)
#in nn class must be int
nn <- neuralnet(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy,data=train,hidden=10,linear.output=FALSE)
predicted_nn_values <- compute(nn,test[,1:4]) #5 is Class!!!
head(predicted_nn_values$net.result)
predictions <- sapply(predicted_nn_values$net.result, round)
head(predictions)
table(predictions,test$Class)


#------------------------
#comparing with Random Forest model
library(randomForest)
#class to factor
df$Class <- factor(df$Class)
set.seed(101)
split2 <- sample.split(df$Class, SplitRatio = 0.7)
train2 <- df[split == T,]
test2 <- df[split == F,]

model <- randomForest(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy, data=train2)
rf.pred <- predict(model,test)
table(rf.pred,test$Class)

