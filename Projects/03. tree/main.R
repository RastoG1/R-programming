#decision tree

library(ISLR)

head(College)
df = College

library(ggplot2)
ggplot(df, aes(Room.Board, Grad.Rate)) + geom_point(aes(color = Private), size = 3, alpha = 0.5)

ggplot(df, aes(F.Undergrad)) + geom_histogram(aes(fill = Private), color = 'black', bins = 50, alpha = 0.5)+ theme_bw()

ggplot(df, aes(Grad.Rate)) + geom_histogram( aes(fill = Private),color = 'black', bins = 50, alpha = 0.6) + theme_bw()

#mistake???
subset(df, Grad.Rate > 100)
#change to 100%
df['Cazenovia College', 'Grad.Rate'] = 100
#df[,df$Grad.Rate > 100] = 100

#Train/test split
library(caTools)
set.seed(101)

sample = sample.split(df$Private, SplitRatio = 0.7)
train = subset(df, sample == T)
test = subset(df, sample == F)

library(rpart)
tree = rpart(Private ~ ., method = 'class', data = train)
summary(tree)
tree_p = predict(tree, test)

tree_p = as.data.frame(tree_p)

j = function(x){
  if(x>0.5){
    return('yes')
  }else{
    return('no')
  }
}

tree_p$Private = sapply(tree_p$Yes, j)
print(head(tree_p))

table(tree_p$Private, test$Private)

install.packages('rpart.plot')
library(rpart.plot)
prp(tree)


#random forest
install.packages('randomForest')
library(randomForest)
rf_model = randomForest(Private ~.,data = train, importance = TRUE)
rf_model$confusion
rf_p = predict(rf_model, test)
table(rf_p, test$Private)

