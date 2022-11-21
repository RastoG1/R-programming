#inspired by Jack Sandom


#Gathering and cleaning the data
library(ggplot2)
library(cowplot)
library(caret)
library(ROCR)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)


credit = read.csv('german_credit.csv', header = T)
str(credit)
head(credit)
tail(credit)
nrow(credit)
ncol(credit)

#continous to categorical
credit$Duration.of.Credit..month. = cut(credit$Duration.of.Credit..month., c(0,12,18,24,Inf), labels = c(1:4))
credit$Credit.Amount = cut(credit$Credit.Amount, c(0,1000,5000,10000,Inf), labels = c(1:4))
credit$Age..years. = cut(credit$Age..years., c(18,25,40,60,Inf), labels = c(1:4))
head(credit[,c(3,6,14)])

for(i in 1:21) credit[, i] <- as.factor(credit[, i])
credit

ggplot(credit, aes(Creditability)) +
  geom_bar(fill = "#4EB25A") +
  theme(axis.title.x=element_blank()) + 
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(breaks=seq(0,700,100))  +
  scale_x_discrete(labels = c("Bad","Good"))+
  ggtitle("Count of Good and Bad Credit Risks")


summary(credit$Creditability)


ggplot(credit, aes(Value.Savings.Stocks, fill = Creditability), stat="identity") +
  geom_bar() +
  scale_fill_manual(values = c("#D3D6D4", "#4EB25A"), labels=c("Bad","Good")) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(breaks=seq(0,700,100)) +
  scale_x_discrete(labels = c("< 100 DM", "100-500 DM", "500-1000 DM", "> 1000 DM", "Unknown")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) + 
  theme(axis.text.y = element_text(size = 10)) +
  theme(legend.text=element_text(size=10)) +
  theme(legend.title=element_text(size=12)) +
  ggtitle("Good and Bad Credit Risks by Credit History")


ggplot(credit, aes(Occupation, fill = Creditability), stat="identity") +
  geom_bar() +
  scale_fill_manual(values = c("#D3D6D4", "#4EB25A"), labels=c("Bad","Good")) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(breaks=seq(0,700,100)) +
  scale_x_discrete(labels = c("Unemployed", "Unskilled", "Skilled", "Management")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) + 
  theme(axis.text.y = element_text(size = 10)) +
  theme(legend.text=element_text(size=10)) +
  theme(legend.title=element_text(size=12)) +
  ggtitle("Good and Bad Credit Risks by Occupation")

ggplot(credit, aes(Age..years., fill = Creditability), stat="identity") +
  geom_bar() +
  scale_fill_manual(values = c("#D3D6D4", "#4EB25A"), labels=c("Bad","Good")) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(breaks=seq(0,700,100)) +
  scale_x_discrete(labels = c("18-25", "26-40", "41-60", "60+")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) + 
  theme(axis.text.y = element_text(size = 10)) +
  theme(legend.text=element_text(size=10)) +
  theme(legend.title=element_text(size=12)) +
  ggtitle("Good and Bad Credit Risks by Age")


#Statistical modeling
set.seed(2828)
inTraining = createDataPartition(credit$Creditability, p=0.7, list=FALSE)
train = credit[inTraining,]
test = credit[-inTraining,]

lmModel <- glm(Creditability ~ ., family = binomial, data = train)

# Fit model to test set
lmFit <- predict(lmModel, type = "response", test)

# Compare predictions to test set
lmPred <- prediction(lmFit, test$Creditability)

# Area Under the Curve (AUC) plot
plot(performance(lmPred, 'tpr', 'fpr'))
lines(0:1,0:1, col = 'red')

performance(lmPred, measure = 'auc')@y.values[[1]]

# Decision Tree
set.seed(28)
dtModel = rpart(Creditability ~ ., data=train)
fancyRpartPlot(dtModel)

dtFit <- predict(dtModel, test, type = 'prob')[, 2]
dtPred <- prediction(dtFit, test$Creditability)
plot(performance(dtPred, 'tpr', 'fpr'))
performance(dtPred, measure = 'auc')@y.values[[1]]

# Random Forest
set.seed(2828)
rfModel <- randomForest(Creditability ~ ., data=train)
rfFit <- predict(rfModel, test, type = 'prob')[,2]
rfPred <- prediction(rfFit, test$Creditability)
plot(performance(rfPred, 'tpr', 'fpr'))
performance(rfPred, measure = 'auc')@y.values[[1]]


#The plot below shows the rank of importance for variables in the Random Forest model. 
#Account balance is ranked as the most significant measurement in the model with purpose second. Purpose identifies 
#the reason for the applicant’s request for credit e.g. car, education, business etc.
par(mfrow=c(1,1))
varImpPlot(rfModel, pch=1, main="Random Forest Model Variables Importance")

rfCM <- confusionMatrix(test$Creditability,
                        predict(rfModel, test, type="class"))
rfCM

