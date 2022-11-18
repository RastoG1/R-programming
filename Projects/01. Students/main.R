install.packages('ggplot2')
install.packages('ggthemes')
install.packages('dplyr')
install.packages('corrgram',repos = 'http://cran.us.r-project.org')
install.packages('corrplot',repos = 'http://cran.us.r-project.org')
install.packages('plotly')
install.packages('caTools')


library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrplot)
library(corrgram)
library(plotly)
library(caTools)


df = read.csv('student-mat.csv', sep = ';')

head(df, 3)
tail(df, 3)

str(df)
summary(df)
any(is.na(df))

numeric_cols = sapply(df, is.numeric)
cor_data <- cor(df[,numeric_cols])
cor_data

help(corrplot)
corrplot(cor_data, method = 'square')
corrplot(cor_data, method = 'color', order = 'alphabet')
corrplot(cor_data, method = 'number')
#more options for corrplot -> https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

pl <- ggplot(df,aes(x=G3)) + geom_histogram(bins=20,alpha=0.5,fill='blue') + theme_minimal()
print(ggplotly(pl))

#or
pl

#or
h <- hist(df$G3, main = 'data')
h
xfit<-seq(min(df$G3),max(df$G3),length=20)
yfit<-dnorm(xfit,mean=mean(df$G3),sd=sd(df$G3))
yfit <- yfit*diff(h$mids[1:2])*length(df$G3)
lines(xfit, yfit, col="blue", lwd=1)

#or
d <- density(df$G3)
plot(d, main="data")
polygon(d, col="gray", border="black")
abline(v = mean(x = df$G3), col = 'red')


#Train & Test Data
set.seed(101)
#split up data randomly
sample <- sample.split(df$age, SplitRatio = 0.70)
# Training Data
train <- subset(df, sample == TRUE)
# Testing Data
test <- subset(df, sample == FALSE)

model = lm(G3 ~ ., train)
summary(model)

#Residuals
res <- residuals(model)
res <- as.data.frame(res)
head(res)
pl2 <- ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5)
pl2
print(ggplotly(pl2))

plot(model)






