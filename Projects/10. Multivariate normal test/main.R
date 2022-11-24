library(foreign)
library(tidyverse)
library(moments)
library(MASS)
library(GGally)
library(readxl)
library(dplyr)

data <- read_excel("data3_1R.xlsx", sheet = 3)
head(data)
str(data)
summary(data)

vars3_1 <- as.data.frame(attr(data,"variable.labels"),stringsAsFactors = F)

data <- data %>% 
  rename("KRK" = "X1", 'HRUDNIK' = 'X2', 'STEHNO' = 'X3', 'KOLENO' = 'X4', 'KOTNIK' = 'X5')

#matrix plot
plot(data)

ggpairs(data)

## Q-Q plot (KRK)
qqnorm(data$KRK, pch = 1, frame = T)
qqline(data$KRK, col = "red", lwd = 1)

# Chí-kvadrát diagram
data$D2 <- mahalanobis(data, colMeans(data), cov(data))
data$rank <- rank(data$D2)
data$p <- (data$rank-0.5)/nrow(data)
data$Q <- qchisq(data$p, 5, ncp = 0, lower.tail = TRUE)

ggplot(data, aes(Q,D2))+
  geom_point()+
  geom_abline(aes(intercept=0,slope=1), color = "blue") + geom_vline(xintercept = 13)

#outliers
outliers <- arrange(data[data$Q > 13,], Q)
outliers


