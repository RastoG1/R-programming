install.packages("tidyverse")
install.packages("readxl")

library(readxl)
library(ggplot2)
library(tidyverse)

file = as.data.frame(read_excel('co2.xlsx'))
head(file)

ggplot(file, aes(x = c(1:132), y = co2)) + geom_line()

library(tseries)

install.packages('forecast')
library('forecast')

x = ts(file$co2, frequency = 12)
fit = tslm(x ~ trend  + season) 
fit
summary(fit)
x = as.numeric(x)
#plot(x, type = 'l')
xx = ts(c(1:52), frequency = 4)
y = predict(fit, xx)
#plot(y, type = 'l')
#lines(y, type = 'l', col = 'blue')


z = file
ggplot(file, aes(x = 1:length(co2))) + geom_line(aes(y = x, colour = 'y')) + geom_line(aes(y = y, colour = 'y^'))+
  scale_colour_manual("", 
                      breaks = c("y", "y^"),
                      values = c("black", "red"))+ggtitle("y vs y^") +
  theme(plot.title = element_text(hjust = 0.5))

plot(forecast(fit, h=10), main = 'Forecast')

#residual analysis
#1.GM
plot(fit$residuals)
abline(h = 0, col = 'red')
mean(fit$residuals)

#2.GM
library(lmtest)
bptest(fit)
#-ok-

#3.GM
library(stats)
acf(fit$residuals, type = "correlation")

library(lmtest)
lmtest::dwtest(fit)

#-ko-

#4.GM
h <- hist(fit$residuals, breaks=15, col="#69b3a2", xlab="residuals",
         main="Distribution", xlim = c(-2,2))

xfit <- seq(min(fit$residuals),max(fit$residuals), length = 200)
yfit <- dnorm(xfit,mean=mean(fit$residuals),sd=sd(fit$residuals))
yfit <- yfit * diff(h$mids[1:2]) * length(fit$residuals)
lines(xfit, yfit, col="red", lwd=2)
abline(v = 0, col = 'blue', lwd = 2)

xyz = as.data.frame(fit$residuals)
#chart with ggplot
ggplot(xyz, aes(x)) +
  geom_histogram(aes(y = ..density..), fill='#69b3a2', col='black') +
  stat_function(fun = dnorm, args = list(mean = mean(xyz$x), sd = sd(xyz$x)), col = 'red', lwd = 1) + ggtitle('Distribution')

jarque.bera.test(fit$residuals)

#-ko-

#model is not appropriate


















