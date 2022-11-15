install.packages("tidyverse")
install.packages("readxl")

library(readxl)
library(ggplot2)

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
ggplot(file, aes(co2)) + geom_histogram(bins = 20, color = 'black', fill = '#69b3a2') + theme_bw()+ggtitle("Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

jarque.bera.test(file$co2)

#-ko-

#model is not appropriate









