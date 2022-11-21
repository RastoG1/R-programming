library(foreign)
library(tidyverse)
library(moments)
library(MASS)

data2_1 <- read.spss("data2_1.sav",to.data.frame=TRUE)
str(data2_1)
summary(data2_1)

vars2_1<-as.data.frame(attr(data2_1, "variable.labels"), stringsAsFactors = F)

cor.test(data2_1$TTIME,data2_1$AGE,method = "pearson")
cor.test(data2_1$TTIME,data2_1$AGE,method = "kendall")
summary(lm(TTIME~AGE, data2_1))
summary(lm(TTIME~CATAGE, data2_1))

skewness(data2_1$TTIME)
kurtosis(data2_1$TTIME)

## Q-Q plot(log_values)
qqnorm(log(data2_1$TTIME), pch = 1, frame = T)
qqline(log(data2_1$TTIME), col = "steelblue", lwd = 1)


ggplot(data = data2_1)+
  geom_boxplot(mapping = aes(CATAGE,TTIME))

ggplot(data = data2_1)+
  geom_boxplot(mapping = aes(CATAGE,log(TTIME)))

ggplot(data = filter(data2_1,TTIME<40))+
  geom_boxplot(mapping = aes(CATAGE,TTIME))

# odhady podmíněných rozdělení (filtrované hodnoty, logaritmické měřítko)
ggplot(data = filter(data2_1,TTIME<40))+
  geom_violin(mapping = aes(CATAGE,TTIME))

ggplot(data = data2_1)+
  geom_violin(mapping = aes(CATAGE,TTIME))+
scale_y_log10()

# bodový graf s hladkou křivkou (filtrované hodnoty, logaritmické měřítko)
ggplot(data = filter(data2_1,TTIME<40))+
  geom_point(mapping = aes(AGE,TTIME))+
  geom_smooth(mapping = aes(AGE, TTIME))

ggplot(data = data2_1,aes(AGE,TTIME))+
  geom_point()+
  geom_smooth()+
  scale_y_log10()

# na závěr histogram s odhadnutým lognormálním rozdělením
fit <- fitdistr(data2_1$TTIME, "lognormal")
ggplot(filter(data2_1,TTIME<80), aes(TTIME)) +
geom_histogram(aes(y = ..density..)) +
stat_function(fun = dlnorm, size = 0.5, color = 'steelblue',
              args = list(mean = fit$estimate[1], sd = fit$estimate[2])) 

