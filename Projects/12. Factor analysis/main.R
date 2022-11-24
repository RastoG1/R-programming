library(foreign)
library(tidyverse)
library(GGally)
library(corrplot)
library(GPArotation)
library(psych)

data4_1=read.spss("data4_1.sav",to.data.frame=TRUE)
str(data4_1)
summary(data4_1)

vars4_1 <- as.data.frame(attr(data4_1,"variable.labels"),stringsAsFactors = F)

disc <- as.data.frame(data4_1[c(3:12)])
colnames(disc) <- vars4_1[c(3:12),]

#normalize data
sdisc <- scale(disc)
sdisc[,1]=-sdisc[,1]
sdisc[,5]=-sdisc[,5]
sdisc[,6]=-sdisc[,6]
sdisc[,10]=-sdisc[,10]


cor(sdisc)

SPCA <- prcomp(sdisc)

#f.a.
sdisc.fa <- factanal(sdisc, factors = 4, rotation = "varimax", scores = "regression")

# Cummunalities
apply(sdisc.fa$loadings^2,1,sum)

# unexplained variability by factors
sdisc.fa$uniquenesses

sdisc.fa$loadings
sdisc.fa$rotmat
sdisc.fa$scores

corrplot(cor(sdisc), order = "hclust")

#library psych
sdisc.fa.psych <- fa(sdisc)






