library(foreign)
library(tidyverse)
library(GGally)

data4_1 <- read.spss("data4_1.sav",to.data.frame=TRUE)
str(data4_1)
summary(data4_1)

vars4_1 <- as.data.frame(attr(data4_1,"variable.labels"),stringsAsFactors = F)

disc <- as.data.frame(data4_1[c(3:12)])
colnames(disc) <- vars4_1[c(3:12),]

ggpairs(disc)

#PCA
PCA <- prcomp(disc, scale=TRUE)

#Vector of sd's
PCA$sdev

# matrix of eigenvectors
PCA$rotation

#component loads
t(PCA$sdev*t(PCA$rotation))

#matrix of components
PCA$x

#eigenvalues of the matrix
apply(PCA$x,2,sd)^2

summary(PCA)

# Screeplot
screeplot(PCA)

#Biplot
biplot(PCA)

