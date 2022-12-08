library(foreign)
library(MASS)
library(ggplot2)
library(GGally)

data <- read.spss("du4_6.sav", to.data.frame=TRUE)

data$pohlavi <- as.character(data$pohlavi)
data$pohlavi[data$pohlavi=='mu\x9e'] = 'man'
data$pohlavi[data$pohlavi=='\x9eena'] = 'woman'
data$pohlavi[data$pohlavi=='????'] = NA
data$pohlavi <- as.factor(data$pohlavi)
ID <- data$id
data <- data[2:5]
#data <- as.data.frame(data)

data
str(data)
summary(data)

ggpairs(data[1:3])


library(ggfortify)
pca <- prcomp(data[1:3], scale = F)
screeplot(pca, type = 'l', main = 'Component Analysis - Scree Plot', ylim = c(0, 300))
biplot(pca)
autoplot(pca, data = data) + geom_point(aes(colour = pohlavi))+ theme_classic()


PCi <- data.frame(pca$x, pohlaví = data$pohlavi)

ggplot(PCi,aes(x=PC1,y=PC2,col=pohlaví))+
  geom_point()+
  scale_color_manual(values = c("green","orange"), na.value = "red")+
  theme_gray()

lda <- lda(pohlavi ~ ., data = data, prior = c(1/2, 1/2))
lda 

pred <- predict(lda, data)
pred$class[1:10]

pointsToLabel <- ID[1:10]
PCi$pohlaví[1:10] <- pred$class[1:10]
PCi$id <- ID

ggplot(PCi,aes(x=PC1,y=PC2,col=pohlaví))+
  geom_point()+
  scale_color_manual(values = c("green","orange"), na.value = "red") + 
  geom_text(aes(label = id), color = "red", size = 3, fontface = "bold", 
            data = subset(PCi, id %in% pointsToLabel),check_overlap = F)+
            theme_gray()
           
head(select(PCi, id, pohlaví), 10)

head(pred$posterior,10) %>% round(4) * 100

table(pred$class, data$pohlavi)

(222+238)/(222+238+20+16) #AUC


#-------------------------------------------------------------------------------------------------------

#decision tree
library(rpart)
tree <- rpart(pohlavi ~., data = data[11:nrow(data),])
tree_p <- predict(tree, data[1:10,])
tree_p
tree_pAll <- predict(tree, data)
test_dfALL <- data
test_dfALL$pohlavi[1:10] <- pred$class[1:10]
library(rpart.plot)
prp(tree, main = 'Decision Tree')
library(rattle)
fancyRpartPlot(tree)

tree_pAll <- tree_pAll[1:506]
tree_pAll[tree_pAll > 0.5] <- 'man'
tree_pAll[tree_pAll <= 0.5] <- 'woman'

table(tree_pAll[11:506], test_dfALL$pohlavi[11:506])
#AUC
(224+236)/(224+236+18+18)

#--------------------------------------------------------------------------------------------------------






