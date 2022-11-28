library(foreign)
library(ggpubr)
library(factoextra)

core_data <- read.spss('du3.sav', to.data.frame = T)

#data <- core_data[, 2:5]
data <- core_data[,c(2,5,10,16)]
data

any(is.na(data))
head(data)

plot(data, main = 'Matrix chart')
ggpairs(data)
qqnorm(data$x1, pch = 1, frame = T, main = 'Q-Q plot(X1 - cena)')
qqline(data$x1, col = "red", lwd = 1)
qqnorm(data$x4, pch = 1, frame = T, main = 'Q-Q plot(X4 - max rýchlosť)')
qqline(data$x4, col = "red", lwd = 1)
qqnorm(data$x9, pch = 1, frame = T, main = 'Q-Q plot(X9 - emisie)')
qqline(data$x9, col = "red", lwd = 1)
qqnorm(data$x15, pch = 1, frame = T, main = 'Q-Q plot(X15 - hmotnosť)')
qqline(data$x15, col = "red", lwd = 1)


D_matrix <- dist(data, method = 'euclidean')

clust <- hclust(D_matrix, method = "ward.D")
fviz_nbclust(data, kmeans, nstart = 136, method = 'wss')
plot(clust)
rect.hclust(clust, k = 5, border = "red")

set.seed(6)
km <- kmeans(data, centers = 5, iter.max = 100, nstart = 1, trace = TRUE)

fviz_cluster(km, data, palette = c('red', 'green', 'blue', 'gray', 'brown'), geom = 'point', ellipse.type = 'convex',
             ggtheme = theme_bw())

c1 <- core_data[km$cluster == 1,c(2,5,10,16)]
c2 <- core_data[km$cluster == 2,c(2,5,10,16)]
c3 <- core_data[km$cluster == 3,c(2,5,10,16)]
c4 <- core_data[km$cluster == 4,c(2,5,10,16)]
c5 <- core_data[km$cluster == 5,c(2,5,10,16)]

km$cluster
# polohy konečných centroidů:
km$centers
# velikosti jednotlivých shluků
km$size


