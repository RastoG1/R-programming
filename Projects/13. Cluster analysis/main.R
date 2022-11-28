library(foreign)
library(ggpubr)
library(factoextra)

core_data <- read.spss('du3.sav', to.data.frame = T)

data <- core_data[, 2:5]
data

any(is.na(data))
head(data)


D_matrix <- dist(data, method = 'euclidean')
# Ve druhém kroku provádíme hierarchické shlukování na dané matici vzdáleností. (single linkage = nearest neighbor)
clust <- hclust(D_matrix, method = "ward.D")

# Pro výsledné řešení můžeme zobrazit dendrogram. Není nejkrásnější, ale dají se do něj vložit další prvky
plot(clust)

fviz_nbclust(data, kmeans, nstart = 136, method = 'wss')
rect.hclust(clust, k = 5, border = "red")


set.seed(6)
km <- kmeans(data, centers = 5, iter.max = 100, nstart = 1, trace = TRUE)

fviz_cluster(km, data, palette = c('red', 'green', 'blue', 'gray', 'brown'), geom = 'point', ellipse.type = 'convex',
             ggtheme = theme_bw())

c1 <- core_data[km$cluster == 1,1:5]
c2 <- core_data[km$cluster == 2,1:5]
c3 <- core_data[km$cluster == 3,1:5]
c4 <- core_data[km$cluster == 4,1:5]
c5 <- core_data[km$cluster == 5,1:5]
c6 <- core_data[km$cluster == 6,1:5]


km$cluster
# polohy konečných centroidů:
km$centers
# velikosti jednotlivých shluků:
km$size


