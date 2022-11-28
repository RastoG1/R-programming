df2 <- read.csv('winequality-red.csv',sep=';')
df1 <- read.csv('winequality-white.csv',sep=';')


#add labels
df1$label <- sapply(df1$pH,function(x){'red'})
df2$label <- sapply(df2$pH,function(x){'white'})

wine <- rbind(df1,df2)

#EDA
library(ggplot2)
ggplot(wine,aes(x=residual.sugar)) + geom_histogram(aes(fill=label),color='black',bins=50)+
  scale_fill_manual(values = c('#faf7ea','#ae4554')) + theme_bw()

ggplot(wine,aes(x=citric.acid)) + geom_histogram(aes(fill = label),color='black',bins=50)+
  scale_fill_manual(values = c('#faf7ea','#ae4554')) + theme_bw()

ggplot(wine,aes(x=alcohol)) + geom_histogram(aes(fill = label),color='black',bins=50)+
  scale_fill_manual(values = c('#faf7ea','#ae4554')) + theme_bw()


ggplot(wine, aes(citric.acid,residual.sugar)) + geom_point(aes(color = label), alpha = 0.2)+
  scale_color_manual(values = c('#faf7ea','#ae4554')) + theme_dark()

ggplot(wine, aes(volatile.acidity,residual.sugar)) + geom_point(aes(color = label), alpha = 0.2)+
  scale_color_manual(values = c('#faf7ea','#ae4554')) + theme_dark()

#model
clus_data <- wine[, 1:12]
head(clus_data)

wine_clust <- kmeans(clus_data, 2)
wine_clust$centers

table(wine$label, wine_clust$cluster)






