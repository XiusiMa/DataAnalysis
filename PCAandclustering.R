bike.day.orign <- read.csv("D:/mxs92/Documents/6210/project/Bike-Sharing-Dataset/day.csv")

bike.day.pc1 <- bike.day.orign[,10:13]

library(corrplot)
bike.day.cor1 <- cor(bike.day.pc1)
corrplot(bike.day.cor1)
plot(bike.day.pc1)

fit1 <- princomp(bike.day.pc1,cor=TRUE)
summary(fit1)
loadings(fit1)[,1:3]
fit1$scores[1:10,1:3]

plot(fit1,type = "lines")
biplot(fit1)

bike.day.cl <- bike.day.orign[,10:13]

dis.bike <- dist(bike.day.cl,method = "manhattan")

bike.hclust.cmp <- hclust(dis.bike,method="complete")
bike.hclust.sng <- hclust(dis.bike,method = "single")

plot(bike.hclust.cmp)
rect.hclust(bike.hclust.cmp, k=5, border="red")
plot(bike.hclust.sng)
rect.hclust(bike.hclust.sng, k=5, border="red")

bike.day.cl2 <- bike.day.orign[,10:13]
df <- bike.day.cl2
dftemp <- scale(df)
fitcl1 <- kmeans(df,1)
fitcl2 <- kmeans(df,2)
fitcl3 <- kmeans(df,3)
fitcl5 <- kmeans(df,5)
fitcl7 <- kmeans(df,7)
fitcl10 <- kmeans(df,10)
library(cluster)
clusplot(dftemp, fitcl1$cluster, color=TRUE, shade=TRUE,labels=2, lines=4)
clusplot(dftemp, fitcl2$cluster, color=TRUE, shade=TRUE,labels=2, lines=4)
clusplot(dftemp, fitcl3$cluster, color=TRUE, shade=TRUE,labels=3, lines=4)
clusplot(dftemp, fitcl5$cluster, color=TRUE, shade=TRUE,labels=5, lines=4)
clusplot(dftemp, fitcl7$cluster, color=TRUE, shade=TRUE,labels=7, lines=4)
clusplot(dftemp, fitcl10$cluster, color=TRUE, shade=TRUE,labels=10, lines=4)

wss <- (nrow(df)-1)*sum(apply(df,2,var))
for (i in 2:30){
  wss[i] <- sum(kmeans(df,centers=i)$withinss)
}  
plot(1:30, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

fitcl5 <- kmeans(df,5)