library(tsne)
library(MASS)
data(Boston)
B.data <- Boston

B.data$medv <- NULL


# initialize counter to 0
#x <- 0
#epc <- function(x) {
#  x <<- x + 1
#  filename <- paste("d:\\plot", x, "jpg", sep=".")
#  cat("> Plotting TSNE to ", filename, " ")
  
  # plot to d:\\plot.x.jpg file of 2400x1800 dimension
#  jpeg(filename, width=2400, height=1800)
  
#  plot(x, t='n', main="T-SNE")
#  text(x, labels=rownames(B.data))
#  dev.off()
#}

# run tsne (maximum iterations:500, callback every 50 epochs, target dimension k=2)

tsne_data<- tsne(B.data, k=2, epoch_callback=NULL, max_iter=500, epoch=50)

plot(tsne_data,pch=Boston$medv/10)

# K-Means Clustering with 5 clusters
fit <- kmeans(tsne_data,5,nstart = 20)

# Cluster Plot against 1st 2 principal components
library(cluster) 
clusplot(tsne_data, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
Boston$cluster <- as.factor(fit$cluster)
summary(Boston$cluster)

glm.fit <- glm(medv~(nox + rm + age + dis + lstat - 1)*factor(cluster),data=Boston)
summary(glm.fit)

lm.fit <- glm(medv~.-cluster,data=Boston)
summary(lm.fit)

# compare the model with loov
library(boot)
cv.glm.fit <- cv.glm(Boston,glm.fit)$delta[1]
cv.glm.fit
cv.lm.fit <- cv.glm(Boston,lm.fit)$delta[1]
cv.lm.fit

# Project 

# input data and analysis  

bike.day.orign <- read.csv("D:/mxs92/Documents/6210/project/Bike-Sharing-Dataset/day.csv")
summary(bike.day.orign)
library(ggplot2) 
library(reshape)
bike.melt <- melt(bike.day.orign)
## Using as id variables
ggplot(bike.melt,aes(x=value))+geom_density()+facet_wrap(~variable,scales="free")
bike.day.pc <- bike.day.orign[,8:12]
cnt<- bike.day.orign[,14]
bike.day.model <- cbind(bike.day.pc,cnt)

# relationship between variables.
library(GGally)
ggpairs(bike.day.model)

bike.day.model$temp <- NULL
# Look up the dataset and calculate the correlation matrix.
library(corrplot)
corrplot(cor(bike.day.orign[,-(1:2)]))
bike.day.cor <- cor(bike.day.model)
corrplot(bike.day.cor)

tsne_data<- tsne(bike.day.orign, k=2, epoch_callback=NULL, max_iter=500, epoch=50)

plot(tsne_data,pch=Boston$medv/10)

# K-Means Clustering with 5 clusters
fit <- kmeans(tsne_data,5,nstart = 20)
