# HW3 6210
#input data
HW3Data <- read.csv("D:/mxs92/Download/HW3Data.csv")
head(HW3Data)

## 1 Calaculate Pearson's correlation coefficient
attach(HW3Data)
cor(weight,height,method="pearson")
data <- data.frame(height=HW3Data$height,weight=HW3Data$weight)
cor(data)
## 2 bootstrap correlation coefficient
xCor <-cor(weight,height,method="pearson")
boots <- matrix(0,10000,1)
new.sample.fn = function(data,index){
  new.height = data$height[index]
  new.weight = data$weight[index]
  return (cor(new.height,new.weight,method = "pearson"))
}
for(i in 1:10000){
 boots[i] <- new.sample.fn(HW3Data,sample(237,size=237,replace=TRUE))
}
  
hist(boots-xCor,100)

# construct confidence interval
xCor+quantile(boots-xCor,c(.025,.975))

