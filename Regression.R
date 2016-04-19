HW5.highDim <- read.csv("D:/mxs92/Download/6210/HW5-highDim.csv")

# HIgh dimension
# fit seperate linear regression
coef.Lm <- c()
for (i in 1:100){
  coef.Lm[i] <- coef(lm(Y~HW5.highDim[,i]+1,data=HW5.highDim))
}
plot(coef.Lm)

# Ridge Regression
library(glmnet)
lambda.s <- exp(seq(10,-10,length.out=100))
xmat <- as.matrix(HW5.highDim[,-101])
ymat <- as.matrix(HW5.highDim[,101])
cv.outcome <- cv.glmnet(x=xmat,y=ymat,lambda=lambda.s,alpha = 0)
plot(cv.outcome)
bestlambda <- cv.outcome$lambda.min
bestlambda
ridge.model <- glmnet(x=xmat,y=ymat,lambda=bestlambda,alpha = 0)
coef.ridge <- coef(ridge.model)
plot(coef.ridge)

# Lasso
library(glmnet)
#lambda.s <- exp(seq(10,-10,length.out=100))
# xmat <- as.matrix(HW5.highDim[,-101])
# ymat <- as.matrix(HW5.highDim[,101])
cv.outcome2 <- cv.glmnet(x=xmat,y=ymat,lambda=lambda.s,alpha = 1)
plot(cv.outcome2)
bestlambda2 <- cv.outcome2$lambda.min
bestlambda2
lasso.model <- glmnet(x=xmat,y=ymat,lambda=bestlambda,alpha = 1)
coef.lasso <- coef(lasso.model)
plot(coef.lasso)

# Boston
library(MASS)
data(Boston)
highCrime <- as.numeric(Boston$crim >= 5)
Boston.New <- cbind(Boston,highCrime)
Boston.New$crim <- NULL

# predict and ROC AUC
library(ROCR) # logistic regression
logr.fit <- glm(highCrime~.,data=Boston.New,family = binomial)
logr.pred <- prediction(predictions=predict(logr.fit,data=Boston.New),labels=Boston.New$highCrime)
performance(logr.pred, "auc")@y.values[[1]]
logr.ROC <- performance(logr.pred,"tpr","fpr")
plot(logr.ROC)
summary(logr.fit)

# other models
library(ROCR) # logistic regression 1
logr.fit1 <- glm(highCrime~dis,data=Boston.New,family = binomial)
logr.pred1 <- prediction(predictions=predict(logr.fit1,data=Boston.New),labels=Boston.New$highCrime)
performance(logr.pred1, "auc")@y.values[[1]]
logr.ROC1 <- performance(logr.pred1,"tpr","fpr")
plot(logr.ROC1)

library(ROCR) # logistic regression 2
logr.fit2 <- glm(highCrime~nox+dis,data=Boston.New,family = binomial)
logr.pred2 <- prediction(predictions=predict(logr.fit2,data=Boston.New),labels=Boston.New$highCrime)
performance(logr.pred2, "auc")@y.values[[1]]
logr.ROC2 <- performance(logr.pred2,"tpr","fpr")
plot(logr.ROC2)

library(ROCR) # logistic regression 3
logr.fit3 <- glm(highCrime~ nox + dis + lstat,data=Boston.New,family = binomial)
logr.pred3 <- prediction(predictions=predict(logr.fit3,data=Boston.New),labels=Boston.New$highCrime)
performance(logr.pred3, "auc")@y.values[[1]]
logr.ROC3 <- performance(logr.pred3,"tpr","fpr")
plot(logr.ROC3)

library(ROCR) # logistic regression 4
logr.fit4 <- glm(highCrime~ nox + dis + rad,data=Boston.New,family = binomial)
logr.pred4 <- prediction(predictions=predict(logr.fit4,data=Boston.New),labels=Boston.New$highCrime)
performance(logr.pred4, "auc")@y.values[[1]]
logr.ROC4 <- performance(logr.pred3,"tpr","fpr")
plot(logr.ROC4)

library(ROCR) # logistic regression 5
logr.fit5 <- glm(highCrime~ nox + dis + rad + lstat,data=Boston.New,family = binomial)
logr.pred5 <- prediction(predictions=predict(logr.fit5,data=Boston.New),labels=Boston.New$highCrime)
performance(logr.pred5, "auc")@y.values[[1]]
logr.ROC5 <- performance(logr.pred5,"tpr","fpr")
plot(logr.ROC5)


linear.fit <- lm(highCrime~.,data=Boston.New) #linear regression
linear.pred <- prediction(predictions=predict(linear.fit,data=Boston.New),labels=Boston.New$highCrime)
performance(linear.pred, "auc")@y.values[[1]]
linear.ROC <- performance(linear.pred,"tpr","fpr")
plot(linear.ROC)

xmatnew <- as.matrix(Boston.New[,-14]) #ridge regression
ridge.cv <- cv.glmnet(x=xmatnew,y=Boston.New$highCrime,alpha = 0)
plot(ridge.cv)
lambda.ridge <- ridge.cv$lambda.min
ridge.fit <- glmnet(x=xmatnew,y=Boston.New$highCrime,lambda=lambda.ridge,alpha = 0)
ridge.pred <- prediction(predictions = predict(ridge.fit,s=lambda.ridge,newx=xmatnew),labels=Boston.New$highCrime)
performance(ridge.pred,"auc")@y.values[[1]]
ridge.ROC <- performance(lasso.pred,"tpr","fpr")
plot(ridge.ROC)

lasso.cv <- cv.glmnet(x=xmatnew,y=Boston.New$highCrime,alpha=1)# lasso regression
plot(lasso.cv)
lambda.lasso <- lasso.cv$lambda.min
lasso.fit <- glmnet(x=xmatnew,y=Boston.New$highCrime,lambda = lambda.lasso,alpha = 1)
lasso.pred <- prediction(predictions = predict(lasso.fit,s=lambda.lasso,newx=xmatnew),labels = Boston.New$highCrime)
performance(lasso.pred,"auc")@y.values[[1]]
lasso.ROC <- performance(lasso.pred,"tpr","fpr")
plot(lasso.ROC)

# LDA
library(MASS)
lda.fit <- lda(highCrime~.,data=Boston.New)
lda.pred <- prediction(predictions=predict(lda.fit)$posterior[,2],labels = Boston.New$highCrime)
performance(lda.pred,"auc")@y.values[[1]]
lda.ROC <- performance(lda.pred,"tpr","fpr")
plot(lda.ROC)

library(MASS)
lda.fit <- lda(highCrime~.-,data=Boston.New)
lda.pred <- prediction(predictions=predict(lda.fit)$posterior[,2],labels = Boston.New$highCrime)
performance(lda.pred,"auc")@y.values[[1]]
lda.ROC <- performance(lda.pred,"tpr","fpr")
plot(lda.ROC)
# QDA
library(MASS)
qda.fit <- qda(highCrime~.-zn-indus-rad-tax-ptratio,data=Boston.New)
qda.pred <- prediction(predictions = predict(qda.fit)$posterior[,2],labels=Boston.New$highCrime)
performance(qda.pred,"auc")@y.values[[1]]
qda.ROC <- performance(qda.pred,"tpr","fpr")
plot(qda.ROC)

# problem 5
nox
dis