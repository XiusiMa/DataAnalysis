library(MASS)
data(Boston)
head(Boston)
logmedv <- log(Boston$medv)
Boston <- cbind(Boston,logmedv)
Boston <- Boston[,-14]

templmF <- lm(logmedv~1,data=Boston)
tempSocpeF <- formula(lm(logmedv~.,data =Boston))
stepF <- stepAIC(templmF,scope=tempSocpeF,direction = "forward")

templmB <- lm(logmedv~.,data=Boston)
tempSocpeB <- formula(lm(logmedv~1,data =Boston))
stepB <- stepAIC(templmB,scope=tempSocpeB,direction = "backward")
#4 
lm <- lm(logmedv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + 
           black + lstat,data=Boston)
summary(lm)

# check assumptions
# a normality of residuals
shapiro.test(lm$residuals)
library(car)
qqPlot(lm)
# b homoskedasticity and linearity
library(car)
ncvTest(lm)
plot(lm,which=3)
plot(Boston$crim,lm$residuals,ylab="Residuals")
plot(Boston$zn,lm$residuals,ylab="Residuals")
plot(Boston$indus,lm$residuals,ylab="Residuals")
plot(Boston$chas,lm$residuals,ylab="Residuals")
plot(Boston$nox,lm$residuals,ylab="Residuals")
plot(Boston$rm,lm$residuals,ylab="Residuals")
plot(Boston$age,lm$residuals,ylab="Residuals")
plot(Boston$dis,lm$residuals,ylab="Residuals")
plot(Boston$rad,lm$residuals,ylab="Residuals")
plot(Boston$tax,lm$residuals,ylab="Residuals")
plot(Boston$ptratio,lm$residuals,ylab="Residuals")
plot(Boston$black,lm$residuals,ylab="Residuals")
plot(Boston$lstat,lm$residuals,ylab="Residuals")
'''
Question
how can residual see the linearily of variables vs residual
how to interpret the ggplot function in this hw
outlierTest() and cooks.distance()
R^2
in this homework mse from kcv

'''
outlierTest(lm)
fitted(lm)
residuals(lm)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(lm)

# c outliers
plot(lm,which=4)

# 6 compare models
lmal
