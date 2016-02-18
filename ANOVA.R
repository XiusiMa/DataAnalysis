
# HW2 ANOVA
# input data
HW2Data2 <- read.csv("~/Downloads/HW2Data2.csv")
head(HW2Data2)

# 1 draw a boxplot of pain relif vs pain levels
boxplot(HW2Data2$PainLevel,HW2Data2$Relief,names = c("PainLevel","Relief"))
library(ggplot2)
qplot(factor(PainLevel),Relief,data=HW2Data2,geom="boxplot")

#2 A factor ANOVA compare Scores of each level group
## test normaility of relief in each level
figurenormal <- par(mfcol=c(3,3))
for(i in 1:8){
  z <- 7
  groupi <- HW2Data2[HW2Data2$PainLevel == i,4] 
  qqnorm(groupi)
  qqline(groupi)
}
## test equal variance
bartlett.test(Relief~factor(PainLevel),data=HW2Data2)

## ANOVA
aov1 <- aov(Relief~factor(PainLevel),data=HW2Data2)
summary(aov1)
model.tables(aov1,"means")


#3 two factor ANOVA with the following factors: Codeine and pain levels.
## Assumption
## ANOVA
aov2 <- aov(Relief~Codeine+factor(PainLevel)+Codeine:factor(PainLevel),data=HW2Data2)
summary(aov2)

# Scheffe’s method to conduct a multiple comparison for the different pairs
library(agricolae)
agricolae::scheffe.test(aov1,"factor(PainLevel)",group=TRUE,console=TRUE)


