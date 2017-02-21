
library(data.table)
# read data --------------
w <- "~/Documents/GWU/R"
train_data <- fread(paste0(w,"/Titanic/train.csv")) 
test_data <- fread(paste0(w,"/Titanic/test.csv"))

# explore data ------------
head(train_data)
plot(density(train_data$Age,na.rm = TRUE))
summary(is.na(train_data)) # check missing value
plot(density(Fare,na.rm = TRUE))
# ggplot(train_data$Pclass,aes(x=train_data$Pclass))+geom_bar(stat = "count")
train_data$Survived <- as.factor(train_data$Survived)

# clean data --------
# sex for dummy variable
train_data$Sex <- ifelse(train_data$Sex=="female",1,0)
train_data$Pclass <- as.factor(train_data$Pclass)
test_data$Sex <- ifelse(test_data$Sex=="female",1,0)
test_data$Pclass <- as.factor(test_data$Pclass)
# Embarked 
train_data$Embarked <- as.factor(train_data$Embarked)
test_data$Embarked <- as.factor(test_data$Embarked)
# age inference
# with each cateloge 
Mr_row <- grep("Mr.", train_data$Name,fixed = TRUE)
for(i in Mr_row){
train_data[i,]$Name <- "Mr"
}
Mrs_row <- grep("Mrs.", train_data$Name,fixed = TRUE)
for(i in Mrs_row){
  train_data[i,]$Name <- "Mrs"
}
Master_row <- grep("Master.", train_data$Name,fixed = TRUE)
for(i in Master_row){
  train_data[i,]$Name <- "Master"
}
Miss_row <- grep("Miss.", train_data$Name,fixed = TRUE)
for(i in Miss_row){
  train_data[i,]$Name <- "Miss"
}
Dr_row <- grep("Dr.", train_data$Name,fixed = TRUE)
for(i in Dr_row){
  train_data[i,]$Name <- "Dr"
}
Ms_row <- grep("Ms.", train_data$Name,fixed = TRUE)
for(i in Ms_row){
  train_data[i,"Name"] <- "Ms"
}
mr_age <- round(mean(train_data[train_data$Name == "Mr",]$Age,na.rm = TRUE),2)
mrs_age <- round(mean(train_data[train_data$Name == "Mrs",]$Age,na.rm = TRUE),2)
miss_age <- round(mean(train_data[train_data$Name == "Miss",]$Age,na.rm = TRUE),2)
dr_age <- round(mean(train_data[train_data$Name == "Dr",]$Age,na.rm = TRUE),2)
master_age <- round(mean(train_data[train_data$Name == "Master",]$Age,na.rm = TRUE),2)
ms_age <- round(mean(train_data[train_data$Name == "Ms",]$Age,na.rm = TRUE),2)
for(i in Mr_row){
  if(is.na(train_data[i,"Age"])){
    train_data[i,"Age"] <- mr_age
  }
}
for(i in Mrs_row){
  if(is.na(train_data[i,"Age"])){
    train_data[i,"Age"] <- mrs_age
  }
}
for(i in Master_row){
  if(is.na(train_data[i,"Age"])){
    train_data[i,"Age"] <- master_age
  }
}
for(i in Dr_row){
  if(is.na(train_data[i,"Age"])){
    train_data[i,"Age"] <- dr_age
  }
}
for(i in Miss_row){
  if(is.na(train_data[i,"Age"])){
    train_data[i,"Age"] <- miss_age
  }
}
# test data ------
Mr_row_t <- grep("Mr.", test_data$Name,fixed = TRUE)
for(i in Mr_row_t){
  if(is.na(test_data[i,"Age"])){
    test_data[i,"Age"] <- mr_age
  }
}
Mrs_row_t <- grep("Mrs.", test_data$Name,fixed = TRUE)
for(i in Mrs_row_t){
  if(is.na(test_data[i,"Age"])){
    test_data[i,"Age"] <- mrs_age
  }
}
Master_row_t <- grep("Master.", test_data$Name,fixed = TRUE)
for(i in Master_row_t){
  if(is.na(test_data[i,"Age"])){
    test_data[i,"Age"] <- master_age
  }
}
Miss_row_t <- grep("Miss.", test_data$Name,fixed = TRUE)
for(i in Miss_row_t){
  if(is.na(test_data[i,"Age"])){
    test_data[i,"Age"] <- miss_age
  }
}
Dr_row_t <- grep("Dr.", test_data$Name,fixed = TRUE)
for(i in Dr_row_t){
  if(is.na(test_data[i,"Age"])){
    test_data[i,"Age"] <- dr_age
  }
}
Ms_row_t <- grep("Ms.", test_data$Name,fixed = TRUE)
for(i in Ms_row_t){
  if(is.na(test_data[i,"Age"])){
    test_data[i,"Age"] <- ms_age
  }
}
summary(is.na(train_data))
# basic model Tree ---------
train_data_apply <- train_data[,-c(1,4,9,11)]
set.seed(99)
row_num <- sample(nrow(train_data),600,replace = FALSE)
sub_train <- train_data_apply[row_num,]
sub_test <- train_data_apply[-row_num,]
library(tree)
tree_model <- tree(Survived~.,data = sub_train)
summary(tree_model)
tree_pred <- predict(tree_model,newdata = sub_test,type = "class")
table(sub_test$Survived,tree_pred)
# (164+64)/(164+64+53+10) = 0.78

tree_model_full <- tree(Survived~.,data = train_data_apply)
summary(tree_model_full)
tree_pred <- predict(tree_model_full,newdata = test_data,type = "class")

# random Forest
library(randomForest)
rF_model <- randomForest(Survived ~., data = sub_train,
                         importance = T, ntree = 500, mtry = 4)
rF_model
result <- predict(rF_model,newdata = sub_test,type = "class")
table(sub_test$Survived,result)
# (154+85)/(154+85+20+32) = 0.82
rF_model_full <- randomForest(Survived ~., data = train_data_apply,
                         importance = T, ntree = 500, mtry = 4)

# boosting



# improved by boosting
library(xgboost)
# xgboost
