rm(list=ls())

library(randomForest)
m <- randomForest(Species~., data=iris, importance=TRUE)
importance(m) #변수중요도

varImpPlot(m, main="varImpPlot of Iris")





################################################################




data("biopsy")
df <- biopsy[,-1]
sum(is.na(df))
df[!complete.cases(df),]
library(DMwR)
df.t <- centralImputation(df)
sum(is.na(df.t))

df.t$class <- as.character(df.t$class)
df.t$class[df.t$class=='malignant'] <- '1'
df.t$class[df.t$class=='benign'] <- '0'
df.t$class <- as.factor(df.t$class)

set.seed(12345)
ind <- sample(2,nrow(df.t),replace=TRUE,prob=c(0.7,0.3))
train <- df.t[ind==1,-1] #id 제거하기 위해 -1
test <- df.t[ind==2,-1]
train
m <- randomForest(class~.,data=train,importance=TRUE)
importance(m)
m

m.test <- predict(m, newdata=test, type="response")
table(m.test, test$class) #실제 test값과 예측한 test값 비교

library(caret)
confusionMatrix(test$class, m.test)





##########################################################################




data("Pima.te")
data("Pima.tr")

sum(is.na(Pima.te))
sum(is.na(Pima.tr))

summary(Pima.te)
summary(Pima.tr)

df <- rbind(Pima.te,Pima.tr)

df$type <- as.character(df$type)
df$type[df$type=='Yes'] <- '1'
df$type[df$type=='No'] <- '0'
df$type <- as.factor(df$type)

set.seed(12345)
ind <- sample(2,nrow(df),replace=TRUE,prob=c(0.7,0.3))
train <- df[ind==1,]
test <- df[ind==2,]

m <- randomForest(type~.,data=train,importance=TRUE)
importance(m)

m.test <- predict(m, newdata=test, type="response")
table(m.test, test$type)

library(caret)
confusionMatrix(test$type, m.test)





##################################################################




data("Boston")
sum(is.na(Boston))

set.seed(12345)
ind <- sample(2,nrow(Boston),replace=TRUE,prob=c(0.7,0.3))
train <- Boston[ind==1,]
test <- Boston[ind==2,]

m <- randomForest(medv~.,data=train,importance=TRUE)
importance(m)

m.test <- predict(m, newdata=test, type="response")

library(ModelMetrics)
rmse(test$medv,m.test)
library(forecast)
accuracy(m.test,test$medv)

# 마이닝에는 회귀와 분류가 있다. regression(연속형변수)가 y가 되는 것. classification(범주형변수)가 y가 되는것.
# 이 경우는 연속형변수라서 confusionMatrix가 안 먹히고 rmse를 써야하는 것