rm(list=ls())
data(Boston)

set.seed(12345)
ind <- sample(2, nrow(Boston),replace=TRUE,prob=c(0.7,0.3))
train <- Boston[ind==1,]
test <- Boston[ind==2,]

linear.tune <- tune.svm(medv ~., data=train,
                        kernel = "linear")
summary(linear.tune)
best.linear <- linear.tune$best.model
tune.test <- predict(best.linear, newdata=test)

library(ModelMetrics)
rmse(test$medv, tune.test)



################
################
################lenear은 연속형일 때 사용한다
###################



data(cars)

set.seed(12345)
ind <- sample(2, nrow(cars), replace=TRUE, prob=c(0.7,0.3))
train <- cars[ind==1,]
test <- cars[ind==2,]

linear.tune <- tune.svm(Price~.,data=train,
                        kernel = "linear")
summary(linear.tune)
best.linear <- linear.tune$best.model
tune.test <- predict(best.linear, newdata=test)

rmse(test$Price, tune.test)
