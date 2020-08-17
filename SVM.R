###분류모델. 이산형일때 사용
rm(list=ls())

library(MASS)
data(biopsy)
table(biopsy$class)
biopsy$class <- as.character(biopsy$class)
biopsy$class[biopsy$class=='malignant'] <- "1"
biopsy$class[biopsy$class=='benign'] <- "0"
biopsy$class <- as.factor(biopsy$class)


library(DMwR)
sum(is.na(biopsy))
biopsy <- knnImputation(biopsy[,-1]) #중앙값 대체

ind <- sample(2,nrow(biopsy),replace=TRUE,prob=c(0.7,0.3))
train <- biopsy[ind==1,-1] #id 제거하기 위해 -1
test <- biopsy[ind==2,-1]

install.packages("e1071")
library(e1071)

poly.tune <- tune.svm(class ~., data = train[,-1],
                      kernel = "polynomial")
summary(poly.tune)

best.poly <- poly.tune$best.model
poly.test <- predict(best.poly, newdata = test)
table(poly.test, test$class)
summary(poly.test)
table(test$class)
library(caret)

confusionMatrix(test$class,poly.test)





##########################################################################




data("Pima.te")
data("Pima.tr")

sum(is.na(Pima.te))
sum(is.na(Pima.tr))

summary(Pima.te)
summary(Pima.tr)

newdata <- rbind(Pima.te,Pima.tr)

newdata.scale <- data.frame(scale(newdata[,-8])) #y변수로 쓸 type 빼고 scale하기
str(newdata.scale)
newdata.scale$type <- newdata$type #y만 binary변수고 나머진 연속형 변수라서 위에서 y만 빼고 scale했던 것

set.seed(12345)
ind <- sample(2, nrow(newdata.scale),replace=TRUE,prob=c(0.7,0.3))
train <- newdata.scale[ind==1,]
test <- newdata.scale[ind==2,]

library(e1071)

poly.tune <- tune.svm(type ~., data = train[,-1],
                      kernel = "polynomial")
summary(poly.tune)

best.poly <- poly.tune$best.model
poly.test <- predict(best.poly, newdata = test)
table(poly.test, test$type)
summary(poly.test)
table(test$class)
library(caret)

confusionMatrix(test$type,poly.test)

# polynomial 말고 radial / sigmoid 함수도 있다. 코드는 다 똑같고 kernel = "" 자리에 저거만 바꿔 써주면 된다.


radial.tune <- tune.svm(type ~., data = train[,-1],
                      kernel = "radial")
summary(radial.tune)

best.radial <- radial.tune$best.model
radial.test <- predict(best.radial, newdata = test)

confusionMatrix(test$type,radial.test)



sigmoid.tune <- tune.svm(type ~., data = train[,-1],
                        kernel = "sigmoid")
summary(sigmoid.tune)

best.sigmoid <- sigmoid.tune$best.model
sigmoid.test <- predict(best.sigmoid, newdata = test)

confusionMatrix(test$type,sigmoid.test)
