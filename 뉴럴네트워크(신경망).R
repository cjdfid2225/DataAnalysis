rm(list=ls())

library(MASS)
data("shuttle")
table(shuttle$use)
shuttle$use <- as.character(shuttle$use)
shuttle$use[shuttle$use=='auto'] <- 1
shuttle$use[shuttle$use=='noauto'] <- 0
shuttle$use <- as.factor(shuttle$use)

library(nnet)
ind <- sample(2,nrow(shuttle),replace=TRUE,prob=c(0.7,0.3))
train <- shuttle[ind==1,] 
test <- shuttle[ind==2,]
m_ <- nnet(use ~., data=train, size=3)
pred <- predict(m_,newdata=test,type="class")
pred <- as.factor(pred)

library(caret)
confusionMatrix(test$use, pred)
all.equal(test$use,pred)

library(ROCR)

pred <- as.numeric(pred) -1
test$use <- as.numeric(test$use) -1
pr <- prediction(pred,test$use)
plot(performance(pr,"tpr","fpr"))
auc <- performance(pr, measure="auc")
auc <- auc@y.values[[1]]
auc




##### neuralet도 자주 쓴다



##################################################################







install.packages("nnet")
library(nnet)
data(iris)
m <- nnet(Species~., data=iris, size=3)

predict(m, newdata=iris)
predict(m,newdata=iris,type="class")
