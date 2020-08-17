rm(list=ls())

#data(Boston)
#보스턴시의 주택가격 데이터
#CRIM : 1인당 범죄율
#ZN : 25,000초과하는 거주지역의 비율
#INDUS : 비소매상업지역이 점유하고 있는 토지의 비율
#CHAS : 찰스강 경계는 1, 아니면 0
#NOX : 10PPM당 일산화질소
#RM : 1가구당 평균 방의 개수
#AGE : 1940년 이전에 건축된 소유주택의 비율
#DIS : 직업센터까지의 접근성 지수
#RAD : 방사형도로까지 접근성 지수
#TAX : 재산세율
#PTRATIO : 학생/교사 비율
#B : 흑인의 비율
#LSTAT : 하위계층 비율
#MEDV : 본인 소유의 주택가격(1,000달러 단위)

##파일입출력
df <- read.csv("Boston.csv", header = TRUE, stringsAsFactors = FALSE)
#header = TRUE : 파일의 첫행을 변수명으로 처리할지 여부
#stringsAsFactors = FALSE : 문자열을 팩터가 아닌 문자열 타입으로 읽기
df <- df[,-1] #첫 행이 ""로 표시되어 쓸모없는 인덱스가 하나 더 늘어나므로 삭제
write.csv(df, file="df.csv", row.names = TRUE)
#row.names : 파일명 설정

##그 밖에 입출력
# read.table("file.txt", header=TRUE, sep=" ")
# install.packages("readxl")
# read.excel(path = "file.xlsx",
#            sheet = "tab1", # 시트 이름
#            col_names = TRUE)

##기술통계량
#install.packages("Hmisc")
library(Hmisc)
describe(df)
summary(medv~crim+zn, data=df)


##데이터전처리
#결측치 확인
sum(is.na(df)) #결측치 숫자 세서 그 합을 보여준다
df[complete.cases(df),] #결측치 아닌 애들을 보여줘
df[!complete.cases(df),] #결측치인 애들을 보여줘
#결측치 삭제
df <- na.omit(df)
#결측치 대체
df$crim[is.na(df$crim)] <- 0

install.packages("DMwR")
library(DMwR)
centralImputation(df)#결측치를 (숫자일경우 중앙값/문자일경우 최빈값)으로 대체
knnImputation(df)#kmeans를 활용한 결측치 대체(최근 이웃 분류 알고리즘)


##데이터분할
#랜덤 샘플링
t.idx <- sample(1:506, 100)
df.tr <- df[t.idx,]
df.te <- df[-t.idx,]
#층화샘플링
install.packages("sampling")
library(sampling)
(x <- strata(c("Species"),size=c(3,3,3),method="srswor",data=iris))
train <- getdata(df,x)

# 다중공선성 : 독립변수들 간에 상관관계
install.packages("car")
library(car)
lm1 <- lm(medv~.,data=df)
#vif(lm1) : 팽창지수. 숫자가 높으면 상관관계가 있다고 본다. 보통 5 내지 10보다 크면 제거한다.
#근데 이 기준을 절대적 기준으로 잡지말고, 팽창지수들을 다 살펴보고 튀는 값들을 제거하는 편이 낫다.
vif(lm1) > 5 # >> rad, tax를 뺀다. (나중에 모델 성능 안좋으면 다시 추가하기도 한다)

# 분산이 0에 가까운 변수 제거
install.packages("caret")
library(caret)
nearZeroVar(df,saveMetrics = TRUE)
df <- df[,-nearZeroVar(df)]

# 상관관계
install.packages("caret")
install.packages("corrplot")
install.packages("psych")

library(caret)
findCorrelation(cor(subset(df,select=-c(medv))))
library(corrplot)
corrplot(cor(df), method = "ellipse")
library(psych)
pairs.panels(df)

#변수중요도 평가
install.packages("randomForest")
library(randomForest)
rf <- randomForest(medv~.,data=df)
varImp(rf)


##모델링
lm1 <- lm(medv ~ crim + chas + nox + rm + dis + rad + ptratio + lstat, data = df.tr)
lm2 <- step(lm1,direction = "both")

pred <- predict(lm1, newdata = df.te)
pred <- predict(lm1, newdata = df.te, interval = "confidence")
pred <- predict(lm1, newdata = df.te, interval = "prediction")

pred
coef(lm1)#회귀계수
fitted(lm1)#예측된 값

prob <- predict(lm1, newdata=df.te)


install.packages("forecast")
library(forecast)
accuracy(lm1)


#prob <- predict(lm1, newdata=df.te, type="response")
#pr <- prediction(prob, df.te$medv)
#prf <- performance(pr, measure="tpr", x.measure="fpr")
#plot(prf)

# 검증
library(caret)
confusionMatrix(predicted, actual)

library(ROCR)
pred <- prediction(probs,labels)
plot(performance(pred, "tpr","fpr"))
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# 리포트
install.packages("rmarkdown")
install.packages("knitr")
library(rmarkdown)
library(knitr)

