if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(MASS)){install.packages("MASS"); library(MASS)}
if(!require(ridge)){install.packages("ridge"); library(ridge)}
if(!require(glmnet)){install.packages("glmnet"); library(glmnet)}
if(!require(lars)){install.packages("lars"); library(lars)}
if(!require(elasticnet)){install.packages("elasticnet"); library(elasticnet)}
if(!require(DAAG)){install.packages("DAAG"); library(DAAG)}
if(!require(car)){install.packages("car"); library(car)}

setwd('D:/samsung_ai/test/')
dat <- read.csv('dataSet.csv')


dat1 <- dplyr::select(dat, 주야, 요일, 사상자수, 중상자수, 부상신고자수,
                     발생지시도, 발생지시군구, 사고유형_대분류,
                     사고유형_중분류, 법규위반, 도로형태_대분류, 도로형태, 
                     당사자종별_1당_대분류, 당사자종별_2당_대분류)

# k-fold 함수사용
dat2 <- dplyr::select(dat1, 주야, 요일, 사상자수, 중상자수, 부상신고자수)
lm <- cv.lm(data = dat2, 사상자수~., m = 5)




# 레벨좀 제거하자----
dat3 <- dplyr::select(dat, 주야, 요일, 사상자수, 중상자수, 부상신고자수)
                      # 발생지시도, 사고유형_대분류,
                      # 사고유형_중분류, 법규위반, 도로형태_대분류, 도로형태, 
                      # 당사자종별_1당_대분류, 당사자종별_2당_대분류)

dat3 <- filter(dat3, 법규위반 != "진로양보 의무 불이행" & 법규위반 != "과로" &
                 법규위반 != "통행우선 순위위반" & 법규위반 != "보행자과실")
dat3 <- droplevels(dat3)



lm <- lm(사상자수~., dat3)
lm1 <- step(lm, direction = "both")
summary(lm1)


ind <- sample(1:2, nrow(dat3), replace = TRUE, prob = c(0.7, 0.3))
train <- dat3[ind==1,]
test <- dat3[ind==2,]

lm <- lm(사상자수~., train)
p <- predict(lm, newdata = test)
table(round(p), test$사상자수)
sum(round(p) == test) / nrow(test)




# levels 평준화
summary(lm1)$coefficients[,4][summary(lm1)$coefficients[,4] <= 0.15]

for (i in length(levels(dat3$발생지시도)):1)
{
  if( levels(dat3$발생지시도)[i] != '경기' &
      levels(dat3$발생지시도)[i] != '경남' &
      levels(dat3$발생지시도)[i] != '대구' &
      levels(dat3$발생지시도)[i] != '부산' &
      levels(dat3$발생지시도)[i] != '충남')
  {
    levels(dat3$발생지시도)[i] <- '없음'
  }
}
for (i in length(levels(dat3$법규위반)):1)
{
  if( levels(dat3$법규위반)[i] != '과속' &
      levels(dat3$법규위반)[i] != '교차로 통행방법 위반' &
      levels(dat3$법규위반)[i] != '보행자 보호의무 위반' &
      levels(dat3$법규위반)[i] != '보행자과실' &
      levels(dat3$법규위반)[i] != '서행 및 일시정지위반' &
      levels(dat3$법규위반)[i] != '신호위반' &
      levels(dat3$법규위반)[i] != '안전거리 미확보' &
      levels(dat3$법규위반)[i] != '안전운전 의무 불이행' &
      levels(dat3$법규위반)[i] != '정비불량 제차의 운전금지위반' &
      levels(dat3$법규위반)[i] != '중앙선 침범' &
      levels(dat3$법규위반)[i] != '직진 및 우회전차의 통행방해')
  {
    levels(dat3$법규위반)[i] <- '없음'
  }
}
for (i in length(levels(dat3$도로형태)):1)
{
  if( levels(dat3$도로형태)[i] != '교량위' &
      levels(dat3$도로형태)[i] != '터널안')
  {
    levels(dat3$도로형태)[i] <- '없음'
  }
}
for (i in length(levels(dat3$당사자종별_2당_대분류)):1)
{
  if( levels(dat3$당사자종별_2당_대분류)[i] != '농기계' &
      levels(dat3$당사자종별_2당_대분류)[i] != '승용차' &
      levels(dat3$당사자종별_2당_대분류)[i] != '승합차' &
      levels(dat3$당사자종별_2당_대분류)[i] != '원동기장치자전거' &
      levels(dat3$당사자종별_2당_대분류)[i] != '이륜차' &
      levels(dat3$당사자종별_2당_대분류)[i] != '자전거')
  {
    levels(dat3$당사자종별_2당_대분류)[i] <- '없음'
  }
}


dat4 <- dplyr::select(dat3, 주야, 사상자수, 중상자수, 부상신고자수,
                      발생지시도, 법규위반, 도로형태, 
                      당사자종별_2당_대분류)

lm <- lm(사상자수~., dat4)
lm1 <- step(lm, direction = "both")
summary(lm1)



ind <- sample(1:2, nrow(dat4), replace = TRUE, prob = c(0.7, 0.3))
train <- dat4[ind==1,]
test <- dat4[ind==2,]

lm <- lm(사상자수~., train)
p <- predict(lm, newdata = test)
table(round(p), test$사상자수)
sum(round(p) == test) / nrow(test)





# 그냥 사상자 구하기----
dat5 <- dplyr::select(dat, 사상자수, 중상자수, 부상신고자수)

ind <- sample(1:2, nrow(dat5), replace = TRUE, prob = c(0.7, 0.3))
train <- dat5[ind==1,]
test <- dat5[ind==2,]

lm <- lm(사상자수~., train)
summary(lm)
p <- predict(lm, newdata = test)
table(round(p), test$사상자수)
sum(round(p) == test) / nrow(test)

library(e1071)
svm <- svm(사상자수~., train, scale = FALSE)
p <- predict(svm, newdata = test)
table(round(p), test$사상자수)
sum(round(p) == test) / nrow(test)

library(party)
ctree <- ctree(사상자수~. , data=train)
p <- predict(ctree, newdata = test)
table(round(p), test$사상자수)
sum(round(p) == test) / nrow(test)

# 중상자+부상신고자+요일 or 주야 svm
dat5 <- dplyr::select(dat, 사상자수, 중상자수, 부상신고자수, 주야, 요일)

ind <- sample(1:2, nrow(dat5), replace = TRUE, prob = c(0.7, 0.3))
train <- dat5[ind==1,]
test <- dat5[ind==2,]

svm <- svm(사상자수~., train, scale = FALSE)
p <- predict(svm, newdata = test)
table(round(p), test$사상자수)
sum(round(p) == test) / nrow(test)




# 중상자, 부상신고자 제외 후 svm
dat5 <- dplyr::select(dat, 주야, 요일, 사상자수, 중상자수, 부상신고자수)

ind <- sample(1:2, nrow(dat5), replace = TRUE, prob = c(0.7, 0.3))
train <- dat5[ind==1,]
test <- dat5[ind==2,]

svm <- svm(사상자수~., train, kernel="radial", cost=1, gamma=0.5)
summary(svm)
p <- predict(svm, newdata = test)
table(round(p), test$사상자수)
sum(round(p) == test) / nrow(test)

svm_tune <- tune(method = svm, train.x=train[,-3], train.y=train[,3, drop = FALSE], 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

?tune
print(svm_tune)



# tune 사용 안하고 요일별로 모델링----
dat5 <- filter(dat, 주야 == "야간") %>% 
  dplyr::select(요일, 사상자수, 중상자수, 부상신고자수)

ind <- sample(1:2, nrow(dat5), replace = TRUE, prob = c(0.7, 0.3))
train <- dat5[ind==1,]
test <- dat5[ind==2,]

svm <- svm(사상자수~., train, kernel="radial", cost=1, gamma=0.5)
p <- predict(svm, newdata = test)
table(round(p), test$사상자수)
sum(round(p) == test) / nrow(test)

# 2가 예측이 부족해서 2에 영향을 주는 걸 좀 찾아볼라고 한다.----
dat5 <- filter(dat, 사상자수 == 1 | 사상자수 == 2) %>% 
  # dplyr::select(주야, 요일, 사상자수, 중상자수, 부상신고자수,
  #               발생지시도, 발생지시군구, 사고유형_대분류,
  #               사고유형_중분류, 법규위반, 도로형태_대분류, 도로형태, 
  #               당사자종별_1당_대분류, 당사자종별_2당_대분류)
  dplyr::select(주야, 요일, 사상자수, 중상자수, 부상신고자수)
    
ind <- sample(1:2, nrow(dat5), replace = TRUE, prob = c(0.7, 0.3))
train <- dat5[ind==1,]
test <- dat5[ind==2,]

svm <- svm(사상자수~., dat5)
p <- predict(svm, newdata = test)
table(round(p), test$사상자수)
sum(round(p) == test) / nrow(test)


# nnet ----
library(nnet)

dat5 <- dplyr::select(dat, 사상자수, 중상자수, 부상신고자수)

ind <- sample(1:2, nrow(dat5), replace = TRUE, prob = c(0.7, 0.3))
train <- dat5[ind==1,]
test <- dat5[ind==2,]

nne <- nnet(사상자수~., data=train, size = 10)

?nnet

p <- predict(nne, newdata = test)
table(round(p), test$사상자수)
sum(round(p) == test) / nrow(test)

# randomForest ----
dat5 <- dplyr::select(dat, 사상자수, 중상자수, 부상신고자수)

ind <- sample(1:2, nrow(dat5), replace = TRUE, prob = c(0.7, 0.3))
train <- dat5[ind==1,]
test <- dat5[ind==2,]

rF <- randomForest::randomForest(사상자수~., data=train)
p <- predict(rF, newdata = test)
table(round(p), test$사상자수)
sum(round(p) == test) / nrow(test)

# svm ----
dat5 <- dplyr::select(dat, 사상자수, 중상자수, 부상신고자수)

ind <- sample(1:2, nrow(dat5), replace = TRUE, prob = c(0.7, 0.3))
train <- dat5[ind==1,]
test <- dat5[ind==2,]

sv <- svm(사상자수~., data=train, kernel = "linear")
sv <- svm(사상자수~., data=train, kernel = "radial")
sv <- svm(사상자수~., data=train, kernel = "polynomial")
p <- predict(sv, newdata = test)
a = table(round(p), test$사상자수)
sum(round(p) == test) / nrow(test)

# Naive Bayes Model----
dat5 <- dplyr::select(dat, 사상자수, 중상자수, 부상신고자수)

ind <- sample(1:2, nrow(dat5), replace = TRUE, prob = c(0.7, 0.3))
train <- dat5[ind==1,]
test <- dat5[ind==2,]

nb <- naiveBayes(사상자수~., data=train)
summary(nb)
p <- predict(nb, newdata = test)
table(round(p), test$사상자수)
sum(round(p) == test) / nrow(test)

# Adaptive Boosting
install.packages("adabag")
library(adabag)
dat5 <- dplyr::select(dat, 사상자수, 중상자수, 부상신고자수)

ind <- sample(1:2, nrow(dat5), replace = TRUE, prob = c(0.7, 0.3))
train <- dat5[ind==1,]
test <- dat5[ind==2,]

bt <- boosting(사상자수~., data=train,  mfinal = 100,control = rpart.control(maxdepth = 1))
p <- predict(bt, newdata = test)
table(round(p), test$사상자수)
sum(round(p) == test) / nrow(test)


# sse 계산 ----
nrow(a)
ncol(a)







