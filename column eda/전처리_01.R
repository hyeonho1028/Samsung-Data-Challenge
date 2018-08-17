setwd('D:/samsung_ai/test/')
dat <- read.csv('dataSet.csv')
# 경상자수

dat <- dplyr::select(dat, c(주야,요일,사망자수,사상자수,중상자수,경상자수,부상신고자수,
                              발생지시도,발생지시군구,사고유형_대분류,사고유형_중분류,
                              법규위반,도로형태_대분류,도로형태,당사자종별_1당_대분류,
                              당사자종별_2당_대분류))


table(dat$법규위반)
library(dplyr)
dat %>% group_by(발생지시군구) %>% summarise(사망자수 = n_distinct(사망자수),
                                   중상자수 = n_distinct(중상자수),
                                   경상자수 = n_distinct(경상자수),
                                   사상자수 = n_distinct(사상자수),
                                   부상신고자수 = n_distinct(부상신고자수))

for (i in length(levels(dat$당사자종별_2당_대분류)):1)
{
  if( levels(dat$당사자종별_2당_대분류)[i] != '승용차' &
      levels(dat$당사자종별_2당_대분류)[i] != '승합차' &
      levels(dat$당사자종별_2당_대분류)[i] != '화물차')
  {
    levels(dat$당사자종별_2당_대분류)[i] <- '없음'
  }
}



for (i in length(levels(dat$당사자종별_1당_대분류)):1)
{
  if( levels(dat$당사자종별_1당_대분류)[i] != '건설기계' &
      levels(dat$당사자종별_1당_대분류)[i] != '승용차' &
      levels(dat$당사자종별_1당_대분류)[i] != '승합차' &
      levels(dat$당사자종별_1당_대분류)[i] != '화물차')
  {
    levels(dat$당사자종별_1당_대분류)[i] <- '불명'
  }
}



for (i in length(levels(dat$발생지시도)):1)
{
  if( levels(dat$발생지시도)[i] != '강원' &
      levels(dat$발생지시도)[i] != '경기' &
      levels(dat$발생지시도)[i] != '경남' &
      levels(dat$발생지시도)[i] != '경북' &
      levels(dat$발생지시도)[i] != '서울' &
      levels(dat$발생지시도)[i] != '인천' &
      levels(dat$발생지시도)[i] != '전남' &
      levels(dat$발생지시도)[i] != '전북' &
      levels(dat$발생지시도)[i] != '충남' )
  {
    levels(dat$발생지시도)[i] <- '불명'
  }
}

for (i in length(levels(dat$도로형태)):1)
{
  if( levels(dat$도로형태)[i] != '교차로내' &
      levels(dat$도로형태)[i] != '교차로부근' &
      levels(dat$도로형태)[i] != '기타단일로' &
      levels(dat$도로형태)[i] != '터널안' )
  {
    levels(dat$도로형태)[i] <- '불명'
  }
}


for (i in length(levels(dat$도로형태_대분류)):1)
{
  if( levels(dat$도로형태_대분류)[i] != '고가도로위' &
      levels(dat$도로형태_대분류)[i] != '교차로' &
      levels(dat$도로형태_대분류)[i] != '단일로' &
      levels(dat$도로형태_대분류)[i] != '지하도로내' )
  {
    levels(dat$도로형태_대분류)[i] <- '기타/불명'
  }
}
table(dat$도로형태_대분류)




for (i in length(levels(dat$법규위반)):1)
{
  if( levels(dat$법규위반)[i] != '과속' &
      levels(dat$법규위반)[i] != '신호위반' &
      levels(dat$법규위반)[i] != '안전거리 미확보' &
      levels(dat$법규위반)[i] != '안전운전 의무 불이행' &
      levels(dat$법규위반)[i] != '중앙선 침범')
  {
    levels(dat$법규위반)[i] <- '없음'
  }
}




for(i in 1:length(dat))
{
  dat[,i] <- as.numeric(dat[,i])
}

cor(dat)


ind <- sample(1:2, nrow(dat), replace = TRUE, prob = c(0.75, 0.25))
train <- dat[ind==1,]
test <- dat[ind==2,]
lm <- lm(사망자수~발생지시군구+사고유형_대분류, dat)
p <- predict(lm, newdata = test)
print(sum((p-test$사망자수)^2))
car::vif(lm)
table(round(p), test$사망자수)

lm <- lm(사망자수~sqrt(사상자수)+사상자수, dat)
p <- predict(lm, newdata = test)
print(sum((p-test$사망자수)^2))
car::vif(lm)
table(round(p), test$사망자수)


names(dat)
a=aov(사상자수~발생지시도+발생지시군구+사고유형_대분류+사고유형_중분류+
        법규위반+도로형태+당사자종별_1당_대분류+
        당사자종별_2당_대분류, dat )
summary(a)

a=aov(경상자수~., dat )
summary(a)



ind <- sample(1:2, nrow(dat), replace = TRUE, prob = c(0.7, 0.3))
train <- dat[ind==1,]
test <- dat[ind==2,]
lm <- lm(사망자수~중상자수+경상자수+부상신고자수, dat)
p <- predict(lm, newdata = test)
print(sum((p-test$사망자수)^2))
car::vif(lm)
table(round(p), test$사망자수)


svm <- e1071::svm(부상신고자수~중상자수+경상자수+사상자수, dat)
p <- predict(svm, newdata = test)
print(sum((p-test$부상신고자수)^2))
car::vif(lm)
table(round(p), test$부상신고자수)







train <- dat[dat$발생년<=2016,]
test <- dat[!dat$발생년<=2016,]
train <- dplyr::select(train, c(주야,요일,사망자수,사상자수,중상자수,경상자수,부상신고자수,
                       발생지시도,발생지시군구,사고유형_대분류,사고유형_중분류,
                       법규위반,도로형태_대분류,도로형태,당사자종별_1당_대분류,
                       당사자종별_2당_대분류))
test <- dplyr::select(test, c(주야,요일,사망자수,사상자수,중상자수,경상자수,부상신고자수,
                       발생지시도,발생지시군구,사고유형_대분류,사고유형_중분류,
                       법규위반,도로형태_대분류,도로형태,당사자종별_1당_대분류,
                       당사자종별_2당_대분류))

model <- lm(사망자수~사상자수, train)
p <- predict(model, newdata = test)
sum((p-test$사망자수)^2)
p


model <- randomForest::randomForest(사망자수~사상자수, train)
model <- e1071::svm(사망자수~사상자수, train)
model <- lm(사망자수~사상자수, train)
model <- nnet::nnet(사망자수~중상자수+경상자수+부상신고자수, train, size = 10)
p <- predict(model, newdata = test)
sum((p-test$사망자수)^2)

model <- randomForest::randomForest(부상신고자수~사고유형_대분류, train)
model <- e1071::svm(부상신고자수~사고유형_대분류, train)
model <- lm(부상신고자수~사고유형_대분류, train)
model <- nnet::nnet(부상신고자수~사고유형_대분류, train, size = 100)
p <- predict(model, newdata = test)
sum((p-test$부상신고자수)^2)

model <- randomForest::randomForest(중상자수~사망자수+부상신고자수, train)
model <- e1071::svm(중상자수~사망자수+부상신고자수, train)
model <- lm(중상자수~사망자수+부상신고자수, train)
model <- nnet::nnet(중상자수~사망자수+부상신고자수, train, size = 100)
p <- predict(model, newdata = test)
sum((p-test$중상자수)^2)


model <- randomForest::randomForest(경상자수~중상자수+사망자수+부상신고자수, train)
model <- e1071::svm(경상자수~중상자수+사망자수+부상신고자수, train)
model <- lm(경상자수~중상자수+사망자수+부상신고자수, train)
model <- nnet::nnet(경상자수~중상자수+사망자수+부상신고자수, train, size = 100)
p <- predict(model, newdata = test)
sum((p-test$경상자수)^2)

# 범주형 변수----
ind <- sample(1:2, nrow(dat), replace = TRUE, prob = c(0.7, 0.3))
train <- dat[ind==1,]
test <- dat[ind==2,]

train$법규위반 <- as.character(train$법규위반)
train <- dplyr::filter(train, 법규위반 %in% c("신호위반", "중앙선 침범"))


a = head(train[train$법규위반 == "안전운전 의무 불이행",], 1700)
train <- rbind(train, a)



table(train$법규위반)
train$법규위반 <- as.factor(train$법규위반)

model <- nnet::multinom(법규위반~사상자수, train)
p <- predict(model, newdata = test)
table(p, factor(test$법규위반))
caret::confusionMatrix(p, factor(test$법규위반))
summary(model)

table(p)

table(train$법규위반)
table(test$법규위반)






train$주야 <- as.factor(train$주야)
train$요일 <- as.factor(train$요일)
train$발생지시도 <- as.factor(train$발생지시도)
train$발생지시군구 <- as.factor(train$발생지시군구)
train$사고유형_대분류 <- as.factor(train$사고유형_대분류)
train$사고유형_중분류 <- as.factor(train$사고유형_중분류)
train$법규위반 <- as.factor(train$법규위반)
train$도로형태_대분류 <- as.factor(train$도로형태_대분류)
train$도로형태 <- as.factor(train$도로형태)
train$당사자종별_1당_대분류 <- as.factor(train$당사자종별_1당_대분류)
train$당사자종별_2당_대분류 <- as.factor(train$당사자종별_2당_대분류)


model <- nnet::multinom(당사자종별_2당_대분류~사상자수+경상자수+중상자수+사망자수+부상신고자수, train)
p <- predict(model, newdata = test)
caret::confusionMatrix(p, test$당사자종별_2당_대분류)





model <- glm(주야~발생지시군구, train, family = binomial(link="logit"))
p <- predict(model, newdata = test, type = "response")
caret::confusionMatrix(factor(as.numeric(p>0.5)), factor(as.numeric(test$주야)-1))

model <- randomForest::randomForest(주야~발생지시도, train)
p <- predict(model, newdata = test, type = "response")
caret::confusionMatrix(p, test$주야)

model <- e1071::svm(주야~발생지시군구, train)
p <- predict(model, newdata = test, type = "response")
caret::confusionMatrix(p, test$주야)









model_mean = matrix()
for ( i in 1:100)
{
  #train1$사상자수 <- ifelse(train1$사상자수 >= 10, NA, train1$사상자수)
  #train1 <- na.omit(train1)
  model <- lm(사상자수~사망자수, train2)
  p <- predict(model, newdata = test)
  print(sum((p-test$사상자수)^2))
  model_mean = rbind(model_mean, sum((p-test$사상자수)^2))
}
mean(model_mean, na.rm  = TRUE)

plot(test$사상자수,p,xlab='관측치',ylab='예측치')
abline(0,1)



