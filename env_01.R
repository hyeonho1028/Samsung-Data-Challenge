if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(xlsx)){install.packages("xlsx"); library(xlsx)}
if(!require(randomForest)){install.packages("randomForest"); library(randomForest)}
if(!require(plyr)){install.packages("plyr"); library(plyr)}

# 기초데이터 탐색 ----
setwd('D:/samsung_ai/test/')
dat <- read.csv('dataSet.csv')

names(dat)
str(dat)
attach(dat)

model1 <- lm(사망자수 ~ . , dat)
model1_1 <- step(model1, direction = "both")
model1_2 <- step(model1, direction = "backward")
model1_3 <- step(model1, direction = "forward")

summary(model1_1)
summary(model1_2)
summary(model1_3)


# 지역별 군집화가 필요하다고 판단 ----
head(dat)
names(dat)


dat %>%
  group_by(발생지시도, 발생지시군구) %>%
  summarize(사망자수평균 = mean(사망자수),
            사고자수평균 = mean(사상자수),
            중상자수평균 = mean(중상자수),
            경상자수평균 = mean(경상자수),
            부상신고자수평균 = mean(부상신고자수))

dat %>%
  group_by(요일) %>%
  summarize(사망자수평균 = mean(사망자수),
                  사고자수평균 = mean(사상자수),
                  중상자수평균 = mean(중상자수),
                  경상자수평균 = mean(경상자수),
                  부상신고자수평균 = mean(부상신고자수))

dat %>% 
  filter(발생지시군구=='화성시') %>% 
  select(경상자수) %>% as.vector() %>% sum() 
  
dat %>% 
  filter(사상자수>=19) %>% 
  select(주야,발생지시도,발생지시군구,  사상자수)
  


# 다중회귀 ----
dat_1 <- select(dat, c(주야, 요일, 사망자수, 사상자수, 중상자수, 경상자수,
                      부상신고자수, 발생지시도, 발생지시군구, 사고유형_대분류,
                      사고유형_중분류, 법규위반, 도로형태_대분류, 
                      도로형태, 당사자종별_1당_대분류, 당사자종별_2당_대분류))

ind <- sample(1:2, nrow(dat_1), prob = c(0.8, 0.2), replace = TRUE)
# replace = TRUE 복원추출임

attach(dat_1)


ind[ifelse(사고유형_중분류 %in% 
                     names(table(사고유형_중분류)[table(사고유형_중분류)==1]),
           TRUE,FALSE)] = 1
ind[ifelse(법규위반 %in% 
                 names(table(법규위반)[table(법규위반)==1]),
                   TRUE,FALSE)] = 1
ind[ifelse(당사자종별_1당_대분류 %in% 
                   names(table(당사자종별_1당_대분류)[table(당사자종별_1당_대분류)==1]),
                   TRUE,FALSE)] = 1



trainingset<-dat_1[ind==1,]
testset<-dat_1[ind==2,]


md1 <- lm(사망자수~., trainingset)
md1_step <- step(md1, direction="both")
md1_step <- step(md1, direction="backward")


summary(md1)
p<-predict(md1_step, newdata = testset)
table(p, testset$사망자수)

install.packages("caret")
library(caret)
confusionMatrix(p, testset$사망자수)


install.packages("car")
require(car)
vif(md1)
vif(md1) > 3




#랜덤포레스트----
md2 <- randomForest(사망자수~., trainingset)
























#cross validation, using rf to predict sepal.length
k = 2

dat_1$id <- sample(1:k, nrow(dat_1), replace = TRUE)
list <- 1:k
prediction <- data.frame()
testsetCopy <- data.frame()

progress.bar <- create_progress_bar("text")
progress.bar$init(k)

for(i in 1:k){
  # remove rows with id i from dataframe to create training set
  # select rows with id i to create test set
  trainingset <- subset(dat_1, id %in% list[-i])
  testset <- subset(dat_1, id %in% c(i))
  #데이터를 5등분하고 한번 뽑은 test data가 다시 train 으로 가지 않도록 5등분 합니다.
  
  #run a random forest model
  mymodel <- lm(사망자수 ~ ., data = trainingset)
  #랜덤 포레스트 알고리즘으로 꽃받침의 길이를 나머지 데이터로 예측하는 모델을 만듭니다.
  
  #remove response column 1, Sepal.Length
  temp <- as.data.frame(predict(mymodel, testset))
  
  # append this iteration's predictions to the end of the prediction data frame
  prediction <- rbind(prediction, temp)
  
  # append this iteration's test set to the test set copy data frame
  # keep only the Sepal Length Column
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,1]))
  
  progress.bar$step()
}

summary(prediction)
table(round(prediction),testset$사망자수)


# add predictions and actual Sepal Length values
result <- cbind(prediction, testsetCopy[, 1])
names(result) <- c("Predicted", "Actual")
result$Difference <- abs(result$Actual - result$Predicted)


# As an example use Mean Absolute Error as Evalution
summary(result$Difference)








md1 <- lm(사망자수~., trainData)
# summary(md1)
p<-predict(md1, newdata = testData)

levels(trainData$사고유형_중분류)
sum(trainData$사고유형_중분류=="경보기무시")
levels(testData$사고유형_중분류)
sum(testData$사고유형_중분류=="경보기무시")






md1 <- lm(사망자수~., dat_3)
# summary(md1)
p<-predict(md1, newdata = testData)




# 범주형 ----
model<-glm(CHCOCNCR~.,trainData,family="binomial")
summary(model)
p<-predict(model,newdata = testData,type = "response")
round(p)
table(round(p),testData$CHCOCNCR)











# 랜덤포레스트 ----
dat_fri <- filter(dat, 요일 == '금')
dat_fri_train <- dat_fri 
dat_fri_test <- 


set.seed(1234)
ind<-sample(2,nrow(data1),replace = TRUE,prob=c(0.7,0.3))
# replace = TRUE 복원추출임
trainData<-data1[ind==1,]
testData<-data1[ind==2,]




setwd('D:/samsung_ai/test/')
dat <- read.csv('dataSet.csv')


dat_1 <- select(dat, c(주야, 요일, 사망자수, 사상자수, 중상자수, 경상자수,
                         부상신고자수, 발생지시도, 발생지시군구, 사고유형_대분류,
                         사고유형_중분류, 법규위반, 도로형태_대분류, 
                         도로형태, 당사자종별_1당_대분류, 당사자종별_2당_대분류))


dat_2 <- select(dat_1, c(주야, 요일, 사망자수, 사상자수, 중상자수, 경상자수,
                           부상신고자수))


ind <- sample(1:2, nrow(dat_2), prob = c(0.7, 0.3), replace = TRUE)
trainingset <- dat_2[ind==1,]
testset <- dat_2[ind==2,]


md1 <- lm(사망자수~., trainingset)
p <- predict(md1, newdata = testset)
table(round(p), testset$사망자수)

confusionMatrix(factor(round(p)), factor(testset$사망자수))


dat_3 <- select(dat_1, c(주야, 요일, 사망자수, 중상자수, 부상신고자수,
                           발생지시도))

ind <- sample(1:2, nrow(dat_3), prob = c(0.75, 0.25), replace = TRUE)
trainingset <- dat_3[ind==1,]
testset <- dat_3[ind==2,]

md1 <- lm(사망자수~., trainingset)
p <- predict(md1, newdata = testset)
table(round(p), testset$사망자수)

confusionMatrix(factor(round(p)), factor(testset$사망자수))





dat_4 <- select(dat_1, c(요일, 사망자수, 사상자수, 경상자수, 중상자수))

ind <- sample(1:2, nrow(dat_4), prob = c(0.7, 0.3), replace = TRUE)
trainingset <- dat_4[ind==1,]
testset <- dat_4[ind==2,]

md1 <- lm(사상자수~., trainingset)
p <- predict(md1, newdata = testset)
table(round(p), testset$사상자수)


confusionMatrix(factor(round(p)), factor(testset$사상자수))

length(levels(factor(round(p))))
length(levels(factor(testset$사상자수)))








install.packages("dplyr")
library(dplyr)
install.packages("car")
library(car)
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)
install.packages("randomForest")
library(randomForest)
install.packages("tree")
library(tree)
install.packages("rpart")
library(rpart)

setwd('D:/samsung_ai/test/')
dat <- read.csv('dataSet.csv')

dat_1 <- select(dat, 주야, 요일, 사망자수, 사상자수, 중상자수, 경상자수,
                부상신고자수, 발생지시도, 발생지시군구, 사고유형_대분류,
                사고유형_중분류, 법규위반, 도로형태_대분류, 도로형태, 
                당사자종별_1당_대분류, 당사자종별_2당_대분류)

# 중회귀분석 ----



dat_2 <- select(dat_1, 주야, 요일, 사망자수, 중상자수, 
                발생지시도, 발생지시군구, 사고유형_대분류,
                사고유형_중분류, 법규위반, 도로형태_대분류, 도로형태, 
                당사자종별_1당_대분류, 당사자종별_2당_대분류)


ind <- sample(1:2, nrow(dat_2), replace = TRUE, prob = c(0.7, 0.3))
train <- dat_2[ind==1,]
test <- dat_2[ind==2,]

model <- lm(사망자수~., train)
mds <- step(model, direction = "both")

summary(mds)
table(dat_2$사고유형_대분류)

p <- predict(model, newdata = test)
table(round(p), test$사망자수)

confusionMatrix(factor(round(p)), factor(test$사망자수))





# 중회귀분석 결과를 이용한 재 모델링 ----
dat_2_1 <- select(dat_1, 주야, 요일, 사망자수, 중상자수, 
                  사고유형_대분류,
                  법규위반,   
                  당사자종별_1당_대분류, 당사자종별_2당_대분류)

table(dat_2_1$사고유형_대분류)
table(dat_2_1$법규위반)
table(dat_2_1$당사자종별_1당_대분류)
table(dat_2_1$당사자종별_2당_대분류)

dat_2_1 <- filter(dat_2_1, 법규위반 != '과로')
dat_2_1 <- filter(dat_2_1, 법규위반 != '보행자과실')
dat_2_1 <- filter(dat_2_1, 법규위반 != '진로양보 의무 불이행')
dat_2_1 <- filter(dat_2_1, 법규위반 != '통행우선 순위위반')
dat_2_1$법규위반 <- droplevels(dat_2_1$법규위반)

dat_2_1 <- filter(dat_2_1, 당사자종별_1당_대분류 != '개인형이동수단(PM)')
dat_2_1$당사자종별_1당_대분류 <- droplevels(dat_2_1$당사자종별_1당_대분류)

ind <- sample(1:2, nrow(dat_2_1), replace = TRUE, prob = c(0.7, 0.3))
train <- dat_2_1[ind==1,]
test <- dat_2_1[ind==2,]

model <- lm(사망자수~., train)
summary(model)

p <- predict(model, newdata = test)







# 다시한번 중회귀 적용 ----
dat_2_2 <- select(dat_1, 주야, 요일, 사망자수, 중상자수, 
                  사고유형_대분류,
                  당사자종별_2당_대분류)


ind <- sample(1:2, nrow(dat_2_2), replace = TRUE, prob = c(0.7, 0.3))
train <- dat_2_2[ind==1,]
test <- dat_2_2[ind==2,]

model <- lm(사망자수~., train)
summary(model)

p <- predict(model, newdata = test)










# 랜덤포레스트 모델링----

dat_3 <- select(dat_1, 요일, 사망자수, 발생지시도, 중상자수)



ind <- sample(1:2, nrow(dat_3), replace = TRUE, prob = c(0.7, 0.3))

train <- dat_3[ind==1,]

test <- dat_3[ind==2,]



model_2 <- randomForest(사망자수~., train)

p <- predict(model_2, newdata = test)

table(round(p), test$사망자수)



confusionMatrix(factor(round(p)), factor(test$사망자수))









# svm 모델링 ----

dat_4 <- select(dat_1, 요일, 사망자수, 발생지시도, 중상자수)



ind <- sample(1:2, nrow(dat_4), replace = TRUE, prob = c(0.7, 0.3))

train <- dat_4[ind==1,]

test <- dat_4[ind==2,]



model_3 <- svm(사망자수~., train, cost = 100, scale = FALSE)

p <- predict(model_3, newdata = test)

table(round(p), test$사망자수)



confusionMatrix(factor(round(p)), factor(test$사망자수))





# tree 모델링 ----

dat_5 <- select(dat_1, 요일, 사망자수, 발생지시도, 중상자수)



ind <- sample(1:2, nrow(dat_5), replace = TRUE, prob = c(0.7, 0.3))

train <- dat_5[ind==1,]

test <- dat_5[ind==2,]



model_4 <- tree(사망자수~., train)



model_4_1 <- cv.tree(model_4, FUN=prune.tree)

plot(model_4_1)







p <- predict(model_4, newdata = test)

table(round(p), test$사망자수)



confusionMatrix(factor(round(p)), factor(test$사망자수))





# rpart 모델링 ----

dat_6 <- select(dat_1, 요일, 사망자수, 발생지시도, 중상자수)



ind <- sample(1:2, nrow(dat_6), replace = TRUE, prob = c(0.7, 0.3))

train <- dat_6[ind==1,]

test <- dat_6[ind==2,]



model_5 <- rpart(사망자수~. , data=train, method="class")

printcp(model_5)





p <- predict(model_5, newdata = test)

table(round(p), test$사망자수)





# 아놔 ----

if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}

setwd('D:/samsung_ai/test/')
dat <- read.csv('dataSet.csv')
attach(dat)

par(mfrow = c(1, 5))
boxplot(사망자수)
boxplot(사상자수)
boxplot(중상자수)
boxplot(경상자수)
boxplot(부상신고자수)

cor(dat[,6:10])
pairs(~사망자수+사상자수+중상자수+경상자수+부상신고자수,data=dat, 
      main="Simple Scatterplot Matrix")


dat_1 <- select(dat, 주야, 요일, 사망자수, 중상자수, 
                발생지시도, 발생지시군구, 사고유형_대분류,
                사고유형_중분류, 법규위반, 도로형태_대분류, 도로형태, 
                당사자종별_1당_대분류, 당사자종별_2당_대분류)

ind <- sample(1:2, nrow(dat_1), replace = TRUE, prob = c(0.7, 0.3))
train <- dat_1[ind==1,]
test <- dat_1[ind==2,]

model <- lm(사망자수~., train)
mds <- step(model, direction = "both")
summary(mds)


# 변수 추가 제거 ----
dat_2 <- select(dat_1, 요일, 사망자수, 중상자수, 
                법규위반, 당사자종별_1당_대분류, 당사자종별_2당_대분류)

table(dat_2$법규위반)




ind <- sample(1:2, nrow(dat_2), replace = TRUE, prob = c(0.7, 0.3))
train <- dat_2[ind==1,]
test <- dat_2[ind==2,]

model <- lm(사망자수~., train)
mds <- step(model, direction = "both")

summary(mds)
summary(mds)$coefficients[,4][summary(mds)$coefficients[,4] < 0.15]

levels(dat_2$법규위반)
for (i in length(levels(dat_2$법규위반)):1)
{
  if( levels(dat_2$법규위반)[i] != '철길건널목 통과방법위반')
  {
    levels(dat_2$법규위반)[i] <- 'not 철길건널목 통과방법위반'
  }
}


levels(dat_2$당사자종별_2당_대분류)

for (i in length(levels(dat_2$당사자종별_2당_대분류)):1)
{
  if( levels(dat_2$당사자종별_2당_대분류)[i] != '농기계' &
      levels(dat_2$당사자종별_2당_대분류)[i] != '보행자' &
      levels(dat_2$당사자종별_2당_대분류)[i] != '열차' &
      levels(dat_2$당사자종별_2당_대분류)[i] != '원동기장치자전거' &
      levels(dat_2$당사자종별_2당_대분류)[i] != '이륜차' &
      levels(dat_2$당사자종별_2당_대분류)[i] != '자전거')
  {
    levels(dat_2$당사자종별_2당_대분류)[i] <- '없음'
  }
}

# 변수 조정 뒤 회귀분석 ----
dat_3 <- select(dat_2, 요일, 사망자수, 중상자수, 
                법규위반, 당사자종별_2당_대분류)


ind <- sample(1:2, nrow(dat_3), replace = TRUE, prob = c(0.7, 0.3))
train <- dat_3[ind==1,]
test <- dat_3[ind==2,]

model <- lm(사망자수~., train)
summary(model)

p <- predict(model, newdata = test)
table(round(p), test$사망자수)


table(test$당사자종별_2당_대분류)
table(train$당사자종별_2당_대분류)
table(test$법규위반)
table(train$법규위반)

a = confusionMatrix(factor(round(p)), factor(test$사망자수))
a$overall









