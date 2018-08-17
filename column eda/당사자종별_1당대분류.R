setwd('D:/samsung_ai/test/')
dat <- read.csv('dataSet.csv')
test_dat <- read.csv('test_kor.csv', stringsAsFactors = F)
result <- read.csv('result_kor.csv')

dat <- dplyr::select(dat, c(주야,요일,사망자수,사상자수,중상자수,경상자수,부상신고자수,
                              발생지시도,발생지시군구,사고유형_대분류,사고유형_중분류,
                              법규위반,도로형태_대분류,도로형태,당사자종별_1당_대분류,
                              당사자종별_2당_대분류))


ind <- sample(1:2, nrow(dat), replace = TRUE, prob = c(0.9, 0.1))
train <- dat[ind==1,]
test <- dat[ind==2,]

model <- nnet::multinom(당사자종별_1당_대분류~당사자종별_2당_대분류+법규위반+사상자수+도로형태, train)
p <- predict(model, newdata = test)
caret::confusionMatrix(p, test$당사자종별_1당_대분류)

model <- randomForest::randomForest(당사자종별_1당_대분류~당사자종별_2당_대분류+법규위반+사상자수+발생지시도, train) # 얘로 결정
p <- predict(model, newdata = test)
caret::confusionMatrix(p, test$당사자종별_1당_대분류)$overall

model <- e1071::naiveBayes(당사자종별_1당_대분류~당사자종별_2당_대분류+법규위반+사상자수+발생지시도, train) # 얘로 결정
p <- predict(model, newdata = test)
caret::confusionMatrix(p, test$당사자종별_1당_대분류)$overall

model <- e1071::naiveBayes(당사자종별_2당_대분류~중상자수+법규위반+도로형태+사고유형_대분류, train)
p <- predict(model, newdata = test)
caret::confusionMatrix(p, test$당사자종별_2당_대분류)

model <- randomForest::randomForest(당사자종별_2당_대분류~중상자수+법규위반+도로형태, train) #47퍼
model <- randomForest::randomForest(당사자종별_2당_대분류~중상자수+법규위반+도로형태+당사자종별_1당_대분류, train) #55퍼
model <- randomForest::randomForest(당사자종별_2당_대분류~중상자수+법규위반+도로형태+사고유형_중분류, train) #69퍼
model <- randomForest::randomForest(당사자종별_2당_대분류~중상자수+법규위반+도로형태+사고유형_대분류, train) #74퍼
p <- predict(model, newdata = test)
caret::confusionMatrix(p, test$당사자종별_2당_대분류)


