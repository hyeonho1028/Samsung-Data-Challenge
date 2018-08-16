setwd('D:/samsung_ai/test/')
dat <- read.csv('dataSet.csv')
test_dat <- read.csv('test_kor.csv', stringsAsFactors = F)
result <- read.csv('result_kor.csv')

dat <- dplyr::select(dat, c(주야,요일,사망자수,사상자수,중상자수,경상자수,부상신고자수,
                              발생지시도,발생지시군구,사고유형_대분류,사고유형_중분류,
                              법규위반,도로형태_대분류,도로형태,당사자종별_1당_대분류,
                              당사자종별_2당_대분류))

ind <- sample(1:2, nrow(dat), replace = TRUE, prob = c(0.8, 0.2))
train <- dat[ind==1,]
test <- dat[ind==2,]




model <- nnet::multinom(발생지시도~발생지시군구, train, MaxNWts = 3808)
p <- predict(model, newdata = test)
caret::confusionMatrix(p, test$발생지시도)

model <- randomForest::randomForest(발생지시도~발생지시군구, train, proximity = T)
p <- predict(model, newdata = test)
caret::confusionMatrix(p, test$발생지시도)

model <- randomForest::randomForest(발생지시도~법규위반+사고유형_중분류+당사자종별_1당_대분류+당사자종별_2당_대분류, train) # 얘로 결정
p <- predict(model, newdata = test)
caret::confusionMatrix(p, test$발생지시도)$overall
randomForest::importance(model)

model <- e1071::naiveBayes(발생지시도~법규위반+사고유형_중분류+당사자종별_1당_대분류+당사자종별_2당_대분류, train) # 얘로 결정
p <- predict(model, newdata = test)
caret::confusionMatrix(p, test$발생지시도)$overall




model <- e1071::naiveBayes(발생지시군구~사상자수, train)
p <- predict(model, newdata = test)
round(caret::confusionMatrix(p, test$발생지시군구)$overall,2)

model <- e1071::naiveBayes(발생지시군구~발생지시도, train)
p <- predict(model, newdata = test)
round(caret::confusionMatrix(p, test$발생지시군구)$overall,2)

model <- randomForest::randomForest(발생지시군구~발생지시도, train)
p <- predict(model, newdata = test)
round(caret::confusionMatrix(p, test$발생지시군구)$overall,2)








