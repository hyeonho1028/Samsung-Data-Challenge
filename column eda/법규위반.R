setwd('D:/samsung_ai/test/')
dat <- read.csv('dataSet.csv', stringsAsFactors = F)
test_dat <- read.csv('test_kor.csv', stringsAsFactors = F)
result <- read.csv('result_kor.csv')

dat <- dplyr::select(dat, c(주야,요일,사망자수,사상자수,중상자수,경상자수,부상신고자수,
                              발생지시도,발생지시군구,사고유형_대분류,사고유형_중분류,
                              법규위반,도로형태_대분류,도로형태,당사자종별_1당_대분류,
                              당사자종별_2당_대분류))


ind <- sample(1:2, nrow(dat), replace = TRUE, prob = c(0.85, 0.15))
train <- dat[ind==1,]
test <- dat[ind==2,]

model <- nnet::multinom(법규위반~(사상자수^2), train)
p <- predict(model, newdata = test)
caret::confusionMatrix(p, factor(test$법규위반))

model <- randomForest::randomForest(법규위반~사상자수, train)
p <- predict(model, newdata = test)
caret::confusionMatrix(p, test$법규위반)

model <- e1071::naiveBayes(법규위반~사상자수, train)
p <- predict(model, newdata = test)
caret::confusionMatrix(p, test$법규위반)

# train1 <- filter(train, 법규위반 == "안전운전 의무 불이행")
# ind <- sample(1:2, nrow(train1), replace = TRUE, prob = c(0.15, 0.85))
# train1 <- train1[ind==1,]
# train <- filter(train, 법규위반 == "중앙선 침범") #| 법규위반 == "과속")
# train <- rbind(train1, train)
# table(train$법규위반)
# train$법규위반 <- as.factor(train$법규위반)
