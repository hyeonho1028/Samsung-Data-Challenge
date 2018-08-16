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

model <- randomForest::randomForest(중상자수~사망자수+부상신고자수+경상자수, train)
model <- randomForest::randomForest(중상자수~사망자수+부상신고자수, train)
model <- lm(중상자수~사망자수+부상신고자수+사상자수, train)
model <- lm(중상자수~사망자수+사상자수+부상신고자수, train)
model <- lm(중상자수~사상자수+사망자수+경상자수, train)
model <- lm(중상자수~사상자수+사망자수+경상자수+부상신고자수, train)
p <- predict(model, newdata = test)
sum((p-test$중상자수)^2)

model <- randomForest::randomForest(경상자수~사망자수+부상신고자수, train)
model <- randomForest::randomForest(경상자수~사망자수+부상신고자수+중상자수, train)
model <- lm(경상자수~사망자수+부상신고자수+사상자수, train)
model <- lm(경상자수~사망자수+사상자수+중상자수+부상신고자수, train)
model <- lm(경상자수~사망자수+사상자수+중상자수, train)
p <- predict(model, newdata = test)
sum((p-test$경상자수)^2)

model <- lm(부상신고자수~사망자수+중상자수+경상자수, train)
model <- lm(부상신고자수~사망자수+경상자수, train)
model <- lm(부상신고자수~사망자수+중상자수, train)
p <- predict(model, newdata = test)
sum((p-test$부상신고자수)^2)






