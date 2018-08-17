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

model <- e1071::naiveBayes(도로형태_대분류~도로형태, train)
p <- predict(model, newdata = test)
caret::confusionMatrix(p, test$도로형태_대분류)

model <- randomForest::randomForest(도로형태_대분류~사상자수+법규위반, train)
p <- predict(model, newdata = test)
caret::confusionMatrix(p, test$도로형태_대분류)

model <- e1071::naiveBayes(도로형태~사상자수+법규위반+도로형태_대분류, train)
p <- predict(model, newdata = test)
caret::confusionMatrix(p, test$도로형태)

model <- randomForest::randomForest(도로형태~사상자수+법규위반+도로형태_대분류, train)
p <- predict(model, newdata = test)
caret::confusionMatrix(p, test$도로형태)

# train1 <- filter(train, 당사자종별_1당_대분류 == "승용차")
# ind <- sample(1:2, nrow(train1), replace = TRUE, prob = c(0.5, 0.5))
# train1 <- train1[ind==1,]
# train <- filter(train, 당사자종별_1당_대분류 == "승합차" | 당사자종별_1당_대분류 == "이륜차" | 
#                   당사자종별_1당_대분류 == "화물차")
# train <- rbind(train1, train)
# table(train$당사자종별_1당_대분류)
# train$당사자종별_1당_대분류 <- as.factor(train$당사자종별_1당_대분류)
# test$당사자종별_1당_대분류 <- as.factor(test$당사자종별_1당_대분류)
