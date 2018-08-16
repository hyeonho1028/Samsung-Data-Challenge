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

train1 <- train[!train$주야=="야간",]
train2 <- train[train$주야=="야간",]
ind <- sample(1:2, nrow(train2), replace = TRUE, prob = c(0.85, 0.15))
train2 <- train2[ind==1,]
train <- rbind(train1, train2)



model <- randomForest::randomForest(주야~법규위반+당사자종별_1당_대분류, train)
p <- predict(model, newdata = test)
round(caret::confusionMatrix(p, test$주야)$overall,2)

train <- filter(train, 요일!="수")
model <- e1071::naiveBayes(요일~., train)
p <- predict(model, newdata = test)
round(caret::confusionMatrix(p, test$요일)$overall,2)

##


















#xg-boost
install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
require(xgboost)

t <- as(as.matrix(train), "dgCMatrix")



