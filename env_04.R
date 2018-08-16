setwd('D:/samsung_ai/test/')
dat <- read.csv('dataSet.csv')

dat5 <- dplyr::select(dat, 사상자수, 중상자수, 부상신고자수)

ind <- sample(1:2, nrow(dat5), replace = TRUE, prob = c(0.7, 0.3))
train <- dat5[ind==1,]
test <- dat5[ind==2,]

sv <- svm(사상자수~., data=train, kernel = "linear")
sv <- svm(사상자수~., data=train, kernel = "radial")
p <- predict(sv, newdata = test)

sum((p-test$사상자수)^2)
sum((round(p)-test$사상자수)^2)


a = table(round(p), test$사상자수)
sum(round(p) == test) / nrow(test)


for (i in 1:100)
{
  set.seed(i)
  ind <- sample(1:2, nrow(dat5), replace = TRUE, prob = c(0.7, 0.3))
  train <- dat5[ind==1,]
  test <- dat5[ind==2,]
  
  lm <- lm(사상자수~., data=train)
  p <- predict(lm, newdata = test)
  print(sum((p-test$사상자수)^2))
}

