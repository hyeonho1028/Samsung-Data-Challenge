setwd('D:/samsung_ai/test/')
dat <- read.csv('dataSet.csv')

cor(dat[,6:10])
dat5 <- dplyr::select(dat, 사상자수, 중상자수, 부상신고자수)

# 사망자수 ----
dat5 <- dat[,6:10]

set.seed(777)
ind <- sample(1:2, nrow(dat5), replace = TRUE, prob = c(0.7, 0.3))
train <- dat5[ind==1,]
test <- dat5[ind==2,]

lm <- lm(사망자수~사상자수+중상자수+부상신고자수, data=train)
p <- predict(lm, newdata = test)
print(sum((p-test$사상자수)^2))
car::vif(lm)






set.seed(777)

# 사상자수 ----
dat6 <- dplyr::select(dat, 사망자수, 경상자수, 사상자수, 중상자수, 부상신고자수)
ind <- sample(1:2, nrow(dat6), replace = TRUE, prob = c(0.7, 0.3))
train <- dat6[ind==1,]
test <- dat6[ind==2,]

lm <- lm(사상자수~중상자수+경상자수+부상신고자수, data=train)
p <- predict(lm, newdata = test)
print(sum((p-test$사상자수)^2))
car::vif(lm)
table(round(p), test$사상자수)

# 중상자수 ----
dat7 <- dplyr::select(dat, 사망자수, 사상자수, 경상자수, 중상자수, 부상신고자수)
ind <- sample(1:2, nrow(dat7), replace = TRUE, prob = c(0.7, 0.3))
train <- dat7[ind==1,]
test <- dat7[ind==2,]

lm <- lm(중상자수~사상자수 + 경상자수 + 부상신고자수, data=train)
p <- predict(lm, newdata = test)
print(sum((p-test$중상자수)^2))
car::vif(lm)
table(round(p), test$중상자수)

# 경상자수 ----
dat6 <- dplyr::select(dat, 사망자수, 사상자수, 중상자수, 경상자수, 부상신고자수)
ind <- sample(1:2, nrow(dat6), replace = TRUE, prob = c(0.7, 0.3))
train <- dat6[ind==1,]
test <- dat6[ind==2,]

lm <- lm(경상자수~사상자수+중상자수+부상신고자수, data=train)
p <- predict(lm, newdata = test)
print(sum((p-test$경상자수)^2))
car::vif(lm)
table(round(p), test$경상자수)

# 사망자수 ----
dat7 <- dplyr::select(dat, 사망자수, 사상자수, 중상자수, 경상자수, 부상신고자수)
ind <- sample(1:2, nrow(dat7), replace = TRUE, prob = c(0.7, 0.3))
train <- dat7[ind==1,]
test <- dat7[ind==2,]

lm <- lm(사망자수~sqrt(사상자수), data=train)
p <- predict(lm, newdata = test)
sum((p-test$사망자수)^2)
car::vif(lm)
table(round(p), test$사망자수)



# 부상신고자수 ----
dat6 <- dplyr::select(dat, 사망자수, 사상자수, 경상자수, 중상자수,  부상신고자수)
ind <- sample(1:2, nrow(dat6), replace = TRUE, prob = c(0.7, 0.3))
train <- dat6[ind==1,]
test <- dat6[ind==2,]

lm <- lm(부상신고자수~사상자수++경상자수, data=train)
p <- predict(lm, newdata = test)
print(sum((p-test$부상신고자수)^2))
car::vif(lm)
table(round(p), test$부상신고자수)


# 중상자수 범주형으로 예측하기 ----
dat6 <- dplyr::select(dat, -c(사상자수, 사망자수, 중상자수, 부상신고자수, 발생년, 발생년월일시,
                                  발생분, 발생위치X_UTMK, 발생위치Y_UTMK, 경도, 위도))
ind <- sample(1:2, nrow(dat6), replace = TRUE, prob = c(0.7, 0.3))
train <- dat6[ind==1,]
test <- dat6[ind==2,]


lm <- lm(경상자수~. , data=train)
lm <- step(lm, direction = "both")
summary(lm)
plot(lm)


p <- predict(lm, newdata = test)
print(sum((p-test$경상자수)^2))
car::vif(lm)
table(round(p), test$경상자수)

table(dat6$당사자종별_1당_대분류)




















# 이거 나중에 해볼꺼----
# k-fold cross vaildation function
dat6 <- dplyr::select(dat, 사망자수, 사상자수, 경상자수, 중상자수,  부상신고자수)

for( i in 1:k)
{
  training.rows <- sample(1:nrow(dat6), size=round(nrow(dat6)*0.7), replace=FALSE)
  training.set <- dat6[training.rows,]
  testing.set <- dat6[-training.rows,]
  training.lm <- lm(경상자수 ~ 사상자수+중상자수+부상신고자수, data=training.set) 
  testing.preds <- predict(training.lm, newdata=testing.set)
  print(sum((testing.preds-testing.set$경상자수)^2))
}

k = 10
dat6$id <- sample(1:k, nrow(dat6), replace = TRUE)
list <- 1:k
prediction <- data.frame()
testsetCopy <- data.frame()

for(i in 1:k){
  trainingset <- subset(dat6, id %in% list[-i])
  testset <- subset(dat6, id %in% c(i))
  mymodel <- lm(경상자수~사상자수+중상자수+부상신고자수, dat6 = trainingset)
  temp <- as.data.frame(predict(mymodel, testset[,-1]))
  prediction <- rbind(prediction, temp)
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,1]))
}

result <- cbind(prediction, testsetCopy[, 1])
names(result) <- c("Predicted", "Actual")
result$Difference <- abs(result$Actual - result$Predicted)

summary(result$Difference)





















