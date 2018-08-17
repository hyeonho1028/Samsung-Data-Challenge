if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(MASS)){install.packages("MASS"); library(MASS)}
if(!require(ridge)){install.packages("ridge"); library(ridge)}
if(!require(glmnet)){install.packages("glmnet"); library(glmnet)}
if(!require(lars)){install.packages("lars"); library(lars)}
if(!require(elasticnet)){install.packages("elasticnet"); library(elasticnet)}

setwd('D:/samsung_ai/test/')
dat <- read.csv('dataSet.csv')
attach(dat)

cor(dat[,6:10])


dat_1 <- select(dat, 주야, 요일, 사상자수, 중상자수, 부상신고자수,
                발생지시도, 발생지시군구, 사고유형_대분류,
                사고유형_중분류, 법규위반, 도로형태_대분류, 도로형태, 
                당사자종별_1당_대분류, 당사자종별_2당_대분류)

lm <- lm(사상자수~., dat_1)
lm1 <- step(lm, direction = "both")
summary(lm1)
table(dat_1$당사자종별_2당_대분류)
table(dat_1$도로형태)
# levels 평준화
summary(lm1)$coefficients[,4][summary(lm1)$coefficients[,4] <= 0.15]

# 모델링 ----------------------------------------------------------------------

dat_1 <- select(dat, 주야, 사상자수, 중상자수, 부상신고자수)

ind <- sample(1:2, nrow(dat_1), replace = TRUE, prob = c(0.7, 0.3))
train <- dat_1[ind==1,]
test <- dat_1[ind==2,]

lm <- lm(사상자수~., train)
p <- predict(lm, newdata = test)
table(round(p), test$사상자수)
sum(round(p) == test) / nrow(test)

lm <- linearRidge(사상자수~., train)
summary(lm)
p <- predict(lm, newdata = test)
table(round(p), test$사상자수)
sum(round(p) == test) / nrow(test)

lm <- lm.ridge(사상자수 ~ ., data = train, lambda = 10^seq(10, -2, length = 100))
summary(lm)
plot(lm)

# lasso ----
lm <- lars(dat_1[,c(1,3,4)], dat_1[,c(2), drop = FALSE], type = "lasso")
plot(lm)



# train, test sampling ----
ind <- sample(1:2, nrow(dat_1), replace = TRUE, prob = c(0.7, 0.3))
train <- dat_1[ind==1,]
test <- dat_1[ind==2,]

lm <- lm(사상자수~., train)
lm1 <- step(lm, direction = "both")
summary(lm1)

p <- predict(lm1, newdata = test)
table(round(p), test$사상자수)

# data science school ----
set.seed(0)
n_samples <- 30
x <- runif(n_samples)
x <- x[order(x)]
y <- cos(1.5 * pi * x) + rnorm(n_samples) * 0.1
X <- poly(x, 9, raw=T)
df <- data.frame(x, y)

model <- lm(y ~ poly(x, 9, raw=T), data=df)
xnew = seq(0, 1, length=1000)
plot(df)
lines(xnew, predict(model, data.frame(x=xnew)), type="l")
coefficients(model)















