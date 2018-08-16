setwd('D:/samsung_ai/test/')
dat <- read.csv('dataSet.csv')
test_dat <- read.csv('test_kor.csv', stringsAsFactors = F)
result <- read.csv('result_kor.csv')

#범주형변수 ""값을 NA로 대치
for(i in 1:ncol(test_dat))
{
  test_dat[,i] <- ifelse(test_dat[,i] == "", "NA", test_dat[,i])
}

# for(i in 1:2)
# {
#   levels(test_dat[i])[1] <- NA
# }
# for(i in 8:16)
# {
#   levels(test_dat[i])[1] <- NA
#   #levels(test_dat[,i]) <- levels(dat[,i])
# }

# 이상값 제거
dat <- dplyr::filter(dat, 사망자수 < 5)
dat <- dplyr::filter(dat, 사상자수 < 15)
dat <- dplyr::filter(dat, 중상자수 < 10)
dat <- dplyr::filter(dat, 경상자수 < 10)
dat <- dplyr::filter(dat, 부상신고자수 < 4)



#----
dat <- dplyr::select(dat, c(주야,요일,사망자수,사상자수,중상자수,경상자수,부상신고자수,
                              발생지시도,발생지시군구,사고유형_대분류,사고유형_중분류,
                              법규위반,도로형태_대분류,도로형태,당사자종별_1당_대분류,
                              당사자종별_2당_대분류))
col_names = names(dat)
for(i in 1:2)
{
  dat[,i] <- as.factor(dat[,i])
}
for(i in 8:16)
{
  dat[,i] <- as.factor(dat[,i])
}

for(i in c(1, 2, 8:16))
{
  levels(test_dat[,i]) <- levels(dat[,i])
}

levels(test_dat$도로형태) <- levels(dat$도로형태)


for(i in 1:nrow(test_dat))
{
  if(c("사망자수") %in% col_names[is.na(test_dat[i,])])
  {
    if(c("사상자수") %in% col_names[is.na(test_dat[i,])] == 0)
    {
      lm_model <- lm(사망자수~사상자수, dat)
      pred <- predict(lm_model, newdata = test_dat[i,])
      test_dat[i,"사망자수"] = pred
    }else
    {
      if(sum(c("발생지시군구", "사고유형_대분류") %in% col_names[is.na(test_dat[i,])]) == 0)
      {
        lm_model <- lm(사망자수~발생지시군구+사고유형_대분류, dat)
        pred <- predict(lm_model, newdata = test_dat[i,])
        test_dat[i,"사망자수"] = pred
      }else
      {
        if(sum(c("사고유형_중분류", "도로형태") %in% col_names[is.na(test_dat[i,])]) == 0)
        {
          lm_model <- lm(사망자수~사고유형_중분류+도로형태, dat)
          pred <- predict(lm_model, newdata = test_dat[i,])
          test_dat[i,"사망자수"] = pred  
        }else
        {
          lm_model <- lm(사망자수~중상자수+경상자수+부상신고자수, dat)
          pred <- predict(lm_model, newdata = test_dat[i,])
          test_dat[i,"사망자수"] = pred
        }
        
      }
      
    }
  }
  if("부상신고자수" %in% col_names[is.na(test_dat[i,])])
  {
    lm_model <- lm(부상신고자수~사망자수, dat)
    pred <- predict(lm_model, newdata = test_dat[i,])
    test_dat[i,"부상신고자수"] = pred
  }
  if("중상자수" %in% col_names[is.na(test_dat[i,])])
  {
    rf_model <- randomForest::randomForest(중상자수~사망자수+부상신고자수, dat)
    pred <- predict(rf_model, newdata = test_dat[i,])
    test_dat[i,"중상자수"] = pred
  }
  if("경상자수" %in% col_names[is.na(test_dat[i,])])
  {
    rf_model <- randomForest::randomForest(경상자수~중상자수+사망자수+부상신고자수, dat)
    pred <- predict(rf_model, newdata = test_dat[i,])
    test_dat[i,"경상자수"] = pred
  }
  if("사상자수" %in% col_names[is.na(test_dat[i,])])
  {
    lm_model <- lm(사상자수~부상신고자수+중상자수+사망자수+경상자수, dat)
    pred <- predict(lm_model, newdata = test_dat[i,])
    test_dat[i,"사상자수"] = pred
  }
}  
  

for(i in 1:nrow(test_dat))
{
  if("법규위반" %in% col_names[is.na(test_dat[i,])])
  {
    rf_model <- randomForest::randomForest(법규위반~사상자수, dat)
    pred <- predict(rf_model, newdata = test_dat[i,])
    test_dat[i,"법규위반"] = pred
  }
}



for(i in 1:nrow(test_dat))
{
  if("도로형태_대분류" %in% col_names[is.na(test_dat[i,])])
  {
    if(!"도로형태" %in% col_names[is.na(test_dat[i,])])
    {
      rf_model <- randomForest::randomForest(도로형태_대분류~도로형태, dat)
      pred <- predict(rf_model, newdata = test_dat[i,])
      test_dat[i,"도로형태_대분류"] = pred
    }else
    {
      rf_model <- randomForest::randomForest(도로형태_대분류~사상자수+법규위반, dat)
      pred <- predict(rf_model, newdata = test_dat[i,])
      test_dat[i,"도로형태_대분류"] = pred

      rf_model <- randomForest::randomForest(도로형태~사상자수+법규위반+도로형태_대분류, dat)
      pred <- predict(rf_model, newdata = test_dat[i,])
      test_dat[i,"도로형태"] = pred
    }
  }else
  {
    if("도로형태" %in% col_names[is.na(test_dat[i,])])
    {
      rf_model <- randomForest::randomForest(도로형태~사상자수+법규위반+도로형태_대분류, dat)
      pred <- predict(rf_model, newdata = test_dat[i,])
      test_dat[i,"도로형태"] = pred
    }
  }
}


for(i in 1:nrow(test_dat))
{
  if("당사자종별_2당_대분류" %in% col_names[is.na(test_dat[i,])])
  {
    if("사고유형_대분류" %in% col_names[is.na(test_dat[i,])] == 0)
    {
      rf_model <- randomForest::randomForest(당사자종별_2당_대분류~중상자수+법규위반+도로형태+사고유형_대분류, dat)
      pred <- predict(rf_model, newdata = test_dat[i,])
      test_dat[i,"당사자종별_2당_대분류"] = pred
    }else 
    {
      if("사고유형_중분류" %in% col_names[is.na(test_dat[i,])] == 0)
      {
        rf_model <- randomForest::randomForest(당사자종별_2당_대분류~중상자수+법규위반+도로형태+사고유형_중분류, dat)
        pred <- predict(rf_model, newdata = test_dat[i,])
        test_dat[i,"당사자종별_2당_대분류"] = pred
      }else
      {
        if("당사자종별_1당_대분류" %in% col_names[is.na(test_dat[i,])] == 0)
        {
          rf_model <- randomForest::randomForest(당사자종별_2당_대분류~중상자수+법규위반+도로형태+당사자종별_1당_대분류, dat)
          pred <- predict(rf_model, newdata = test_dat[i,])
          test_dat[i,"당사자종별_2당_대분류"] = pred
        }else
        {
          rf_model <- randomForest::randomForest(당사자종별_2당_대분류~중상자수+법규위반+도로형태, dat)
          pred <- predict(rf_model, newdata = test_dat[i,])
          test_dat[i,"당사자종별_2당_대분류"] = pred
        }
      }
    }
  }
  if("사고유형_대분류" %in% col_names[is.na(test_dat[i,])])
  {
    rf_model <- randomForest::randomForest(사고유형_대분류~당사자종별_2당_대분류, dat)
    pred <- predict(rf_model, newdata = test_dat[i,])
    test_dat[i,"사고유형_대분류"] = pred
  }
  if("사고유형_중분류" %in% col_names[is.na(test_dat[i,])])
  {
    rf_model <- randomForest::randomForest(사고유형_중분류~사고유형_대분류+당사자종별_2당_대분류+법규위반, dat)
    pred <- predict(rf_model, newdata = test_dat[i,])
    test_dat[i,"사고유형_중분류"] = pred
  }
  if("발생지시도" %in% col_names[is.na(test_dat[i,])])
  {
    if("발생지시군구" %in% col_names[is.na(test_dat[i,])] == 0)
    {
      nb_model <- e1071::naiveBayes(발생지시도~발생지시군구, dat)
      pred <- predict(nb_model, newdata = test_dat[i,])
      test_dat[i,"발생지시도"] = pred
    }else
    {
      nb_model <- e1071::naiveBayes(발생지시도~법규위반+사고유형_중분류+당사자종별_1당_대분류+당사자종별_2당_대분류, train)
      pred <- predict(nb_model, newdata = test_dat[i,])
      test_dat[i,"발생지시도"] = pred
      
      nb_model <- e1071::naiveBayes(발생지시군구~발생지시도, dat)
      pred <- predict(nb_model, newdata = test_dat[i,])
      test_dat[i,"발생지시군구"] = pred
    }
  }
  if("발생지시군구" %in% col_names[is.na(test_dat[i,])])
  {
    nb_model <- e1071::naiveBayes(발생지시군구~발생지시도, dat)
    pred <- predict(nb_model, newdata = test_dat[i,])
    test_dat[i,"발생지시군구"] = pred
  }
  if("당사자종별_1당_대분류" %in% col_names[is.na(test_dat[i,])])
  {
    rf_model <- randomForest::randomForest(당사자종별_1당_대분류~당사자종별_2당_대분류+법규위반+사상자수+발생지시도, dat)
    pred <- predict(rf_model, newdata = test_dat[i,])
    test_dat[i,"당사자종별_1당_대분류"] = pred
  }
  if("주야" %in% col_names[is.na(test_dat[i,])])
  {
    rf_model <- randomForest::randomForest(주야~법규위반+당사자종별_1당_대분류, dat)
    pred <- predict(rf_model, newdata = test_dat[i,])
    test_dat[i,"주야"] = pred
  }
  if("요일" %in% col_names[is.na(test_dat[i,])])
  {
    nb_model <- e1071::naiveBayes(요일~., dat)
    pred <- predict(nb_model, newdata = test_dat[i,])
    test_dat[i,"요일"] = pred
  }
}


    
    
  
  
  




