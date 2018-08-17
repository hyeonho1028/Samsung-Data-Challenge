setwd('D:/samsung_ai/test/')
dat <- read.csv('dataSet.csv')
test_dat <- read.csv('test_kor.csv')
result <- read.csv('result_kor.csv')

#범주형변수 ""값을 NA로 대치
for(i in 1:ncol(test_dat))
{
  test_dat[,i] <- ifelse(test_dat[,i] == "", NA, test_dat[,i])
}

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
test_dat[,3:7] <- NA
for( i in 3:7)
{
  test_dat[,i] <- as.numeric(test_dat[,i])
}
for(i in 1:nrow(test_dat))
{
  if(c("사망자수") %in% col_names[is.na(test_dat[i,])])
  {
    if(c("사상자수") %in% col_names[is.na(test_dat[i,])] == 0)
    {
      lm_model <- lm(사망자수~사상자수, dat)
      pred <- predict(lm_model, newdata = test_dat[i,])
      test_dat[i,"사망자수"] = pred
      
      if(c("경상자수") %in% col_names[is.na(test_dat[i,])])
      {
        if(sum(c("부상신고자수", "중상자수") %in% col_names[is.na(test_dat[i,])]) == 0) # 둘다 있는 경우
        {
          lm_model <- lm(경상자수~사상자수+사망자수+부상신고자수+중상자수, dat)
          pred <- predict(lm_model, newdata = test_dat[i,])
          test_dat[i,"경상자수"] = pred
        }else if(sum(c("부상신고자수", "중상자수") %in% col_names[is.na(test_dat[i,])]) == 1) # 하나만
        {
          if(c("부상신고자수") %in% col_names[is.na(test_dat[i,])] == 0)
          {
            lm_model <- lm(경상자수~사상자수+사망자수+부상신고자수, dat)
            pred <- predict(lm_model, newdata = test_dat[i,])
            test_dat[i,"경상자수"] = pred
          }else
          {
            lm_model <- lm(경상자수~사상자수+사망자수+중상자수, dat)
            pred <- predict(lm_model, newdata = test_dat[i,])
            test_dat[i,"경상자수"] = pred
          }
        }
      }else if(c("중상자수") %in% col_names[is.na(test_dat[i,])])
      {
        if(sum(c("부상신고자수", "경상자수") %in% col_names[is.na(test_dat[i,])]) == 0) # 둘다 있는 경우
        {
          lm_model <- lm(중상자수~사상자수+사망자수+부상신고자수+경상자수, dat)
          pred <- predict(lm_model, newdata = test_dat[i,])
          test_dat[i,"중상자수"] = pred
        }else if(sum(c("부상신고자수", "경상자수") %in% col_names[is.na(test_dat[i,])]) == 1) # 하나만
        {
          if(c("부상신고자수") %in% col_names[is.na(test_dat[i,])] == 0)
          {
            lm_model <- lm(중상자수~사상자수+사망자수+부상신고자수, dat)
            pred <- predict(lm_model, newdata = test_dat[i,])
            test_dat[i,"중상자수"] = pred
          }else
          {
            lm_model <- lm(중상자수~사상자수+사망자수+경상자수, dat)
            pred <- predict(lm_model, newdata = test_dat[i,])
            test_dat[i,"중상자수"] = pred
          }
        }
      }else if(c("부상신고자수") %in% col_names[is.na(test_dat[i,])]) #부상신고자 없을때
      {
        if(sum(c("중상자수", "경상자수") %in% col_names[is.na(test_dat[i,])]) == 0) # 둘다 있는 경우
        {
          lm_model <- lm(부상신고자수~사상자수+사망자수+중상자수+경상자수, dat)
          pred <- predict(lm_model, newdata = test_dat[i,])
          test_dat[i,"부상신고자수"] = pred
        }
      }
    }else
    {
      if(sum(c("발생지시군구", "사고유형_대분류") %in% col_names[is.na(test_dat[i,])]) == 0)
      {
        lm_model <- lm(사망자수~발생지시군구+사고유형_대분류, dat)
        
        pred <- predict(lm_model, newdata = test_dat[1,])#----

        
                pred <- predict(lm_model, newdata = test_dat[i,])#----
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
    if(sum(c("중상자수", "경상자수") %in% col_names[is.na(test_dat[i,])]) == 0) # 둘다 있는 경우
    {
      lm_model <- lm(부상신고자수~사망자수+중상자수+경상자수, dat)
      pred <- predict(lm_model, newdata = test_dat[i,])
      test_dat[i,"부상신고자수"] = pred
    }else if(sum(c("중상자수", "경상자수") %in% col_names[is.na(test_dat[i,])]) == 1) # 하나만 있는 경우
    {
      if(!"중상자수" %in% col_names[is.na(test_dat[i,])])
      {
        lm_model <- lm(부상신고자수~사망자수+중상자수, dat)
        pred <- predict(lm_model, newdata = test_dat[i,])
        test_dat[i,"부상신고자수"] = pred
      }else
      {
        lm_model <- lm(부상신고자수~사망자수+경상자수, dat)
        pred <- predict(lm_model, newdata = test_dat[i,])
        test_dat[i,"부상신고자수"] = pred
      }
    }else
    {
      lm_model <- lm(부상신고자수~사망자수, dat)
      pred <- predict(lm_model, newdata = test_dat[i,])
      test_dat[i,"부상신고자수"] = pred
    }
  }
  if("경상자수" %in% col_names[is.na(test_dat[i,])])
  {
    if("중상자수" %in% col_names[is.na(test_dat[i,])] == 0)
    {
      rf_model <- randomForest::randomForest(경상자수~중상자수+사망자수+부상신고자수, dat)
      pred <- predict(rf_model, newdata = test_dat[i,])
      test_dat[i,"경상자수"] = pred
    }else
    {
      rf_model <- randomForest::randomForest(경상자수~사망자수+부상신고자수, dat)
      pred <- predict(rf_model, newdata = test_dat[i,])
      test_dat[i,"경상자수"] = pred
    }
  }
  if("중상자수" %in% col_names[is.na(test_dat[i,])])
  {
    rf_model <- randomForest::randomForest(중상자수~사망자수+부상신고자수+경상자수, dat)
    pred <- predict(rf_model, newdata = test_dat[i,])
    test_dat[i,"중상자수"] = pred
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
  if(c("사망자수") %in% col_names[is.na(test_dat[i,])])
  {
    if(c("사상자수") %in% col_names[is.na(test_dat[i,])] == 0)
    {
      lm_model <- lm(사망자수~사상자수, dat)
      pred <- predict(lm_model, newdata = test_dat[i,])
      test_dat[i,"사망자수"] = pred
      
      if(c("경상자수") %in% col_names[is.na(test_dat[i,])])
      {
        if(sum(c("부상신고자수", "중상자수") %in% col_names[is.na(test_dat[i,])]) == 0) # 둘다 있는 경우
        {
          lm_model <- lm(경상자수~사상자수+사망자수+부상신고자수+중상자수, dat)
          pred <- predict(lm_model, newdata = test_dat[i,])
          test_dat[i,"경상자수"] = pred
        }else if(sum(c("부상신고자수", "중상자수") %in% col_names[is.na(test_dat[i,])]) == 1) # 하나만
        {
          if(c("부상신고자수") %in% col_names[is.na(test_dat[i,])] == 0)
          {
            lm_model <- lm(경상자수~사상자수+사망자수+부상신고자수, dat)
            pred <- predict(lm_model, newdata = test_dat[i,])
            test_dat[i,"경상자수"] = pred
          }else
          {
            lm_model <- lm(경상자수~사상자수+사망자수+중상자수, dat)
            pred <- predict(lm_model, newdata = test_dat[i,])
            test_dat[i,"경상자수"] = pred
          }
        }
      }else if(c("중상자수") %in% col_names[is.na(test_dat[i,])])
      {
        if(sum(c("부상신고자수", "경상자수") %in% col_names[is.na(test_dat[i,])]) == 0) # 둘다 있는 경우
        {
          lm_model <- lm(중상자수~사상자수+사망자수+부상신고자수+경상자수, dat)
          pred <- predict(lm_model, newdata = test_dat[i,])
          test_dat[i,"중상자수"] = pred
        }else if(sum(c("부상신고자수", "경상자수") %in% col_names[is.na(test_dat[i,])]) == 1) # 하나만
        {
          if(c("부상신고자수") %in% col_names[is.na(test_dat[i,])] == 0)
          {
            lm_model <- lm(중상자수~사상자수+사망자수+부상신고자수, dat)
            pred <- predict(lm_model, newdata = test_dat[i,])
            test_dat[i,"중상자수"] = pred
          }else
          {
            lm_model <- lm(중상자수~사상자수+사망자수+경상자수, dat)
            pred <- predict(lm_model, newdata = test_dat[i,])
            test_dat[i,"중상자수"] = pred
          }
        }
      }else if(c("부상신고자수") %in% col_names[is.na(test_dat[i,])]) #부상신고자 없을때
      {
        if(sum(c("중상자수", "경상자수") %in% col_names[is.na(test_dat[i,])]) == 0) # 둘다 있는 경우
        {
          lm_model <- lm(부상신고자수~사상자수+사망자수+중상자수+경상자수, dat)
          pred <- predict(lm_model, newdata = test_dat[i,])
          test_dat[i,"부상신고자수"] = pred
        }
      }
    }else
    {
      if(sum(c("발생지시군구", "사고유형_대분류") %in% col_names[is.na(test_dat[1,])]) == 0)
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
    if(sum(c("중상자수", "경상자수") %in% col_names[is.na(test_dat[i,])]) == 0) # 둘다 있는 경우
    {
      lm_model <- lm(부상신고자수~사망자수+중상자수+경상자수, dat)
      pred <- predict(lm_model, newdata = test_dat[i,])
      test_dat[i,"부상신고자수"] = pred
    }else if(sum(c("중상자수", "경상자수") %in% col_names[is.na(test_dat[i,])]) == 1) # 하나만 있는 경우
    {
      if(!"중상자수" %in% col_names[is.na(test_dat[i,])])
      {
        lm_model <- lm(부상신고자수~사망자수+중상자수, dat)
        pred <- predict(lm_model, newdata = test_dat[i,])
        test_dat[i,"부상신고자수"] = pred
      }else
      {
        lm_model <- lm(부상신고자수~사망자수+경상자수, dat)
        pred <- predict(lm_model, newdata = test_dat[i,])
        test_dat[i,"부상신고자수"] = pred
      }
    }else
    {
      lm_model <- lm(부상신고자수~사망자수, dat)
      pred <- predict(lm_model, newdata = test_dat[i,])
      test_dat[i,"부상신고자수"] = pred
    }
  }
  if("경상자수" %in% col_names[is.na(test_dat[i,])])
  {
    if("중상자수" %in% col_names[is.na(test_dat[i,])] == 0)
    {
      rf_model <- randomForest::randomForest(경상자수~중상자수+사망자수+부상신고자수, dat)
      pred <- predict(rf_model, newdata = test_dat[i,])
      test_dat[i,"경상자수"] = pred
    }else
    {
      rf_model <- randomForest::randomForest(경상자수~사망자수+부상신고자수, dat)
      pred <- predict(rf_model, newdata = test_dat[i,])
      test_dat[i,"경상자수"] = pred
    }
  }
  if("중상자수" %in% col_names[is.na(test_dat[i,])])
  {
    rf_model <- randomForest::randomForest(중상자수~사망자수+부상신고자수+경상자수, dat)
    pred <- predict(rf_model, newdata = test_dat[i,])
    test_dat[i,"중상자수"] = pred
  }
  if("사상자수" %in% col_names[is.na(test_dat[i,])])
  {
    lm_model <- lm(사상자수~부상신고자수+중상자수+사망자수+경상자수, dat)
    pred <- predict(lm_model, newdata = test_dat[i,])
    test_dat[i,"사상자수"] = pred
  }
  if("법규위반" %in% col_names[is.na(test_dat[i,])])
  {
    rf_model <- randomForest::randomForest(법규위반~사상자수, dat)
    pred <- predict(rf_model, newdata = test_dat[i,])
    test_dat[i,"법규위반"] = factor(pred)
  }
  if("도로형태_대분류" %in% col_names[is.na(test_dat[i,])])
  {
    if(!"도로형태" %in% col_names[is.na(test_dat[i,])])
    {
      rf_model <- randomForest::randomForest(도로형태_대분류~도로형태, dat)
      pred <- predict(rf_model, newdata = test_dat[i,])
      test_dat[i,"도로형태_대분류"] = factor(pred)
    }else
    {
      rf_model <- randomForest::randomForest(도로형태_대분류~사상자수+법규위반, dat)
      pred <- predict(rf_model, newdata = test_dat[i,])
      test_dat[i,"도로형태_대분류"] = factor(pred)
      
      rf_model <- randomForest::randomForest(도로형태~사상자수+법규위반+도로형태_대분류, dat)
      pred <- predict(rf_model, newdata = test_dat[i,])
      test_dat[i,"도로형태"] = factor(pred)
    }
  }
}

test_dat


