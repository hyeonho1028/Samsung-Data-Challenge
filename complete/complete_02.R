setwd('D:/samsung_ai/test/')
dat <- read.csv('dataSet.csv')
test_dat <- read.csv('test_kor.csv', stringsAsFactors = F)
result <- read.csv('result_kor.csv', stringsAsFactors = F)

#범주형변수 ""값을 NA로 대치
for(i in 1:ncol(test_dat))
{
  test_dat[,i] <- ifelse(test_dat[,i] == "", NA, test_dat[,i])
  # if(i == 1 | i ==2 | i == 8 | i == 9 | i == 10 | i == 11 | i == 12 | i == 13 | i == 14 | i == 15 | i == 16)
  # {
  #   test_dat[,i] <- as.factor(test_dat[,i])
  # }
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

# for(i in c(1, 2, 8:16))
# {
#   levels(test_dat[,i]) <- levels(dat[,i])
# }

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
    test_dat[i,"법규위반"] = pred
  }
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
      nb_model <- e1071::naiveBayes(발생지시도~법규위반+사고유형_중분류+당사자종별_1당_대분류+당사자종별_2당_대분류, dat)
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
  print(paste0(i/nrow(test_dat)*100,"% 진행"))
}  

for(i in 1:length(result$열))
{
  if(result[i,"열"] == "A")
  {
    result[i,"열"] = 1
  }
  else if(result[i,"열"] == "B")
  {
    result[i,"열"] = 2
  }
  else if(result[i,"열"] == "C")
  {
    result[i,"열"] = 3
  }
  else if(result[i,"열"] == "D")
  {
    result[i,"열"] = 4
  }
  else if(result[i,"열"] == "E")
  {
    result[i,"열"] = 5
  }
  else if(result[i,"열"] == "F")
  {
    result[i,"열"] = 6
  }
  else if(result[i,"열"] == "G")
  {
    result[i,"열"] = 7
  }
  else if(result[i,"열"] == "H")
  {
    result[i,"열"] = 8
  }
  else if(result[i,"열"] == "I")
  {
    result[i,"열"] = 9
  }
  else if(result[i,"열"] == "J")
  {
    result[i,"열"] = 10
  }
  else if(result[i,"열"] == "K")
  {
    result[i,"열"] = 11
  }
  else if(result[i,"열"] == "L")
  {
    result[i,"열"] = 12
  }
  else if(result[i,"열"] == "M")
  {
    result[i,"열"] = 13
  }
  else if(result[i,"열"] == "N")
  {
    result[i,"열"] = 14
  }
  else if(result[i,"열"] == "O")
  {
    result[i,"열"] = 15
  }
  else if(result[i,"열"] == "P")
  {
    result[i,"열"] = 16
  }
}

result$열 <- as.integer(result$열)
result$행 <- result$행-1
result$값 <- as.character(result$값)

for(i in 1:nrow(result))
{
  if(class(test_dat[result[i,1], result[i,2]]) == "factor")
  {
    result[i,3] <- as.character(test_dat[result[i,1], result[i,2]])
  }else
  {
    result[i,3] <- test_dat[result[i,1], result[i,2]]  
  }
}


write.csv(result, 'D:/samsung_ai/test/result_test.csv', row.names = F)
#
#


#write.csv(test_dat, 'C:/Users/renz/Desktop/Lab2/test.csv')
#test_dat <- read.csv('C:/Users/renz/Desktop/Lab2/test.csv')
