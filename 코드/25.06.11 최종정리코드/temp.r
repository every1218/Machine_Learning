library(chisq.posthoc.test)
library(multcomp) 
library(car) 
library(rstatix)
library(psych) 


df = read.csv('C:/Users/a0102/OneDrive/바탕 화면/머신러닝/color.csv', fileEncoding="utf-8",stringsAsFactors = TRUE)
dim(df)
head(df)
unique(df$색깔)
is.na(df)
na.omit(df)

tab = table(df$색깔)
num = length(unique(df$색깔))
total <-sum(tab)
expected <- rep(total/num, num)
fit_result<-chisq.test(tab, p=expected/total)
round(fit_result$p.value,2)

std_residuals <- (tab-expected)/sqrt(expected)
chi_square_values <- std_residuals^2
p_values<-1-pchisq(chi_square_values,df=num-1)
ans <- data.frame(
  names = names(tab),
  expect = expected,
  num =as.numeric(tab),
  std_residuals= as.numeric(std_residuals),
  chisq=as.numeric(chi_square_values), 
  p_values=round(as.numeric(p_values),2)
)
ans




df = read.csv('C:/Users/a0102/OneDrive/바탕 화면/머신러닝/gender_food.csv',fileEncoding="utf-8",stringsAsFactors = TRUE)
dim(df)
head(df)
unique(df$Gender)

tab <- table(df$Gender, df$Food)
tab
independent_result <- chisq.test(tab)
round(independent_result$p.value,2)
ans <- chisq.posthoc.test(tab, method = "bonferroni")
ans






df= read.csv('C:/Users/a0102/OneDrive/바탕 화면/머신러닝/Machine_set.csv', fileEncoding="utf-8",stringsAsFactors = TRUE)
dim(df)
head(df)
unique(df$machine)

set_a <- subset(df, machine == "A")$value
set_b <- subset(df, machine == "B")$value
F_test_result <- var.test(set_a, set_b) 
round(F_test_result$p.value,2)
ans <- var.test(set_a, set_b, alternative = "less") 
ans






df = read.csv("C:/Users/a0102/OneDrive/바탕 화면/머신러닝/class_scores.csv", fileEncoding="utf-8",stringsAsFactors = TRUE)
dim(df)
head(df)
unique(df$Class)

ANOVA1_result <- aov(Score ~ Class, data = df)
summary(ANOVA1_result)
ans <- glht(ANOVA1_result, linfct = mcp(Class = "Tukey"))
summary(ans)






df <- read.csv("C:/Users/a0102/OneDrive/바탕 화면/머신러닝/growth.csv", fileEncoding="utf-8",stringsAsFactors = TRUE)
dim(df)
head(df)
unique(df$fertilizer)
unique(df$water)

ANOVA2_result <- aov(growth ~ fertilizer + water, data = df)
summary(ANOVA2_result)

leveneTest(growth ~ fertilizer, data = df)
tukey_ans <- glht(ANOVA2_result, linfct = mcp(fertilizer = "Tukey"))
summary(tukey_ans)
games_howell_test(df, growth ~ fertilizer)

leveneTest(growth ~ water, data = df)
tukey_ans <- glht(ANOVA2_result, linfct = mcp(water = "Tukey"))
summary(tukey_ans)
games_howell_test(df, growth ~ water)

leveneTest(growth ~ water, data = df)
tukey_ans <- glht(ANOVA2_result, linfct = mcp(water = "Tukey"))
summary(tukey_ans)
games_howell_test(df, growth ~ water)






df= read.csv('C:/Users/a0102/OneDrive/바탕 화면/머신러닝/pearson.csv',fileEncoding="utf-8", stringsAsFactors = TRUE)
dim(df)
head(df)

temp <- df[,c("수면시간", "휴대폰사용시간", "카페인섭취량", "운동시간")]
pearson_result=corr.test(df$집중력점수, temp , method="pearson")
round(pearson_result$p,2)
round(pearson_result$r,2)






df = read.csv('C:/Users/a0102/OneDrive/바탕 화면/머신러닝/spearman.csv', fileEncoding="utf-8", stringsAsFactors = TRUE)
dim(df)
head(df)

temp <- df[,c("스트레스수준", "자기효능감수준", "학교만족도", "가족지원수준", "소속감수준")]
spearman_result=corr.test(df$집중력등급, temp, method="spearman")
round(spearman_result$p,2)
round(spearman_result$r,2)














df = read.csv('C:/Users/a0102/OneDrive/바탕 화면/머신러닝/Food.csv',fileEncoding="utf-8",stringsAsFactors = TRUE)
dim(df)
head(df)
unique(df$FavoriteColor)
unique(df$Gender)
unique(df$Food)

tab <- table(df$Gender, df$Food)
tab
independent_result <- chisq.test(tab)
round(independent_result$p.value,2)
#귀무가설 : 성별과 선호색은 관련이 없다
#대립가설 : 성별과 선호색은 관련이 있다
#p밸류 0.52 -> 0.05보다크다 -> 귀무가설 채택한다
#성별과 선호색은 관련이 없다

ans <- chisq.posthoc.test(tab, method = "bonferroni")
ans

# 버거 ) 남자>여자
# 파스타) 여자>남자
# 피자) 남자>여자
# 샐러드) 여자>남자
# 스테이크) 남자>여자
# 스시) 여자>남자






df = read.csv('C:/Users/a0102/OneDrive/바탕 화면/머신러닝/Measurement.csv',fileEncoding="utf-8",stringsAsFactors = TRUE)
dim(df)
head(df)
unique(df$Category1)
unique(df$Category2)
unique(df$Category3)
unique(df$Category4)

tab = table(df$Category2)
num = length(unique(df$Category2))
total <-sum(tab)
expected <- rep(total/num, num)
fit_result<-chisq.test(tab, p=expected/total)
round(fit_result$p.value,2)

#카테고리1 -> 분포 차이 있음 0 
#카테고리2 -> 분포 차이 없음 0.81 ->적합
#카테고리3 -> 분포 차이 있음 0
#카테고리4 -> 분포 차이 있음 0.68 ->적합

std_residuals <- (tab-expected)/sqrt(expected)
chi_square_values <- std_residuals^2
p_values<-1-pchisq(chi_square_values,df=num-1)
ans <- data.frame(
  names = names(tab),
  expect = expected,
  num =as.numeric(tab),
  std_residuals= as.numeric(std_residuals),
  chisq=as.numeric(chi_square_values), 
  p_values=round(as.numeric(p_values),2)
)
ans



df <- read.csv("C:/Users/a0102/OneDrive/바탕 화면/머신러닝/Measurement.csv", fileEncoding="utf-8",stringsAsFactors = TRUE)
dim(df)
head(df)
unique(df$Category2)
unique(df$Category4)
df$Measurement

ANOVA2_result <- aov(Measurement ~ Category2 +Category4, data = df)
summary(ANOVA2_result)
#귀무가설 : [카테고리2,카테고리4] 집단 간의 measurement 평균의 차이가 없다
#대립가설 : [카테고리2,카테고리4] 집단 중 적어도 하는 measurement 평균의 차이가 있다
#둘다 p밸류 0.05보다 크다 -> 귀무가설 채택
# [카테고리2,카테고리4] 집단 간의 measurement 평균의 차이가 없다 -> 사후검정 불필요

leveneTest(Measurement ~ Category2, data = df)
tukey_ans <- glht(ANOVA2_result, linfct = mcp(Category2 = "Tukey"))
summary(tukey_ans)
games_howell_test(df, Measurement ~ Category2)

leveneTest(Measurement ~ Category4, data = df)
tukey_ans <- glht(ANOVA2_result, linfct = mcp(Category4 = "Tukey"))
summary(tukey_ans)
games_howell_test(df, Measurement ~ Category4)
















df <- read.csv("C:/Users/a0102/OneDrive/바탕 화면/머신러닝/Factory.csv", fileEncoding="utf-8",stringsAsFactors = TRUE)
dim(df)
head(df)

df$Reference
# set_a <- subset(df, machine == "A")$value
# set_b <- subset(df, machine == "B")$value
F_test_result <- var.test(df$Reference, df$Comparison1) 
round(F_test_result$p.value,2)
#귀무가설 : 레퍼런스와 comparison1의 분산의 차이가 없다
#대립가설 : 레퍼런스와 comparison1의 분산의 차이가 있다
#p밸류=0 0.05보다 작음 -> 귀무가설 기각
#레퍼런스와 comparison1의 분산의 차이가 있다

ans <- var.test(df$Reference, df$Comparison1, alternative = "less") 
ans
#귀무가설 : 레퍼런스>=컴패리전
#대립가설 : 레퍼런스<컴패리전
#



F_test_result <- var.test(df$Reference, df$Comparison2) 
round(F_test_result$p.value,2)
ans <- var.test(df$Reference, df$Comparison2, alternative = "less") 
ans

#귀무가설 : 레퍼런스>=컴패리전
#대립가설 : 레퍼런스<컴패리전2








df = read.csv('C:/Users/a0102/OneDrive/바탕 화면/머신러닝/Analysis.csv',fileEncoding="utf-8",stringsAsFactors = TRUE)
dim(df)
head(df)

temp <- df[,c("Independent1", "Independent2", "Independent3", "Independent4", "Independent5")]
pearson_result=corr.test(df$Dependent, temp , method="pearson")
#귀무가설: 디펜던트 와 independent1,2,3,4,5는 선형관계가 없다
#대립가설: 디펜던트 와 independent1,2,3,4,5는 선형관계가 있다
#1,3 -> 0.05보다 작다 -> 귀무가설 기각 -> 선형관계가 있다
  #1) (상관계수 0.4) 양의 선형관계가 있다
  #3) (상관계수 0.3) 양의 선형관계가 있다

#2,4,5 -> p밸류 0.05보다 크다 -> 귀무가설 채택 -> 선형관계가 없다

round(pearson_result$p,2)
round(pearson_result$r,2)













df <- read.csv("C:/Users/a0102/OneDrive/바탕 화면/머신러닝/cafe_experience.csv", fileEncoding="utf-8",stringsAsFactors = TRUE)
dim(df)
head(df)
#df$Experience_Score
unique(df$Coffee_country)
unique(df$Seat_Type)
unique(df$Coffee_Type)
unique(df$Visit_Time)
unique(df$Cafe_Lighting)


ANOVA2_result <- aov(Experience_Score ~ Coffee_country + Seat_Type +Coffee_Type+Visit_Time+Cafe_Lighting, data = df)
summary(ANOVA2_result)
#귀무가설 : []집단 간의 expericenc_score 평균의 차이가 없다
#대립가설 : []집단 중 적어도 하나는 expericenc_score 평균의 차이가 있다
#coffecountry만 귀무가설 채택


leveneTest(Experience_Score ~ Coffee_country, data = df)
#p밸류 0.11 -> 0.05보다 크다 => tukey 검정

tukey_ans <- glht(ANOVA2_result, linfct = mcp(Coffee_country = "Tukey"))
summary(tukey_ans)
games_howell_test(df, Experience_Score ~ fertilizer)
#p밸류 0 -> 0.05보다 작다 -> 귀무가설기각
#spain-indea) 잔차 -2.92
#spain<indead

tukey_ans <- glht(ANOVA2_result, linfct = mcp(Coffee_country = "Tukey"))
summary(tukey_ans)
games_howell_test(df, Experience_Score ~ fertilizer)


