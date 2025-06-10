library(chisq.posthoc.test) #독립성검정-사후검정
library(multcomp) #anova-사후검정
library(car) #등분산검정
library(rstatix) #games_howell_test
library(psych) #상관관계


#1
df = read.csv("C:/Users/a0102/OneDrive/바탕 화면/머신러닝/acascore.csv", fileEncoding="utf-8",stringsAsFactors = TRUE)
dim(df)
head(df)
unique(df$공부양)


ANOVA1_result <- aov(검사점수 ~ 공부양, data = df)
summary(ANOVA1_result)
#귀무가설 : 공부양 집단 간 검사점수평균의 차이가 없다
#대립가설 : 적어도 하나의 공부양 집단은 검사점수평균의 차이가 있다
#p밸류0.823-> 0.05보다 크다 -> 귀무가설 채택
#-> 공부양 집단 간 검사점수평균의 차이가 없다

# ans <- glht(ANOVA1_result, linfct = mcp(Class = "Tukey"))
# summary(ans)



#2번 시험불안 -> 검사점수 스피어만
df = read.csv("C:/Users/a0102/OneDrive/바탕 화면/머신러닝/acascore.csv", fileEncoding="utf-8",stringsAsFactors = TRUE)
dim(df)
head(df)
unique(df$공부양)

spearman_result=corr.test(df$검사점수, df$시험불안, method="spearman")
round(spearman_result$p,2)
round(spearman_result$r,2)
#귀무가설 : 시험불안과 검사점수는 단조관계가 없다
#대립가설 : 시험불안과 검사점수는 단조관계가 있다
#p밸류 = 0 : 0.05보다 작다 -> 귀무가설 기각
#-> 시험불안과 검사점수는 강한 음의 단조관계가 있다




#3번 공부양(범주형) 시험불안(범주형) 
df = read.csv("C:/Users/a0102/OneDrive/바탕 화면/머신러닝/acascore.csv", fileEncoding="utf-8",stringsAsFactors = TRUE)
dim(df)
head(df)
unique(df$공부양)
unique(df$시험불안)

tab <- table(df$공부양, df$시험불안)
tab
independent_result <- chisq.test(tab)
round(independent_result$p.value,2)
#귀무가설 : 공부양과 시험불안은 관련이 없다
#대립가설 : 공부양과 시험불안은 관련이 있다
#1 -> p밸류가 0.05보다 큼 -> 귀무가설 채택
#-> 공부양과 시험불안은 관련이 없다

ans <- chisq.posthoc.test(tab, method = "bonferroni")
ans







#1 독립성검정 
df = read.csv("C:/Users/a0102/OneDrive/바탕 화면/머신러닝/student_habits_performance.csv", fileEncoding="cp949",stringsAsFactors = TRUE)
dim(df)
head(df)
unique(df$gender)
unique(df$part_time_job)

tab <- table(df$gender, df$part_time_job)
tab
independent_result <- chisq.test(tab)
round(independent_result$p.value,2)
#귀무가설 : gender와 part_time_job은 관련이 없다
#대립가설 : gender와 part_time_job은 관련이 있다
#p밸류 0.05보다 큼 -> 귀무가설 채택

ans <- chisq.posthoc.test(tab, method = "bonferroni")
ans



#2 연속형 - 범주형인데 등분산검정 f-test? -> 문제오류
dim(df)
head(df)
unique(df$exam_score)
unique(df$part_time_job)


#3 일원분산분석
dim(df)
head(df)
unique(df$diet_quality)
unique(df$exam_score)

ANOVA1_result <- aov(exam_score ~ diet_quality, data = df)
summary(ANOVA1_result)
#귀무가설 : 다이어트 퀄리티의 각 집단은 시험점수 평균의 차이가 없다
#대립가설 : 다이어트 퀄리티의 적어도 하나의 집단은 시험점수 평균의 차이가 있다
#p밸류 0.282 -> 0.05보다 큼->귀무가설 채택
#다이어트 퀄리티의 각 집단은 시험점수 평균의 차이가 없다
ans <- glht(ANOVA1_result, linfct = mcp(Class = "Tukey"))
summary(ans)



#4번
dim(df)
head(df)
unique(df$internet_quality)
unique(df$exam_score)
ANOVA1_result <- aov(exam_score ~ internet_quality, data = df)
summary(ANOVA1_result)

#0.0보다 크다 귀무가설 기각 각집단은 평균의 차이가 없다

#등분산이 아니다
#분산의 차이가 있다
#-> 귀무가설 기각 -> 0.05보다 작음 -> gameshowl
#근데 일원분산분석인데?
# ans <- glht(ANOVA1_result, linfct = mcp(internet_quality= "Tukey"))
# summary(ans)


#5번
dim(df)
head(df)
unique(df$study_hours_per_day)
unique(df$exam_score)

temp <- df[,c("수면시간", "휴대폰사용시간", "카페인섭취량", "운동시간")]
pearson_result=corr.test(df$exam_score, df$study_hours_per_day , method="pearson")
round(pearson_result$p,2)
round(pearson_result$r,2)
#귀무가설 : 하루 공부시간은 시험점수와 상관관계가 없다
#대립가설 : 하루 공부시간은 시험점수와 상관관계가 있다
#p밸류 0 -> 0.05보다 작다 -> 귀무가설기각
#상관계수 :0.83
#-> 하루 공부시간은 시험점수와 강한 양의 상관관계가 있다
