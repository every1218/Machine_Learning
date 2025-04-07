library(ggplot2)

#1 Score_2.csv 파일을 불러와 df라는 이름의 데이터프레임으로 저장하세요.
df <- read.csv("C:/Users/a0102/Downloads/Score_2.csv")


#2각 과목(kor, eng, math, sci)의 평균, 중앙값, 표준편차를 구해보세요.
kor = c(mean(df$kor), median(df$kor), sd(df$kor))
eng = c(mean(df$eng), median(df$eng), sd(df$eng))
math = c(mean(df$math), median(df$math), sd(df$math))
sci = c(mean(df$sci), median(df$sci), sd(df$sci))
temp = rbind(kor, eng,math,sci)
print(temp)


#3영어 점수가 70점 이상인 학생만 필터링해서 출력하세요.
df[df$eng>=70,]


#4수학 점수의 분포를 히스토그램으로 시각화해보세요.
grp = data.frame(values = df$math)

ggplot(grp, aes(x = values)) +
  geom_histogram(aes(y = ..count..), binwidth = 5, fill = "steelblue", color = "white")


#5과학 점수를 막대그래프로 표현하세요.
grp = data.frame(student = 1:11,score = df$sci)

ggplot(grp, aes(x=factor(student) , y= score,  fill=student)) +
  geom_bar(stat = "identity")


#6 학생별 국어 점수 변화를 라인그래프로 그려보세요.
grp = data.frame(student = 1:11,score = df$kor)

ggplot(grp, aes(x=student, y= score)) +
  geom_line()


#7 수학 점수와 과학 점수의 관계를 산점도로 표현해보세요.
grp = data.frame(mt =df$math,si = df$sci)

ggplot(grp, aes(x=mt, y= si)) +
  geom_point()


#8 국어 점수의 이상치를 파악하기 위해 상자 수염 그림을 그려보세요.
#grp = data.frame(student = "group", score = df$kor)
grp = data.frame(student = c(rep("group",11)), score = df$kor)

ggplot(grp, aes(x=student, y=score)) +
  geom_boxplot(fill = c("lightblue"), outlier.color = "red")


#9 국어, 영어, 수학, 과학의 평균 점수를 avg라는 새 열로 추가하고, 
#이 평균을 기준으로 상자 수염 그림을 그려보세요.
avg = c(mean(df$kor), mean(df$eng), mean(df$math), mean(df$sci))

grp = data.frame(student = c(rep("group",4)), score = avg)

ggplot(grp, aes(x=student, y=score)) +
  geom_boxplot(fill = c("lightblue"), outlier.color = "red")


#10 평균 점수(avg) 기준으로 내림차순 정렬 후, 상위 3명의 정보를 출력해보세요.
df$avg = rowMeans(df[c("kor", "eng", "math", "sci")])
head(df[order(-df$avg),],3)