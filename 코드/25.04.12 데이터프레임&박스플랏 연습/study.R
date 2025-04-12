#1번
df = read.csv('C:/Users/a0102/Downloads/Score_2.csv')

#2번
avg = as.integer(c(mean(df$kor), mean(df$eng), mean(df$math), mean(df$sci), NA))
df = rbind(df, avg)

#3번
math_high = df[df$math>=80,]
print(mean(math_high$kor))
print(mean(math_high$eng))

#4번
gr = data.frame(
  subject = c(rep("kor",11), rep("eng",11),rep("math",11),rep("sci",11)),
  number =c(df$kor, df$eng, df$math, df$sci)
)
ggplot(gr,aes(x=subject, y=number))+
  geom_boxplot()


#5번 
#이상치로 판단되는 값 없음.