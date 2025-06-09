#9
df= read.csv('C:/Users/a0102/OneDrive/바탕 화면/머신러닝/class_scores.csv', fileEncoding="utf-8",stringsAsFactors = TRUE)
dim(df)
head(df)
unique(df$Class)
set_a <- subset(df, Class == "A")$Score
set_b <- subset(df, Class == "B")$Score
set_c <- subset(df, Class == "C")$Score

F_test_result <- var.test(set_a, set_b) 
F_test_result
#귀무가설 : A와 B는 분산의 차이가 없다
#p밸류 0.8561 : 0.05보다크다. 귀무가설을 채택한다
# => A와B는 분산의 차이가 없다

F_test_result <- var.test(set_b, set_c) 
F_test_result
#귀무가설 : B와 C는 분산의 차이가 없다
#p밸류 0.05보다크다. 귀무가설을 채택한다
# => B와C 분산의 차이가 없다

F_test_result <- var.test(set_a, set_c) 
F_test_result
#귀무가설 : A와 C는 분산의 차이가 없다
#p밸류 0.05보다크다. 귀무가설을 채택한다
# => A와C 분산의 차이가 없다

#=>A반,B반,C반은 시험점수 분산의 차이가 없다
