#1
v <- c(100, 200, 300, 400, 200, 200, 100, 400, 250, 230, 70, 80, 90)
mean(v)
sd(v)
median(v)
q1 = quantile(v,0.25)
q2 = quantile(v,0.75)
q2-q1


#2
mid = c(75, 88, 91, 68, 82)
last = c(60, 87, 55, 47, 92)
mean(mid)
mean(last)

#3
mid = c(75, 88, 82)
last = c(60, 87, 55)
mean(mid)
mean(last)


#4
df = data.frame(mid, last)
df$score = df$mid*0.5 + df$last*0.3
df = df[order(-df$score),]
df
