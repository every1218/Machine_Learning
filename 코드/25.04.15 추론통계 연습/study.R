#1
(143-150)/sqrt(20*20/36)
round(pnorm(-2.1),4)
round(pnorm(143, mean=150, 20/sqrt(36)),4)
#0.0179


#2
z = round((55-60)/sqrt(100/40),2)
round(pnorm(z),4)
# 0.0008


#3
z = round((157-160)/sqrt(7*7/10),2)
round(pnorm(z),4)
#0.0869

#4
z = round((20-15)/sqrt(100/25),2)
round(pnorm(z),4)
#0.9938


#5
z = round((15-25)/sqrt(144/35),2)
round(pnorm(z),4)
#0


#6
z = round((17-18)/sqrt(9/36),2)
temp = round(pnorm(z),4)
1-temp
#0.9772


#7
#표본평균 80보다 낮을확률 + 표본평균 90보다 높을확률
p = round(pnorm(80,85,sqrt(100/30)),4)
p*2
#0.0062



#8 5점이상, 8점이하 차이가 날 확률
z = round((77-85)/sqrt(100/30),2)
#0.0062

#9
mn = round(pnorm(173,175,sqrt(225/49)),4)
mn #173이상일확률
mx = round(pnorm(178,175,sqrt(225/49)),4)
mx #178이하일 확률
mx-(1-mn)
#0.0945




