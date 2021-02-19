combi = function(n,r){
  factorial(n)/(factorial(r)*factorial(n-r))
}
ans1 =combi(20,3)/combi(45,3)
ans2 =26/30
sprintf("%.3f",ans2)

ans3 =(283+90-5)/930
sprintf("%.3f",ans3)

ans4 =(324+323)/930
sprintf("%.3f",ans4)


pbinom(2,10,0.15)
1-(pbinom(3,10,0.15))
dbinom(10,10,0.85)
10*0.2
10*0.20*0.80
dpois(0,50)
ppois(4,2)
dpois(1,2)
pnorm(18,16,sqrt(9))
pnorm(20,16,sqrt(9))
pnorm(20,16,sqrt(9)) - pnorm(15,16,sqrt(9))
qnorm(0.95,60,15)
pt(1.33,18)
1-pt(-2.074,26)
pt(3.169,10)-(pt(-1.372,10))
qt(1-0.025,4)
qt(1-0.05,12)
qt(1-0.94,22)
qt(0.03+0.94,22)
pchisq(43.773,30)
1-pchisq(9.656,30)
qchisq(1-0.975,18)
qchisq(0.075,23)
qchisq(0.60,10)
pr=pchisq(1.5,10)
qchisq(0.6+pr,10)
qchisq(0.2+0.7,9)
1-pf(3.68,12,20)
pf(2.28,4,22)
1-pf(0.995,8,4)
pf(0.005,8,4)
qf(1-0.995,8,4)
qf(0.005,8,4)
b=qf(0.05,9,5)
qf(0.15,9,15)
pnorm(11.5,12,3)
qnorm(0.80,60,22.5)
pt(2.53,18)
qt(0.38+0.5,4)
qt(0.6+0.2,22)
qchisq(0.9,10)
pf(5.09,12,20)
1-(0.7+0.2)
qf(0.1,9,15)



weight=c(28, 25, 32, 26, 28, 40, 34, 48, 32, 26, 28, 30)
ad.test(weight)
signmedian.test(weight,conf.level = 0.95)



WashingPowderWeight=c(562.9, 550.6, 557.0, 563.0, 552.1, 551.2, 551.2, 559.5, 559.4, 561.2)
install.packages("nortest")
library(nortest)
ad.test(WashingPowderWeight)
install.packages("signmedian.test")
library(signmedian.test)
signmedian.test(WashingPowderWeight,conf.level = 0.90)


library(UsingR)
ad.test(heartrate$maxrate)
t.test(heartrate$maxrate)



library(UsingR)
Loblolly$age
signmedian.test(Loblolly$age,mu=9,conf.level = 0.95,alternative = "greater")

women$height
women$weight
ad.test(women$weight)
t.test(women$weight,mu=140,conf.level = 0.95,alternative = "less")
Bread_weight=c(79.4, 80.1, 82.0, 79.8, 79.7, 82.3, 80.5, 78.8, 79.7, 79.7)
ad.test(Bread_weight)
signmedian.test(Bread_weight,mu=80,conf.level = 0.95,alternative = "two.sided")

before =c(185,200,190,240,250,190,210,200,190,250,245)
after =c(185,190,200,250,250,199,220,210,190,260,250)
t.test(before,after, paired = TRUE,conf.level = 0.95,alternative = "greater")

t.test(before,after ,paired = TRUE,conf.level = 0.95)


N1=c(143.5,152.2,153.2,157.9,152.3,160.9,160.8,150.9,152.3,159.9)
N2=c(143.0,151.5,152.1,158.0,151.8,160.5,160.0,151.0,152.5,160)
t.test(N1,N2,paired = TRUE,conf.level = 0.95,alternative = "two.sided")

t.test(N1,N2,paired = TRUE,conf.level = 0.95)

lower=c(178,172,185,184,201,201,160,168,180,179)
eye=c(181,172,190,187,210,202,166,173,183,184)
t.test(lower,eye, paired = TRUE,conf.level = 0.95,alternative ="less")

t.test(lower,eye, paired = TRUE,conf.level = 0.95)
