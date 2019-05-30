
### AR(1) process
set.seed(1245)
n = 2000
phi = 0.98
alpha = 2.0
y = rep(0,n)
y0 = 98.0
y[1] = alpha + y0 + rnorm(1)
for (t in 2:n) {
  y[t] = alpha + phi*y[t-1] + rnorm(1)
}
par(mfrow=c(1,1))
plot(y[2:200],type='l',col=3)
plot(y,type='l',col=3)
mean(y[4:n])
source(file="intord.R")
intord(y)

t = 1:200
y = 0.5*log(t) + 0.1*rnorm(200)

ar2_dgp = function(n,alpha,phi1,phi2,y0) {
  set.seed(1245)
  y = rep(0,n)
  y[1] = alpha + phi1*y0 + rnorm(1)
  y[2] = alpha + phi1*y[1] + phi2*y0 + rnorm(1)
  for (t in 3:n) {
    y[t] = alpha + phi1*y[t-1] + phi2*y[t-2] + rnorm(1)
  }
  list(y=y)
}

yar2 = ar2_dgp(2000,0.0,0.6,0.3,0.9)$y
plot(yar2[2:200],type='l',col=3)
plot(yar2,type='l',col=3)
mean(yar2[4:n])

library(lmtest)
ys = embed(yar2,2)
yt = ys[,1]
yt1 = ys[,2]

r = lm(yt~yt1)
bgtest(r)

n = 200
bgstat = bgpval = boxt_stat = boxt_pval = rep(0,10000)
for (i in 1:10000) {
  x = rnorm(n)
  y = 0.8 -2.0*x + rnorm(n)
  r = lm(y~x)
  bgstat[i] = bgtest(r)$statistic
  bgpval[i] = bgtest(r)$p.value 
  boxt_stat[i] = Box.test(r$resid,lag=1,type=c("Ljung-Box"))$statistic
  boxt_pval[i] = Box.test(r$resid,lag=1,type=c("Ljung-Box"))$p.value
  }
quantile(bgstat,probs=c(0.95))
quantile(bgpval,probs=c(0.05))
quantile(boxt_stat,probs=c(0.95))
quantile(boxt_pval,probs=c(0.05))

b = Box.test(r$resid,lag=1,type=c("Ljung-Box"))
str(b) 

