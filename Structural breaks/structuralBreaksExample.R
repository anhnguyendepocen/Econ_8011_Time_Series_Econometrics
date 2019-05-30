
# linear trend

# true DGP:
set.seed(1358)
n = 200
t = 1:n
y = 1.0 + 0.05*t + 2*rnorm(n)

t2 = 101:n
y[101:n] = 8.0 + 0.05*t2 + 2*rnorm(n/2)

plot(y,type='l')

# Estimating a model
r1 = lm(y[1:100]~t[1:100])
summary(r1)
abline(a=r1$coeff[1],b=r1$coeff[2],col=4)


r2 = lm(y[101:n]~t[101:n])
summary(r2)
abline(a=r2$coeff[1],b=r2$coeff[2],col=2)

# dummy for structural break
d = ifelse(t<=100,0,1)

# interaction dummy
td = t*d

rd = lm(y~t+d+td)
summary(rd)
ap = cbind(y,rd$fitted)
matplot(ap,col=3:4,type='l')

# difference the variable because it is nonstationary!
dy = diff(y)
plot(t[2:n],dy,type='l',col=3)


dd = diff(d)
lines(dd*80,type='l')


rdd = lm(dy~t[2:n]+dd)
summary(rdd)
ap = cbind(dy,rdd$fitted)
matplot(ap,col=3:4,type='l')


# looking for the break point
tstat = rep(0,181)
pvalue = rep(0,181)
phi = rep(0,181)
odds = rep(0,181)

for (i in 10:190) {
# dummy for structural break
d = ifelse(t==i,1,0)

# interaction dummy
#td = t*d

rd = lm(y~t+d)
tstat[i] = summary(rd)$coef[3,3]
pvalue[i] = summary(rd)$coef[3,4]
}

par(mfrow=c(1,2))
plot(abs(tstat),col=4,type='l')
abline(v=101,col=2)
plot(-log(pvalue),col=3,type='l')
abline(v=101,col=2)

# summary(rd)
# ap = cbind(y,rd$fitted)
# matplot(ap,col=3:4,type='l')

###
## AR estimation, etc. ##
###
set.seed(17236)
yy = rep(0,200)
# AR1 dgp
phitrue = 0.50  ## works well for phi = 0.9
atrue = 0.0
dd = ifelse(t==151,5,0)
e = rnorm(200)*0.1
yy[1] = rnorm(1)

t1 = 1:200
#for (i in 2:100) {
#y[i] = atrue + phitrue*y[i-1] + e[i]
#}
for (i in 2:210) {
  yy[i] = atrue + dd[i] + phitrue*y[i-1] + e[i]
}
y = yy[11:210]
plot(t1,y,type='l',col=3)
abline(v=141,col="red",lwd=2)

d = ifelse(t<=140,0,1)

tstatar = rep(0,181)
pvaluear = rep(0,181)
phiar = rep(0,181)
oddsar = rep(0,181)


yl = embed(y,2)
yt = yl[,1]
yt1 = yl[,2]

t = 2:n
v = 197

for (i in 10:190) {
# dummy for structural break
d = ifelse(t==i,1,0)

# interaction dummy
#td = t*d

rd = lm(yt~yt1+d)
phiar[i] = rd$coeff[2]
tstatar[i] = summary(rd)$coef[3,3]
pvaluear[i] = summary(rd)$coef[3,4]
oddsar[i] = (1 + (summary(rd)$coef[3,3]^2)/v)^((v+1)/2)
}

maxind = which(tstatar==max(tstatar))

par(mfrow=c(2,2))
plot(oddsar,col=4,type='l')
abline(v=150,col=2)
abline(v=maxind,col=15)
plot(-log(pvaluear),col=3,type='l')
abline(v=150,col=2)
abline(v=maxind,col=15)
plot(phiar,col=6,type='l')
abline(v=150,col=2)
abline(v=maxind,col=15)
plot(t1,y,type='l',col=1, lwd=2)
abline(v=150,col="red",lwd=2)
abline(v=maxind,col=15)

mean(phiar[10:length(phiar)])


# Differencing y (assuming unit root)
dy = diff(y)

tstatard = rep(0,181)
pvalueard = rep(0,181)
phiard = rep(0,181)
oddsard = rep(0,181)


yl = embed(dy,2)
yt = yl[,1]
yt1 = yl[,2]

t = 2:(n-1)
v = 196

for (i in 10:190) {
# dummy for structural break
d = ifelse(t==i,1,0)

# interaction dummy
#td = t*d

rd = lm(yt~yt1+d)
phiard[i] = rd$coeff[2]
tstatard[i] = summary(rd)$coef[3,3]
pvalueard[i] = summary(rd)$coef[3,4]
oddsard[i] = (1 + (summary(rd)$coef[3,3]^2)/v)^((v+1)/2)
}

maxind = which(tstatard==max(tstatard))

par(mfrow=c(2,2))
plot(oddsard,col=4,type='l')
abline(v=150,col=2)
abline(v=maxind,col=15)
plot(-log(pvalueard),col=3,type='l')
abline(v=150,col=2)
abline(v=maxind,col=15)
plot(phiard,col=6,type='l')
abline(v=150,col=2)
abline(v=maxind,col=15)
plot(t1[-length(t1)],dy,type='l',col=1, lwd=2)
abline(v=150,col="red",lwd=2)
abline(v=maxind,col=15)





# rolling regression
for (i in 10:200) {
# dummy for structural break
#d = ifelse(t>i,1,0)

# interaction dummy
#td = t*d

rd = lm(yt[1:i]~yt1[1:i])
phi[i] = rd$coeff[2]
#tstat[i] = summary(rd)$coef[3,3]
#pvalue[i] = summary(rd)$coef[3,4]
tstat[i] = summary(rd)$coef[2,3]
}
#maxind = which(tstat==max(tstat))
plot(phi,col=6,type='l')
abline(v=150,col=2)
#abline(v=maxind,col=15)
plot(tstat,col=6,type='l')
abline(v=150,col=2)
#abline(v=maxind,col=15)



