
n = 200
t = 1:n
x = 0.001*exp(t) + rnorm(n)
par(mfrow=c(1,1))
plot(x,type='l')
plot(x[1:100],type='l')

plot(log(x),type='l')

y = 1.0 + 0.5*t + 1.0*t^2 + 200*rnorm(n)
plot(y,type='l')

source(file="intord.R")
intord(x)
intord(y)



# qu. 2
n = 200
x = rnorm(n)
y = rnorm(n, mean=1, sd=2)
z = 5.0 + 3.0*rt(n,df=5)
w = rgamma(n, shape=2, rate = 1)
par(mfrow=c(2,2))
hist(x,col=2,breaks=100)
hist(y,col=3,breaks=100)
hist(z,col=4,breaks=100)
hist(w,col=5,breaks=100)

plot(x,col=2,type='l')
plot(y,col=3,type='l')
plot(z,col=4,type='l')
plot(w,col=5,type='l')

source(file="intord.R")
intord(w)

summary(lm(y~z))

# add time trend
n = length(Y)
t = 1:n
summary(lm(Y~Z+t))


X = cumsum(x)
Y = cumsum(y)
Z = cumsum(z)
W = cumsum(w)


summary(lm(Y~X))
summary(lm(Y~Z))
summary(lm(Z~W))
summary(lm(Y~X+t))


plot(X[1:1000],col=2,type='l')
plot(Y[1:1000],col=3,type='l')
plot(Z[1:1000],col=4,type='l')
plot(W[1:1000],col=5,type='l')


# qu. 3 read in data
rgdpdat = read.csv("realGDP.csv",header = T)
cpidat = read.csv("CPI.csv",header = T)
str(rgdpdat)
str(cpidat)

rgdp = rgdpdat$realGDP
cpi = cpidat$CPIAUCSL

#realgdp = as.ts(rgdp, start=1947.1,end=2018.1)

# cheating!
rgdp1 = as.numeric(rgdp[1:200])
cpi1 = as.numeric(cpi[1:200])

length(cpi1)
length(rgdp1)
r1 = lm(rgdp1~cpi1)
summary(r1)
intord(rgdp1)
intord(cpi1)

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
plot(y[2:200],type='l',col=3)
plot(y,type='l',col=3)
mean(y[4:n])

# AR(2) process
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
