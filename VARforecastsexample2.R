# VARforecastsexample2.R
# (install first time!) and load package vars:
library(vars) 

# READ IN DATA 
# data with 3 variables fire, serv, trade
df <- read.table("cincemp.dat", header = TRUE)
# if csv file
# df <- read.csv("cincemp.csv", header = TRUE)
str(df)
summary(df)


fire <- df$fire[1:110]
serv <- df$serv[1:110]
trade <- df$trade[1:110]

fst <- cbind(fire,serv,trade)

var.2c <- VAR(fst, p = 6, type = "const")
summary(var.2c)
fcasts <- predict(var.2c, n.ahead = 8, ci = 0.95)
fcasts


fcastfire <- fcasts$fcst$fire
fcastfire

# just the forecast values
fcastfire <- fcasts$fcst$fire[,1]
fcastserv <- fcasts$fcst$serv[,1]
fcasttrade <- fcasts$fcst$trade[,1]

fcastfire
fcastserv
fcasttrade

# Forecasting with a VECM
# Johansen VECM procedure
library(urca)
library(vars)

yy <- cbind(fire,serv,trade)

n <- length(fire)
# yy12 <- yy[1:(n-12),]

vecm1 <- ca.jo(yy, ecdet = "const", type="eigen", K=3, spec="longrun")
summary(vecm1)
vecmols1 <- cajools(vecm1)
summary(vecmols1)



varf <- vec2var(vecm1, r = 1)
fcast <- predict(varf, n.ahead = 12, ci = 0.95) 
plot(fcast)
fcast

lhfaf <- fcast$fcst$lhfa[,1]   # forecast
lhfaact <- yy[(n-11):n,1]

hfas <- cbind(lhfaact,lhfaf)
matplot(hfas,type='l',lty=1,col=1:2)

rmsfe <- sqrt(sum((lhfaact-lhfaf)^2)/12)
rmsfe





