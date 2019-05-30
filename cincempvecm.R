#
# cincemp.R
#
#  Cincinnati employment data
#
# variables: FIRE SERVICE TRADE S1 S2 S3 S4 S5 S6 S7 S8 S9 S10 S11

#

# READ IN DATA 
#df = read.table("cincemp.dat", header = TRUE)
df = read.csv("cincfst.csv", header = TRUE)
str(df)
summary(df)


fire <- df$fire
serv <- df$serv
trade <- df$trade


# convert to a time series (monthly, starting 1982.11)
firet <- ts(fire, start = c(1993, 1), frequency = 12)
servt <- ts(serv, start = c(1993, 1), frequency = 12)
tradet <- ts(trade, start = c(1993, 1), frequency = 12)

par(mfrow=c(1,3))
plot(firet,type='l',col=4,lwd=1,)
title("FIRE sector employment")
plot(servt,type='l',col=4,lwd=1)
title("Service sector employment")
plot(tradet,type='l',col=4,lwd=1)
title("Trade sector employment")



df <- diff(fire)
ds <- diff(serv)
dt <- diff(trade)


y <- cbind(df,ds,dt)
yy <- cbind(fire,serv,trade)


library(urca)
library(vars)

# Johansen VECM procedure
vecm1 <- ca.jo(yy, ecdet = "const", type="eigen", K=2, spec="longrun",
season=12)
summary(vecm1)
vecmols1 <- cajools(vecm1)
summary(vecmols1)

# cointegrating relationships - Johansen
cointv <- vecm1@V
cointj <- cointv[,1]
yym <- as.matrix(yy)
ecmj <- yym%*%cointj[1:3] + cointj[4] 

ecj <- embed(ecmj,2)
ecmj1 <- ecj[,2]


# VECM with Johansen ecm term
var3 <- VAR(y, p=2, type="const",exogen=ecmj1,season=12)
summary(var3)



# Granger causality

ve1 <- cajools(vecm1,reg.number=1)  # fire is 1st equation

# restricted model
yr <- cbind(fire,serv)  #  drop one variable for restricted model
vecmr <- ca.jo(yr, ecdet = "const", type="eigen", K=4, spec="longrun",
season=12)
ve1r <- cajools(vecmr,reg.number=1)  # fire is 1st equation


# Joint F-test for Granger causality
anova(ve1, ve1r, test="F") 

gc <- causality(var3,cause="df") # no idea what garbage this is producing!!


# estimate model with p lags

# p = MAX number of lags + 1 *** USE p = lags+1
p <- 8
lds <- embed(ds,p)
ldf <- embed(df,p)
ldt <- embed(dt,p)
nobs <- nrow(lds)

s12 <- seas(nobs,12)
s11 <- s12$seas[,1:11]


# drop obs from beginning of ecm term
necm <- length(ecmj1)
nn <- necm-nobs+1
ecm11 <- ecmj1[nn:necm]

r1 <- lm(ldf[,1]~ldf[,2:p]+lds[,2:p]+ldt[,2:p]+s11+ecm11)
r2 <- lm(ldf[,1]~ldf[,2:p]+lds[,2:p]+s11+ecm11)

# Granger causality for trade -> fire
anova(r1, r2, test="F") 


# Granger causality for service -> fire
r3 <- lm(ldf[,1]~ldf[,2:p]+ldt[,2:p]+s11+ecm11)
anova(r1, r3, test="F") 


# Granger causality for fire -> fire
r4 <- lm(ldf[,1]~lds[,2:p]+ldt[,2:p]+s11+ecm11)
anova(r1, r4, test="F") 



