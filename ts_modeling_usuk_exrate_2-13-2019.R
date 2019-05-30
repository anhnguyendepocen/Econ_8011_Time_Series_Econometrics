# must convert to "time series variables" to use dynlm package
df <- read.csv("INDPRO.csv",header=T)
#dfts = ts(df,frequency=12,start=c(1971,1)) #Monthly data starting Jan. 2001
#str(dfts)
str(df)
n = length(df$INDPRO)
usprod = df$INDPRO[277:n]
plot(usprod,type='l',col=4)


usout = ts(usprod,frequency=12,start=c(1971,1),end=c(2018,7)) #Monthly data starting Jan. 2001
plot(usout,type='l',col=4)


df <- read.csv("EXUSUK.csv",header=T)
#dfts = ts(df,frequency=12,start=c(1971,1)) #Monthly data starting Jan. 2001
#str(dfts)
str(df)

usukxr = df$EXUSUK
plot(usukxr,type='l',col=4)

usukxrate = ts(usukxr,frequency=12,start=c(1971,1),end=c(2018,7)) #Monthly data starting Jan. 2001
plot(usukxrate,type='l',col=4)
#usukxr = usukxrate[1:571]

df <- read.csv("UNRATE.csv",header=T)
#dfts = ts(df,frequency=12,start=c(1971,1)) #Monthly data starting Jan. 2001
#str(dfts)
str(df)

urate = df$UNRATE
plot(urate,type='l',col=4)
n = length(df$UNRATE)
urate = df$UNRATE[277:n]
plot(urate,type='l',col=4)

unrate = ts(urate,frequency=12,start=c(1971,1),end=c(2018,7)) #Monthly data starting Jan. 2001
plot(unrate,type='l',col=4)

xx = cbind(usukxrate,unrate,usout)
summary(xx)

length(usukxrate)
length(unrate)
length(usout)
### must eliminate missing values!

# Take logs
lxr = log(usukxrate)
lout = log(usout)
lunr = log(unrate)

source(file="intord.R")
intord(usukxrate) # integrated of order 1 (difference once)
intord(usout)
intord(unrate)

# determine if cointegrated
library(dynlm)

cr = dynlm(lout~lunr+lxr)
uhat = cr$residuals
intord(uhat)

cr = dynlm(lxr~lunr+lout)
uhat = cr$residuals
intord(uhat)

# looking at pairs of variables
cr = dynlm(lxr~lunr)
uhat = cr$residuals
intord(uhat)

cr = dynlm(lout~lunr)
uhat = cr$residuals
intord(uhat)

cr = dynlm(lout~lxr)
uhat = cr$residuals
intord(uhat)

# suppose we found lunr and lxr are cointegrated:
cr = dynlm(lxr~lunr)
ecm = cr$residuals
# we will include one lag of ecm in the model.


# difference of log = growth rate
du = diff(lunr)
dxr = diff(lxr)
dpd = diff(lout)



par(mfrow=c(2,3))
plot(du,type='l',lwd=1,col=4)
plot(dxr,type='l',lwd=1,col=4)
plot(dpd,type='l',lwd=1,col=4)
plot(lunr,type='l',lwd=1,col=4)
plot(lxr,type='l',lwd=1,col=4)
plot(lout,type='l',lwd=1,col=4)


summary(du)
sd(du)
summary(dxr)
sd(dxr)
summary(dpd)
sd(dpd)

# build a dynamic regression model
library(dynlm)

# convert ecm to a time series
#ecmt = ts(ecm,frequency=12,start=c(1971,1), end=c(2018,7))
dxrt = ts(dxr,frequency=12,start=c(1971,1), end=c(2018,7))
dpdt = ts(dpd,frequency=12,start=c(1971,1), end=c(2018,7))
dut = ts(du,frequency=12,start=c(1971,1), end=c(2018,7))

# general specification
r1 = dynlm(dxrt~L(dxrt,1:2)+L(dxrt,12)+L(dut,0:1)+L(dpdt,0:3)+season(dxrt),start=c(1971,1), end=c(2018,7))
summary(r1)
AIC(r1); BIC(r1)

# Test for seasonality
r2 = dynlm(dxrt~L(dxrt,1:12)+L(dut,0:12)+L(dpdt,0:12),start=c(1971,1), end=c(2018,7))
anova(r2,r1)

r2 = dynlm(dxrt~L(dxrt,1:3)+L(dpdt,0:2),start=c(1971,1), end=c(2018,7))
r2 = dynlm(dxrt~L(dxrt,1:4),start=c(1981,1), end=c(2018,7))
summary(r2)
AIC(r2); BIC(r2)


# best models
rb1 = dynlm(dxrt~L(dxrt,1:3)+L(dpdt,0:2),start=c(1971,1), end=c(2018,7))
summary(rb1)
AIC(rb1); BIC(rb1)
rb2 = dynlm(dxrt~L(dxrt,1:3),start=c(1971,1), end=c(2018,7))
summary(rb2)
AIC(rb2); BIC(rb2)

anova(rb2,rb1)

# LOO-CV?
source(file="RMSPE.R")
rmspe2 = RMSPE(rb2)
rmspe1 = RMSPE(rb1)

# test for serial correlation (in the error)
library(lmtest)
# Breusch-Godfrey test for 1st order serial correlation
bgtest(rb2)
bgtest(rb2,order=2)
bgtest(rb2,order=3)
bgtest(rb2,order=4)
bgtest(rb2,order=8)
bgtest(rb2,order=12)

bgtest(rb1)
bgtest(rb1,order=2)
bgtest(rb1,order=3)
bgtest(rb1,order=4)
bgtest(rb1,order=8)
bgtest(rb1,order=12)

# choose best models
r2 <- dynlm(dxrt~L(dxrt,1:3)+L(dpdt,0:0)+L(ecmt,1),start=c(1971,1))  #+L(dut,0)
summary(r2)
AIC(r2); BIC(r2)

r2 <- dynlm(dxrt~L(dxrt,1:3)+L(dpdt,0:2)+L(ecmt,1),start=c(1971,1))  #+L(dut,0)
summary(r2)
AIC(r2); BIC(r2)


# test for serial correlation
# Breusch-Godfrey test for 1st order serial correlation
bgtest(r2)
bgtest(r2,order=2)
bgtest(r2,order=3)
bgtest(r2,order=4)
bgtest(r2,order=8)
bgtest(r2,order=12)


# model with serial correlation?
r2 <- dynlm(dxrt~L(dxrt,1:3)+L(dpdt,0:1),start=c(1971,1))  #+L(dut,0)
summary(r2)
AIC(r2); BIC(r2)

# test for serial correlation
# Breusch-Godfrey test for 1st order serial correlation
bgtest(r2)
bgtest(r2,order=2)
bgtest(r2,order=3)
bgtest(r2,order=4)
bgtest(r2,order=8)
bgtest(r2,order=12)

par(mfrow=c(1,1))
plot(r2$residuals,type='l',col=3)
abline(h=0)

plot(r2$residuals[1:100],type='l',col=3)
abline(h=0)

# ACF of residuals
acf(r2$residuals,lwd=3)

# should try to get a model where all bgtest p-values > 0.1 (not sig. at 10% level),

# GC test using anova

# testing if urate Granger causes exchange rate:

# unrestricted model (need to add some lags of dut back into the model)
r2 <- dynlm(dxrt~L(dxrt,1:3)+L(dut,0:3)+L(dpdt,0:1)+L(ecmt,1),start=c(1971,1), end=c(2018,7))

# restricted model
r2r <- dynlm(dxrt~L(dxrt,1:3)+L(dpdt,0:1)+L(ecmt,1),start=c(1971,1), end=c(2018,7))

# Test:
anova(r2r,r2,test="F")



r2 <- dynlm(dxrt~L(dxrt,1:3)+L(dut,0:3)+L(dpdt,0:1)+L(ecmt,1),start=c(1971,1))

# restricted model
r2r <- dynlm(dxrt~L(dxrt,1:3)+L(dpdt,0:1)+L(ecmt,1),start=c(1971,1))

# Test:
anova(r2r,r2,test="F")


# Same approach as above, just different variable dropped:
r2 <- dynlm(dxrt~L(dxrt,1:3)+L(dpdt,0:3)+L(ecmt,1),start=c(1971,1), end=c(2018,7))

# restricted model
r2r <- dynlm(dxrt~L(dxrt,1:3)+L(ecmt,1),start=c(1971,1), end=c(2018,7))
# Test:
anova(r2r,r2,test="F")



# Same approach as above, just different variable dropped:
r2 <- dynlm(dxrt~L(dxrt,1:3)+L(dpdt,0:1)+L(ecmt,1),start=c(1972,1), end=c(2018,3))

# restricted model
r2r <- dynlm(dxrt~L(dpdt,0:1)+L(ecmt,1),start=c(1972,1), end=c(2018,3))
# Test:
anova(r2r,r2,test="F")

#############################################################







r1 <- dynlm(du~L(du,1:2)+L(dcpi,0:5)+L(dhas,0:5)+L(dinc,0:1))
summary(r1)
AIC(r1); BIC(r1)

# test for seasonality - an F-test of all seasonal dummies
r1r <- dynlm(du~L(du,1:2)+L(dhas,0:5)+L(dinc,0:1))
summary(r1r)


anova(r1r,r1,test="F")

## Test for "Granger causality" using function gctest
# source(file="gctest.R")
# Test if US UK exrate Granger causes unemployment rate
# r2 is the unrestricted model
# r2r the restricted model
# gct = gctest(r1r,r1)
# gct$F
# gct$pvalue

## suppose above model (r2) is the best model
## Test for "Granger causality"



####### Cointegration

# difference between X and y
dux = lunr - lxr
plot(dux,type='l')

# a linear regression is an estimate of a linear combination that minimizes
# the residuals (ave. distance between the variables)

# IF a stationary combination exists, then the residuals will be an estimate of that

# Check if the residuals from a regression are stationary.
### REGRESS one variable on the other in NONstationary form (not differenced)
### with no lags
cointr = lm(lunr~lxr)
coint_relationship = cointr$residuals

## determine if residuals are stationary or not
source(file="intord.R")
intord(coint_relationship) 

# suppose I have a third variable
z = rnorm(n)
Z = cumsum(z)

# look at all 3 together, and subsets (pairs)
cointr = lm(Y~X+Z)
coint_relationship = cointr$residuals
intord(coint_relationship) 

cointr = lm(Z~X+Y)
coint_relationship = cointr$residuals
intord(coint_relationship)

cointr = lm(Z~X)
coint_relationship = cointr$residuals
intord(coint_relationship)

cointr = lm(Z~Y)
coint_relationship = cointr$residuals
intord(coint_relationship)

cointrxy = lm(X~Y)
coint_relationshipxy = cointrxy$residuals
intord(coint_relationshipxy)

# IF cointegrated, include ONE (and only one) lag of the cointegrated relationship
# in the model (error correction term)
ecm  = coint_relationshipxy
ecmt = ts(ecm,frequency=12,start=c(1971,1))
du = du[1:200]
dxr = dxr[1:200]
dpd = dpd[1:200]


r1r <- dynlm(du~L(du,1:2)+L(dpd,0:2)+L(dxr,0:2),start=c(1971,1))
summary(r1r)

library(lmtest)
bgtest(r1r)








######################################################################


# determine order of integration (how many times to difference)
source("intord.R")
intord(du)

du = diff(lu)
dcpi = diff(lcpi)
dinc = diff(linc)
dhas = diff(lhas)

# p = max number of lags - 1
#p <- 7   # p = 7 creates 6 lags 
#ldu <- embed(du,p)
#lcpi <- embed(dcpi,p)
#lhas <- embed(dhas,p)
#linc <- embed(dinc,p)

# define dependent variable and explanatory variable matrix
#r1 <- lm(ldu[,1]~ldu[,2:p]+lcpi[,1:p]+lhas[,1:p]+linc[,1:p])
#summary(r1)


###########
## Code using dynlm package
########
library(dynlm)
p = 6
r1 <- dynlm(du~L(du,1:2)+L(dhas,0:5)+L(dinc,0:1))
summary(r1)
AIC(r1); BIC(r1)

# add seasonal lag
r1 <- dynlm(du~L(du,1:2)+L(dhas,0:5)+L(dinc,0:1)+L(dinc,12)) #+L(du,12))
summary(r1)
AIC(r1); BIC(r1)

# number of obs.
n = length(du)

# create seasonal dummies
source("seas.R")
s = seas(n,12)  # number of obs., frequency of data (12 = monthly)
str(s)

# create time series seasonal dummy variables
s = ts(s$seas[,1:11],frequency=12,start=c(2001,1))  

r1 <- dynlm(du~L(du,1:11)+L(dcpi,0:11)+L(dhas,0:11)+s,start=c(2001,1))
summary(r1)
AIC(r1); BIC(r1)

# test for seasonality - an F-test of all seasonal dummies
r1r <- dynlm(du~L(du,1:11)+L(dcpi,0:11)+L(dhas,0:11),start=c(2001,1))
summary(r1r)
anova(r1r,r1,test="F")
AIC(r1r); BIC(r1r)


# Granger causality testing
r1 <- dynlm(du~L(du,1:2)+L(dcpi,0:5)+L(dhas,0:5)+L(dinc,0:1),start=c(2001,1))
summary(r1)
AIC(r1); BIC(r1)

# test for seasonality - an F-test of all seasonal dummies
r1r <- dynlm(du~L(du,1:2)+L(dhas,0:5)+L(dinc,0:1),start=c(2001,1))
summary(r1r)

t = 1:200
x = 0.1*t + 0.1*t^2 + rnorm(200)
intord(x)
