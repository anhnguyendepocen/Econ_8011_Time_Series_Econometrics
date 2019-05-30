# US aggregate data, including retail sales 
#
#df <- read.table("USagg-salesclean.dat", header = TRUE)
df <- read.csv("usagg-clean.csv", header = TRUE)
str(df)
summary(df)

# par(mfrow=c(3,3))
# plot(df$rsales, type='l')
# plot(df$totpersinc, type='l')
# plot(df$emplnonfarm, type='l')
# plot(df$CPI, type='l')
# plot(df$unempl, type='l')
# plot(df$mortgrate, type='l')
# plot(df$conscredit, type='l')
# plot(df$govspend, type='l')
# plot(df$houshfinasset, type='l')

# define variables
rsales <- df$rsales
totpersinc <- df$totpersinc
emplnonfarm <- df$emplnonfarm
cpi <- df$CPI
unemp <- df$unempl
mortgrate <- df$mortgrate
conscredit <- df$conscredit
govspend <- df$govspend
houshfinasset <- df$houshfinasset


#### CONVERT TO TIME SERIES
# convert to a time series (monthly, starting 2001.01)
rsalest <- ts(df$rsales, start = c(2001, 1), frequency = 12)
totpersinct <- ts(df$totpersinc, start = c(2001, 1), frequency = 12)
emplnonfarmt <- ts(df$emplnonfarm, start = c(2001, 1), frequency = 12)
cpit <- ts(df$CPI, start = c(2001, 1), frequency = 12)
unempt <- ts(df$unempl, start = c(2001, 1), frequency = 12)
mortgratet <- ts(df$mortgrate, start = c(2001, 1), frequency = 12)
conscreditt <- ts(df$conscredit, start = c(2001, 1), frequency = 12)
govspendt <- ts(df$govspend, start = c(2001, 1), frequency = 12)
houshfinassett <- ts(df$houshfinasset, start = c(2001, 1), frequency = 12)


par(mfrow=c(3,3))
plot(govspend,type='l',col=1,lwd=2)
title("Monthly US Government Spending 2000:1-2010:12")
# legend("topleft",legend=c("Gov. Spending"),col=3,lty=1,lwd=2,bty="n",cex=1.1)
plot(totpersinc,type='l',col=2,lwd=2)
title("Total Personal Income 2000:1-2010:12")
plot(unemp,type='l',col=3,lwd=2)
title("Unemployment (1000s) 2000:1-2010:12")
plot(mortgrate,type='l',col=4,lwd=2)
title("Mortgage Rate 2000:1-2010:12")
plot(emplnonfarm,type='l',col=1,lwd=2)
title("Employment Nonfarm Sector (1000s) 2000:1-2010:12")
# legend("topleft",legend=c("Gov. Spending"),col=3,lty=1,lwd=2,bty="n",cex=1.1)
plot(cpi,type='l',col=2,lwd=2)
title("Consumer Price Index 2000:1-2010:12")
plot(conscredit,type='l',col=3,lwd=2)
title("Total Consumer Credit 2000:1-2010:12")
plot(houshfinasset,type='l',col=4,lwd=2)
title("Household Total Financial Assets 2000:1-2010:12")
plot(rsales,type='l',col=4,lwd=2)
title("Total Retail Sales 2000:1-2010:12")



lg <- log(govspend)
linc <- log(totpersinc)
lu <- log(unemp)
lhfa <- log(houshfinasset)
lp <- log(cpi)


dg <- diff(lg)
dinc <- diff(linc)
du <- diff(lu)
dha <- diff(lhfa)
dp <- diff(lp)

n <- length(lu)


par(mfrow=c(3,2))
plot(dg,type='l',col=4,lwd=2)
title("diff. log gov. spending")
plot(dha,type='l',col=4,lwd=2)
title("diff. household fin. assets")
plot(dinc,type='l',col=4,lwd=2)
title("diff. household pers. inc.")
plot(du,type='l',col=4,lwd=2)
title("diff. unemp. rate")
plot(dp,type='l',col=4,lwd=2)
title("diff. cpi")

# run intord to determine order of integration
intord(dg)
intord(dha)
intord(dinc)
intord(du)
intord(dp)

y <- cbind(dha, dinc, du, dp)
yy <- cbind(lhfa, linc, lu, lp)

library(urca)

## cointegration - variables must be in levels! #
jc <- ca.jo(yy, type="eigen", ecdet="const") 
summary(jc)
jct <- ca.jo(yy, type="trace", ecdet="const") 
summary(jct)

# eignevalues, etc., can be taken from the output of ca.jo
eigenvals <- jc@lambda

# cointegrating relationships
cointv <- jc@V
cointj <- cointv[,1]
yym <- as.matrix(yy)
ecmj <- yym%*%cointj[1:4] + cointj[5]

c1 <- lm(lhfa ~ linc + lu + lp) # try with and w/o gov. spending
ecm <- c1$residuals

ecmj0 = ecmj - mean(ecmj)
par(mfrow=c(2,2))
ecms <- cbind(ecm,ecmj0)
matplot(ecms,type='l',col=3:4)
plot(ecm,type='l')
plot(ecmj0,type='l')

#  *** repeat of the above w/o extra commands to make it clearer! ***
# include one lag of either ecmj or ecm in the VAR as an exog. variable
# to estimate the VECM

# cointegrating relationships - Johansen
cointv <- jc@V
cointj <- cointv[,1]
yym <- as.matrix(yy)
ecmj <- yym%*%cointj[1:4] + cointj[5] 

# Engle-Granger
c1 <- lm(lhfa ~ linc + lu + lp) # try with and w/o gov. spending
ecm <- c1$residuals

par(mfrow=c(2,2))
ecms <- cbind(ecm,ecmj)
matplot(ecms,type='l',col=3:4)

plot(ecm,type='l')
plot(ecmj,type='l')

source(file="intord.R")
intord(ecm)
ec <- embed(ecm,2)
ecm1 <- ec[,2]

ecj <- embed(ecmj,2)
ecmj1 <- ecj[,2]



# VECM since appear cointegrated, except for possibly gov. spending

library(vars)

# lag selection criteria
VARselect(y, lag.max=12, type="const")

# or if you want to use the results later or want to save them:
vs <- VARselect(y, lag.max=12, type="const")
vs$selection
vs$criteria

# choosing lag length using BIC
m1 <- VAR(y, type = "const", lag.max = 12,ic = c("SC"))

# choosing lag length using AIC
m2 <- VAR(y, type = "const", lag.max = 12,ic = c("AIC"))


# First a VAR(3) = BIC + 1 !! (since BIC can underparameterize)
var3 <- VAR(y, p=3, type="const")
summary(var3)
plot(var3, names = "dha")

plot(var3, names = "du")

# VECM with Engle-Granger error
# with seasonal dummies
var3 <- VAR(y, p=3, type="const",exogen=ecm1,season=12)
# w/o seasonal dummies
var3 <- VAR(y, p=3, type="const",exogen=ecm1)
summary(var3)

# plots of fit results
plot(var3, names = "dha")
plot(var3, names = "du")

anova(ve1, ve1r, test="F")

v



# Johansen VECM procedure
vecm1 <- ca.jo(yy, ecdet = "const", type="eigen", K=4, spec="longrun",
season=12)
summary(vecm1)
vecmols1 <- cajools(vecm1)
summary(vecmols1)



# Granger causality
# varc <- vec2var(vecm1, r = 1)
causality(var3, cause = "dha") # doesn't seem to work on a VECM

ve1 <- cajools(vecm1,reg.number=1)  #hfa is 1st equation
# yy <- cbind(lhfa, linc, lu, lp) - unrestricted setup
yr <- cbind(lhfa, linc, lp)    #  drop one variable for restricted model
vecmr <- ca.jo(yr, ecdet = "const", type="eigen", K=4, spec="longrun",
season=12)
ve1r <- cajools(vecmr,reg.number=1)  #hfa is 1st equation
summary(vecmr)
# Joint F-test for Granger causality
anova(ve1, ve1r, test="F") 

# assuming 1 coint relationship
ve1rr <- cajorls(vecmr,r=1)
summary(ve1rrr)
str(ve1rr)
ve1rr$rlm$coefficients


# variables in var3: dg dha dinc du dp

# IRFs
irfs <- irf(var3, impulse = "dinc", response = c("du"), boot = TRUE)
plot(irfs)

irfs <- irf(var3, impulse = "du", response = c("dinc", "dha"), boot =
FALSE)
plot(irfs)

# with CIs
irfs <- irf(var3, impulse = "du", response = c("dinc", "dha"), n.ahead=24, boot=T)
plot(irfs)


# Variance Decompositions (contribution of each variable to predicting a variable)
vard <- fevd(var3, n.ahead=12)
vard
vard$dha

# Forecasting with a VECM
# Johansen VECM procedure
library(urca)
library(vars)

yy <- cbind(lhfa, linc, lu, lp)

n <- length(lhfa)
yy12 <- yy[1:(n-12),]

vecm1 <- ca.jo(yy12, ecdet = "const", type="eigen", K=3, spec="longrun")
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



# Estimate a VAR(3) and forecast
y <- cbind(dha, dinc, du, dp)
var3 <- VAR(y, p=3, type="const")
fcast <- predict(var3, n.ahead = 12, ci = 0.95) 
plot(fcast)
fcast

