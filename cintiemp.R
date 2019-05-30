# Cincinnati MSA employment data
# Year	Period	labor force	employment	unemployment	unemployment rate

# Not Seasonally Adjusted
# Area:                   Cincinnati, OH-KY-IN Metropolitan Statistical Area
# Area Type:              Metropolitan areas
# State/Region/Division:  Ohio

#### Dec, 2015 is "preliminary"

d <- read.csv("cintiemp.csv", header = TRUE)

e <- d$employment
u <- d$unemployment
lf <- d$labor.force
urate <- d$unemployment.rate

# checking unemp. rate calculation
ur2 <- 100*u/lf
cbind(urate,ur2)

## DON'T DO THIS (except for plotting purposes)!!
# convert to a time series (monthly, starting 2005.1)
et <- ts(e, start = c(2005, 1), frequency = 12)
ut <- ts(u, start = c(2005, 1), frequency = 12)
lft <- ts(lf, start = c(2005, 1), frequency = 12)
uratet <- ts(urate, start = c(2005, 1), frequency = 12)


# using 'plot.ts' for time-series plot (puts dates on horizontal axis)
par(mfrow=c(2,2))
plot(et,type='l',col=4,lwd=1,)
title("Cinti. MSA employment")
abline(h=0,col=3)
# legend("topleft",legend=c("ffr"),col=3,lty=1,lwd=2,bty="n",cex=1.1)
plot(ut,type='l',col=4,lwd=1)
title("Cinti. MSA unemployment")
abline(h=0,col=3)
plot(lft,type='l',col=4,lwd=1)
title("Cinti. MSA labor force")
abline(h=0,col=3)
plot(uratet,type='l',col=4,lwd=1)
title("Cinti. MSA unemployment rate")
abline(h=0,col=3)


source(file="intord.R")

llf <- log(lf)
lur <- log(urate)
le <- log(e)

intord(llf)


dllf <- diff(llf)
dlur <- diff(lur)
dle <- diff(le)

# Suppose, for example, we found that urate was stationary
# then we would use lur (not differencing)
# So, MUST drop one obs. at the beginning of lur so the number
# of observations matched.
# m <- length(lur)
# lu <- lur[2:m]



# Dynamic regression model

# create lags
maxp <- 13
durlags <- embed(dlur,maxp)
dlflags <- embed(dllf,maxp)
delags <- embed(dle,maxp)

dynr1 <- lm(durlags[,1]~durlags[,2:6]+durlags[,13]+delags[,1:6]+delags[,13]+dlflags[,1:6]+dlflags[,13])
summary(dynr1)
AIC(dynr1); BIC(dynr1)


dynr2 <- lm(durlags[,1]~durlags[,2:5]+durlags[,13]+delags[,1:5]+delags[,13]+dlflags[,1:5]+dlflags[,13])
summary(dynr2)
AIC(dynr1); BIC(dynr1)
AIC(dynr2); BIC(dynr2)




# Maybe seasonal dummies would be better?!
source(file="seas.R")
n <- length(durlags[,1])
s <- seas(n,12)

dynr1s <- lm(durlags[,1]~durlags[,2:13]+delags[,1:13]+dlflags[,1:13]+ s$seas[,1:11])
summary(dynr1s)
AIC(dynr1s); BIC(dynr1s)

dynr2s <- lm(durlags[,1]~durlags[,2:5]+delags[,1:4]+dlflags[,1:5]+ s$seas[,1:11])
summary(dynr2s)
AIC(dynr2s); BIC(dynr2s)












### using unemp instead of urate, nor levels instead of logs
### does not make any difference.
# change to using log of unemployment (instead of rate)
#lur <- log(u)

# now try levels
#lur <- u
#le <- e
#llf <- lf

# using aggmacro.csv data

lur <- log(unemp)
le <- log(cpi)
llf <- log(inc)


# cointegration
cointr <- lm(lur~le+llf)
summary(cointr)
res <- cointr$resid
intord(res)

cointr <- lm(le~lur+llf)
summary(cointr)
res <- cointr$resid
intord(res)

cointr <- lm(lff~lur+le)
summary(cointr)
res <- cointr$resid
intord(res)

cointr <- lm(le~llf)
summary(cointr)
res <- cointr$resid
intord(res)

cointr <- lm(lur~llf)
summary(cointr)
res <- cointr$resid
intord(res)

cointr <- lm(le~lur)
summary(cointr)
res <- cointr$resid
intord(res)


