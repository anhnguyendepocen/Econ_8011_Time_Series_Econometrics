#
# strucch.R
#

# Some data to use as an example:
# Aggregate macro data, monthly 2001:1 to 2010:12 = 120 obs

# READ IN DATA 
df <- read.table("aggmacro.dat", header = TRUE)
str(df)
summary(df)

cpi <- df$CPI
inc <- df$TOTPERSINC
unemp <- df$UNEMPL

# We will use the following data just as an example.
y <- log(cpi)
x1 <- log(inc)
x2 <- log(unemp)

# difference the nonstationary variables
dy <- diff(y)
dx1 <- diff(x1)

# drop one observation from x2
m <- length(x2)
z <- x2[2:m]

p <- 7
ldy <- embed(dy,p)
lz <- embed(z,p)
ldx1 <- embed(dx1,p)

# Estimation of a model on the full sample
r2 <- lm(ldy[,1]~ldy[,2:7]+ldx1[,1:7]+lz[,1:3])
summary(r2)
AIC(r2); BIC(r2)

# Recursive least squares estimation
n <- length(ldy[,1])
k <- 17 # no. of parameters in model
b <- rep(0,n*k)
dim(b) <- c(n,k)

for (i in 20:n) {
rrec <- lm(ldy[1:i,1]~ldy[1:i,2:7]+ldx1[1:i,1:7]+lz[1:i,1:3])
b[i,] <- rrec$coeff
}

b <- b[20:n,]

par(mfrow=c(5,4))
for (i in 1:k) {
plot(b[,i],type='l')
}


# recursive residuals

library(strucchange)
rres <- recresid(r2)
plot(rres, type='l')


# Rolling regression with a sample size of m
m <- 50

# Recursive least squares estimation
n <- length(ldy[,1])
k <- 17 # no. of parameters in model
b <- rep(0,n*k)
dim(b) <- c(n,k)

for (i in m:n) {
start <- i - m + 1
rrec <- lm(ldy[start:i,1]~ldy[start:i,2:7]+ldx1[start:i,1:7]+lz[start:i,1:3])
b[i,] <- rrec$coeff
}

b <- b[50:n,]

par(mfrow=c(5,4))
for (i in 1:k) {
plot(b[,i],type='l')
}




## Example 7.4 from Greene (1993), "Econometric Analysis"
## Chow test on Longley data
data("longley")
sctest(Employed ~ Year + GNP.deflator + GNP + Armed.Forces, data = longley,
  type = "Chow", point = 7)

## which is equivalent to segmenting the regression via
fac <- factor(c(rep(1, 7), rep(2, 9)))
fm0 <- lm(Employed ~ Year + GNP.deflator + GNP + Armed.Forces, data = longley)
fm1 <- lm(Employed ~ fac/(Year + GNP.deflator + GNP + Armed.Forces), data = longley)
anova(fm0, fm1)

## estimates from Table 7.5 in Greene (1993)
summary(fm0)
summary(fm1)






library(strucchange)
ocus <- efp(Employed ~ Year + GNP.deflator + GNP + Armed.Forces, data = longley, type="OLS-CUSUM")
bound.ocus <- boundary(ocus, alpha=0.05)
plot(ocus)
sctest(ocus)


fs <- Fstats(Employed ~ Year + GNP.deflator + GNP + Armed.Forces, data = longley)
plot(fs)




x <- rnorm(100) + rep(c(0, 2), each = 50)
rr <- recresid(x ~ 1)
plot(cumsum(rr), type = "l")

plot(efp(x ~ 1, type = "Rec-CUSUM"))

fs <- Fstats(x ~ 1)
plot(fs)
plot(fs, pval=TRUE)

# try different values for m (shift)
m <- 0.5
y <- c(rnorm(100),(rnorm(100)+m))
plot(y,type='l')

rr <- recresid(y ~ 1)
plot(cumsum(rr), type = "l")

plot(efp(y ~ 1, type = "Rec-CUSUM"))

fs <- Fstats(y ~ 1) 
plot(fs)

plot(fs, pval=TRUE)


# Chow test with dummy variable(s)

br0 <- rep(0,100)
br1 <- rep(1,100)
d1 <- c(br0,br1)

br0 <- rep(0,150)
br1 <- rep(1,50)
d2 <- c(br0,br1)



t <- 1:200
yt  <- 1.0 + 0.5*t + rnorm(200) + 0.5*t*d1

d1t <- d1*t

ru <- lm(yt ~ d1 + t + d2 + d1t)
summary(ru)
rr <- lm(yt ~ 1)
anova(rr,ru,test="F")


# DF test with structural break

m <- 10
y <- c(rnorm(100),(rnorm(100)+m))
plot(y,type='l')

dy <- diff(y)
ylag <- y[1:199]
d11 <- d1[2:200]
# no break included
summary(lm(dy~ylag))

# break included
summary(lm(dy~ylag+d11))




