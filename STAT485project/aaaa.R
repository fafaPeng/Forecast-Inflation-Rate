
library(TSA)
library(tseries)
setwd("C:/Users/chery/Desktop/485projectFinal")

data = read.csv("2000.csv")

data1 = data[,2]

data.ts = ts(data1, frequency = 12, start = c(2000, 1))

### Time series plot ###
plot(window(data.ts, start=c(2000,1)), 
     main="Inflation rate in the US from 2000 to 2022", ylab = "Inflation Rate")
Month = c('J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N', 'D')
points(window(data.ts, start=c(2000,1)), pch=Month)

### Dickey-Fuller test
adf.test(data.ts)

# p-value is 0.01, therefore stationary.

############# 这下面用的是box and Jenkins Method来选择model #########

### ACF
acf(as.vector(data.ts), main="Sample ACF for Inflation Rate",lag.max=30)
#suggest MA(1)


### PACF
pacf(as.vector(data.ts), main="Sample PACF for Inflation Rate", lag.max=30)
#AR(2) or AR(1)

### EACF
eacf(data.ts)
#EACF suggests maybe a MA(1) or ARMA(1,2)



############## Fitting Model #################

###############
### MA(1)  ####
###############

MA_1 = arima(data.ts, order=c(0, 0, 1))
MA_1
#log likelihood = -35.07,  aic = 74.15

########## Check Residual ###########

## ACF of residual
acf(as.vector(MA_1$residuals), lag.max = 40)

## Residual Plot
plot(as.vector(MA_1$residuals), type="o", main='Residual Plot of MA(1)')
#Residual hang together too much for white noise.

## Residual vs lag 1 residual
plot(y=MA_1$residuals, x=zlag(MA_1$residuals, d=1), 
     xlab = 'Lag 1 residual', ylab='residual')

## Residual vs lag 2 residual
plot(y=MA_1$residuals, x=zlag(MA_1$residuals, d=2), 
     xlab = 'Lag 2 residual', ylab='residual')

## Histogram
hist(MA_1$residuals, xlab='Standardized Residual')

##QQ Plot
qqnorm(rstandard(MA_1))
qqline(rstandard(MA_1))


###############
### AR(1)  ####
###############

AR_1 = arima(data.ts, order=c(1, 0, 0))
AR_1
#log likelihood = -38.13,  aic = 80.26
#Stationary

## ACF of residual
acf(as.vector(AR_1$residuals), lag.max = 40)

## Residual Plot
plot(as.vector(AR_1$residuals), type="o", main='Residual Plot of AR(1)')
#Residual hang together too much for white noise.

## Residual vs lag 1 residual
plot(y=AR_1$residuals, x=zlag(AR_1$residuals, d=1), 
     xlab = 'Lag 1 residual', ylab='residual')

## Residual vs lag 2 residual
plot(y=AR_1$residuals, x=zlag(AR_1$residuals, d=2), 
     xlab = 'Lag 2 residual', ylab='residual')

## Histogram
hist(AR_1$residuals, xlab='Standardized Residual')

##QQ Plot
qqnorm(rstandard(AR_1))
qqline(rstandard(AR_1))

#Not as good as MA(1)


###############
### AR(2)  ####
###############

AR_2 = arima(data.ts, order=c(2, 0, 0))
AR_2
#log likelihood = -34.05,  aic = 74.1
#Complex Roots, stationary, roots=1.617+-1.777i

## ACF of residual
acf(as.vector(AR_2$residuals), lag.max = 40)

## Residual Plot
plot(as.vector(AR_2$residuals), type="o", main='Residual Plot of AR(2)')
#Residual hang together too much for white noise.

## Residual vs lag 1 residual
plot(y=AR_2$residuals, x=zlag(AR_2$residuals, d=1), 
     xlab = 'Lag 1 residual', ylab='residual')

## Residual vs lag 2 residual
plot(y=AR_2$residuals, x=zlag(AR_2$residuals, d=2), 
     xlab = 'Lag 2 residual', ylab='residual')

## Histogram
hist(AR_2$residuals, xlab='Standardized Residual')

##QQ Plot
qqnorm(rstandard(AR_2))
qqline(rstandard(AR_2))

########## 这下面使用dynamic method选择 model ########

###################
###  ARMA(3,2)  ###
###################

ARMA_3_2 = arima(data.ts, order=c(3, 0, 2))
ARMA_3_2
# log likelihood = -29.87,  aic = 71.75
#Stationary

## ACF of residual
acf(as.vector(ARMA_3_2$residuals), lag.max = 40)

## Residual Plot
plot(as.vector(ARMA_3_2$residuals), type="o", main='Residual Plot of ARMA(1,2)')

## Residual vs lag 1 residual
plot(y=ARMA_3_2$residuals, x=zlag(ARMA_3_2$residuals, d=1), 
     xlab = 'Lag 1 residual', ylab='residual')

## Residual vs lag 2 residual
plot(y=ARMA_3_2$residuals, x=zlag(ARMA_3_2$residuals, d=2), 
     xlab = 'Lag 2 residual', ylab='residual')


## Histogram
hist(ARMA_3_2$residuals, xlab='Standardized Residual')

##QQ Plot
qqnorm(rstandard(ARMA_3_2))
qqline(rstandard(ARMA_3_2))

###################
###  ARMA(2,2)  ###
###################

ARMA_2_2 = arima(data.ts, order=c(2, 0, 2))
ARMA_2_2
# log likelihood = -32.06,  aic = 74.13
#Stationary

## ACF of residual
acf(as.vector(ARMA_2_2$residuals), lag.max = 40)

## Residual Plot
plot(as.vector(ARMA_2_2$residuals), type="o", main='Residual Plot of ARMA(1,2)')

## Residual vs lag 1 residual
plot(y=ARMA_2_2$residuals, x=zlag(ARMA_2_2$residuals, d=1), 
     xlab = 'Lag 1 residual', ylab='residual')

## Residual vs lag 2 residual
plot(y=ARMA_2_2$residuals, x=zlag(ARMA_2_2$residuals, d=2), 
     xlab = 'Lag 2 residual', ylab='residual')


## Histogram
hist(ARMA_2_2$residuals, xlab='Standardized Residual')

##QQ Plot
qqnorm(rstandard(ARMA_2_2))
qqline(rstandard(ARMA_2_2))


###################
###  ARMA(2,1)  ###
###################

ARMA_2_1 = arima(data.ts, order=c(2, 0, 1))
ARMA_2_1
# log likelihood = -33.02,  aic = 74.03
#Stationary

## ACF of residual
acf(as.vector(ARMA_2_1$residuals), lag.max = 40)

## Residual Plot
plot(as.vector(ARMA_2_1$residuals), type="o", main='Residual Plot of ARMA(1,2)')

## Residual vs lag 1 residual
plot(y=ARMA_2_1$residuals, x=zlag(ARMA_2_1$residuals, d=1), 
     xlab = 'Lag 1 residual', ylab='residual')

## Residual vs lag 2 residual
plot(y=ARMA_2_1$residuals, x=zlag(ARMA_2_1$residuals, d=2), 
     xlab = 'Lag 2 residual', ylab='residual')


## Histogram
hist(ARMA_2_1$residuals, xlab='Standardized Residual')

##QQ Plot
qqnorm(rstandard(ARMA_2_1))
qqline(rstandard(ARMA_2_1))


###################
###  ARMA(1,2)  ###
###################

ARMA_1_2 = arima(data.ts, order=c(1, 0, 2))
ARMA_1_2
# log likelihood = -32.27,  aic = 72.54
#Stationary

## ACF of residual
acf(as.vector(ARMA_1_2$residuals), lag.max = 40)

## Residual Plot
plot(as.vector(ARMA_1_2$residuals), type="o", main='Residual Plot of ARMA(1,2)')

## Residual vs lag 1 residual
plot(y=ARMA_1_2$residuals, x=zlag(ARMA_1_2$residuals, d=1), 
     xlab = 'Lag 1 residual', ylab='residual')

## Residual vs lag 2 residual
plot(y=ARMA_1_2$residuals, x=zlag(ARMA_1_2$residuals, d=2), 
     xlab = 'Lag 2 residual', ylab='residual')


## Histogram
hist(ARMA_1_2$residuals, xlab='Standardized Residual')

##QQ Plot
qqnorm(rstandard(ARMA_1_2))
qqline(rstandard(ARMA_1_2))

######## Dynamic method ARMA（3，2），（2，2），（1，2）出来的结果差不多，
######## 根据principle of parsimony，选variable最少的
######## box-jenkins 选出来ARMA（1，2）最好，所以最后我们选ARMA（1，2）

###################
### Forcasting ###
###################

ARMA_1_2_predict=predict(ARMA_1_2, n.ahead =15, newxreg = NULL, se.fit=TRUE)
ARMA_1_2_predict

plot(as.vector(ARMA_1_2),n.ahead=10 ,xlab='Time', ylab='Inflation',main='Forecasting of 95% CI')
abline(h=coef(ARMA_1_2)[names(coef(ARMA_1_2))=='intercept'])


# Fitted Value VS Original Time Series
plot(window(data.ts, start=c(2000,1)), 
     main="Inflation rate in the US from 2000 to 2022", ylab = "Inflation Rate")
lines(fitted(ARMA_1_2), col="red", lty=1)
legend("bottomleft", c("Original", "Fitted"), 
       col=c("black", "red"), lty=1, bty="n")


###################
### Difference 1###
###################

### Time series plot ###
plot(window(diff(data.ts), start=c(2000,1)), 
     main="Inflation rate in the US from 2000 to 2022", ylab = "Inflation Rate")


### ACF
acf(diff(as.vector(data.ts)), 
    main="Diff Sample ACF for Inflation Rate",lag.max=30)
# IMA(1,2)

### PACF
pacf(diff(as.vector(data.ts)), main=" Diff Sample PACF for Inflation Rate", lag.max=30)


### EACF
eacf(diff(data.ts))
#EACF suggests IMA(1,2)


diff_data = diff(data.ts)

#############
### IMA(1,2)
IMA_2 = arima(data.ts, order=c(0, 1, 2))
IMA_2
# log likelihood = -35.07,  aic = 74.14

########## Check Residual ###########

## ACF of residual
acf(as.vector(IMA_2$residuals), lag.max = 40)

## Residual Plot
plot(as.vector(IMA_2$residuals), type="o")
#Residual hang together too much for white noise.

## Residual vs lag 1 residual
plot(y=IMA_2$residuals, x=zlag(IMA_2$residuals, d=1), 
     xlab = 'Lag 1 residual', ylab='residual')

## Residual vs lag 2 residual
plot(y=IMA_2$residuals, x=zlag(IMA_2$residuals, d=2), 
     xlab = 'Lag 2 residual', ylab='residual')

## Histogram
hist(IMA_2$residuals, xlab='Standardized Residual')

##QQ Plot
qqnorm(rstandard(IMA_2))
qqline(rstandard(IMA_2))

### Predict value VS Original Serie
plot(window(data.ts), start=c(2000,1), 
     main="Inflation rate in the US from 2000 to 2022", ylab = "Inflation Rate")
lines(fitted(IMA_2), col="red", lty=1)
legend("bottomleft", c("Original", "Fitted"), 
       col=c("black", "red"), lty=1, bty="n")

# Not as good as ARMA(1,2)






