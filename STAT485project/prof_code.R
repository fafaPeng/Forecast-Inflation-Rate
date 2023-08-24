data(ima22.s) #Simulated data for IMA(2,2) illustrated in Exhibit 5.5 of the textbook
plot(ima22.s,ylab="IMA(2,2) Simulation",type='o') # data shows a stochastic upward trend
acf(ima22.s) # Suggesting an MA(9) but...
pacf(ima22.s) # Suggesting an AR(1) (Fitting and Diagnostics needed)
eacf(ima22.s) # Suggesting an ARMA(1,2) if ignoring one "X" in (1,3), or an ARMA(2,2)
plot(ima22.s,ylab="IMA(2,2) Simulation",type='o')
arima(ima22.s,order=c(1,0,0),include.mean=FALSE) # Fitting an AR(1) with mean 0, Failed
arima(ima22.s,order=c(1,0,0)) # Fitting an AR(1) with mean mu: High corr at lag 1, mu=12
mean(ima22.s) # sample average of the observations: 21.5
res=residuals(arima(ima22.s,order=c(1,0,0)))
plot(res) # Residuals look random
plot(y=res,x=zlag(res,1))
plot(y=res,x=zlag(res,2)) # Possible positive corr at lag 2
acf(res) # corr at lag 2 about 0.1 but within CI
# Studying the first-differenced series, i.e. ARIMA(p,1,q), to "remove" the
trend
plot(diff(ima22.s),ylab='First Difference',type='o') # Possible downward
trend
ima22_d1=diff(ima22.s,difference=1) # First-differenced observations
acf(ima22_d1) # Possible MA(1) or MA(3), ignoring corr at lag 9
pacf(ima22_d1) # Possible AR(2) or AR(3)
eacf(ima22_d1) # No obvious choices. Perhaps ARMA(1,2), ARMA(2,2) or
ARMA(2,3)
arima(ima22.s,order=c(2,1,0)) # Fitting ARIMA(2,1,0) to the original series,
log_L=-101.2
arima(ima22.s,order=c(1,1,2)) # Fitting ARIMA(1,1,2) to the original series,
log_L=-94.8
res=residuals(arima(ima22.s,order=c(1,1,2)))
plot(res)
plot(y=res,x=zlag(res,1))
plot(y=res,x=zlag(res,2))
acf(res) # Residuals for fitted ARIMA(1,1,2) look random
arima(ima22.s,order=c(2,1,2)) # Fitting ARIMA(2,1,2) to the original series,
perhaps phi_2=0 so not needed, log_L=-94.8
# Could try other models.
# From the models fitted, ARIMA(1,1,2) is good candidate, ARIMA(2,1,0) may also
be considered
# Studying the second-differenced series, i.e. ARIMA(p,2,q), to "remove" the
downward trend in the first-differenced series
plot(diff(ima22.s,difference=2),ylab='Differenced Twice',type='o')
# Note: diff(ima22.s,2) is the lag 2 difference, ima22.s(t)-ima22.s(t-2), not
the second order difference.
ima22_d2=diff(ima22.s,difference=2)
acf(ima22_d2) # Possibly MA(2)
pacf(ima22_d2) # Possibly AR(2)
eacf(ima22_d2) # Possibly ARMA(1,1) or ARMA(1,2)
# Could try different ARIMA(p,2,q) models.
# Dynamics method: Trying ARMA(p,p-1) for original series, 1st and 2nddifferenced series
plot(ima22.s,ylab="IMA(2,2) Simulation",type='o')
arima(ima22.s,order=c(1,0,0)) # Phi_1 close to 1, large response time,
log_L=-117.4
arima(ima22.s,order=c(2,0,1)) # Phi_2>0 and close to 1. log_L=-116.2 (not
much improvement for 2 additional parameters)
arima(ima22.s,order=c(1,0,1)) # Adding an MA term to ARIMA(1,0,0) doesn't
help
arima(ima22.s,order=c(3,0,2)) # Phi_3 close to 0, log_L=-97.4
arima(ima22.s,order=c(2,0,2)) # Similar fit without phi_3, (parsimony)
arima(ima22.s,order=c(1,1,0)) # Trying AR(1) with first order differencing.
Phi_1 close to 0 so possibly white noise
acf(ima22_d1) # However, ACF not consistent with white noise,
lag 2 and 4 corr
arima(ima22.s,order=c(2,1,1)) # Possible candidate, log_L=-97.8, more
diagnostics needed
arima(ima22.s,order=c(3,1,2)) # Phi_2 and phi_3 close to 0, log_L=-94.1
arima(ima22.s,order=c(1,1,2)) # Possible candidate, log_L=-94.8
arima(ima22.s,order=c(1,2,0)) # Trying AR(1) with second order
differencing.log_L=-103.5
arima(ima22.s,order=c(2,2,1)) # Phi_2 close to 0, log_L=-97.1
arima(ima22.s,order=c(1,2,1)) # Possible candidate, log_L=-97.9 similar to
above model
arima(ima22_d2,order=c(2,0,1)) # Checking fitted model using the seconddifferenced series. Very similar results as (2,2,1) above, as expected.
arima(ima22.s,order=c(3,2,2)) # Fit looks good but log_L=-94.4. Question:
should we add 3 parameters? Models (1,2,1) and (1,1,2) are just as good.
arima(ima22.s,order=c(0,2,2)) # Trying a special case of (3,2,2) model,
possible candidate, log_L=-96.1
# Could try other special cases and should do model diagnostics
# Preliminary analysis suggests one of ARIMA(2,0,2), (2,1,1), (1,1,2), (1,2,1),(0,2,2)
# Interesting note: they all have p+d+q=4
# Trying different estimation methods
# Looking only at the AR(1) with the original series ima22.s
#
plot(ima22.s,ylab="IMA(2,2) Simulation",type='o')
arima(ima22.s,order=c(1,0,0)) #default is CSS-ML
arima(ima22.s,order=c(1,0,0),method="CSS-ML")
plot(ima22.s,ylab="IMA(2,2) Simulation",type='o');abline(h=12.05)
arima(ima22.s,order=c(1,0,0),method="ML")
arima(ima22.s,order=c(1,0,0),method="CSS")
plot(ima22.s,ylab="IMA(2,2) Simulation",type='o');abline(h=26.73)
# Deciding on the parameter mu is a difficult task here. Expert knowledge would
be nice.
