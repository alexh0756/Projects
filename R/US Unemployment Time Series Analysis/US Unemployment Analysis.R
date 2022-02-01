#Packages
library(tidyr)
library(TSA)
library(tseries)
library(forecast)
library(fUnitRoots)
library(lmtest)
library(FitAR)

sort.score <- function(x, score = c("bic", "aic")){
  if (score == "aic"){
    x[with(x, order(AIC)),]
  } else if (score == "bic") {
    x[with(x, order(BIC)),]
  } else {
    warning('score = "x" only accepts valid arguments ("aic","bic")')
  }
}

residual.analysis <- function(model, std = TRUE,start = 2, class = c("ARIMA","GARCH","ARMA-GARCH", "fGARCH")[1]){
  library(TSA)
  library(FitAR)
  if (class == "ARIMA"){
    if (std == TRUE){
      res.model = rstandard(model)
    }else{
      res.model = residuals(model)
    }
  }else if (class == "GARCH"){
    res.model = model$residuals[start:model$n.used]
  }else if (class == "ARMA-GARCH"){
    res.model = model@fit$residuals
  }else if (class == "fGARCH"){
    res.model = model@residuals
  }else {
    stop("The argument 'class' must be either 'ARIMA' or 'GARCH' ")
  }
  par(mfrow=c(3,2))
  plot(res.model,type='o',ylab='Standardised residuals', main="Time series plot of standardised residuals")
  abline(h=0)
  hist(res.model,main="Histogram of standardised residuals")
  qqnorm(res.model,main="QQ plot of standardised residuals")
  qqline(res.model, col = 2)
  acf(res.model,main="ACF of standardised residuals")
  print(shapiro.test(res.model))
  k=0
  LBQPlot(res.model, lag.max = 30, StartLag = k + 1, k = 0, SquaredQ = FALSE)
  par(mfrow=c(1,1))
}

residual.plots <- function(residual.model, data, model_name) {
  par(mfrow=c(3,2))
  plot(y = residual.model, x= as.vector(time(data)),type = 'l',xlab = 'Year', 
       ylab='Standardised Residuals',main =   paste0("Standardised residuals from ", 
                                                     model_name, "."))
  hist(residual.model,xlab='Standardised Residuals', 
       main = "Histogram of standardised residuals.")
  acf(residual.model, main = "ACF of standardized residuals.")
  qqnorm(y=residual.model, main = "QQ plot of standardised residuals.")
  qqline(y=residual.model, col = 2, lwd = 1, lty = 2)
  pacf(residual.model, main = "PACF of standardized residuals.")
  par(mfrow=c(1,1))
}

acf.pacf <- function(data, acf = 'ACF', acfx = NULL, pacf = 'PACF', pacfx = NULL) {
  par(mfrow=c(1,2))
  acf(as.vector(data), main = acf, lag.max = 60, xlab = acfx)
  pacf(as.vector(data), main = pacf, lag.max = 60, xlab = pacfx)
  par(mfrow=c(1,1))
}

#Data Preparation
unemployment <- read.csv('USUnemployment.csv')
colnames(unemployment)[colnames(unemployment) == "ï..Year"] <- "Year"
head(unemployment)
tail(unemployment)
unemployment <- gather(data = unemployment, 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', key = 'Month', value = 'rate')
unemployment <- unemployment[order(unemployment$Year),]
unemployment <- unite(data = unemployment, col = 'Year-Month', 'Year', 'Month', sep = '-')
head(unemployment)
tail(unemployment)
unemployment_TS<- ts(unemployment$rate, start=c(1948,1), end = c(2019,12), frequency = 12)
unemployment_TS

par(mfrow=c(1,1))
plot(unemployment_TS, ylab = "Unemployment Rate (%)", xlab ="Time",type ='l', 
     main = "Unemployment Rate(%) in the United States from 1948 to 2019")
points(y=unemployment_TS,x= time(unemployment_TS), pch = as.vector(season(unemployment_TS)))
                                 

#Fitting a Linear Model
t<-time(unemployment_TS)
t
model1 = lm(unemployment_TS~t)
summary(model1)
plot(unemployment_TS,type='l',ylab='Unemployment Rate(%)', main='Unemployment Rate Linear Model')
abline(model1)

# Residual analysis for Linear Model
res.model1 = rstudent(model1)
x11()
residual.plots(res.model1, unemployment_TS, "linear model")

#Fitting a Quadratic Model
t <- time(unemployment_TS)
t2 <- t^2
model2 = lm(unemployment_TS~ t + t2)
summary(model2)
plot(ts(fitted(model2)), ylim = c(min(c(fitted(model2), as.vector(unemployment_TS))), max(c(fitted(model2),as.vector(unemployment_TS)))),
     ylab='Unemployment Rate (%)' , main = "Unemployment Rate Quadratic Model", type="l",lty=2,col="red")
lines(as.vector(unemployment_TS),type="l")

# Residual analysis for Quadratic Model
res.model2 = rstudent(model2)
x11()
residual.plots(res.model2, unemployment_TS, "quadratic model")

#Fitting a Seasonal Model
month.=season(unemployment_TS) # period added to improve table display and this line sets up indicators
model3=lm(unemployment_TS~month.-1) # -1 removes the intercept term
summary(model3)
plot(ts(fitted(model3)), ylab='Unemployment Rate(%)', main = "Unemployment Rate(%) in the United States",
     ylim = c(min(c(fitted(model3), as.vector(unemployment_TS))),
              max(c(fitted(model3), as.vector(unemployment_TS)))
     ), col = "red" )
lines(as.vector(unemployment_TS),type="l")

# Residual analysis for Seasonal Model
res.model3 = rstudent(model3)
x11() 
residual.plots(res.model3, unemployment_TS, "seasonal model")

# Fitting a Harmonic Model
har.=harmonic(unemployment_TS,1)
model.unemployment_TS.har=lm(unemployment_TS~har.)
summary(model.unemployment_TS.har)

plot(ts(fitted(model.unemployment_TS.har)), ylim = c(min(c(fitted(model.unemployment_TS.har),
                                                         as.vector(unemployment_TS))), max(c(fitted(model.unemployment_TS.har),as.vector(unemployment_TS)))),
     ylab='y' , main = "Fitted Harmonic Model", type="l",lty=2,col="red")
lines(as.vector(unemployment_TS),type="o")

# Residual analysis for Harmonic Model
res.model.unemployment_TS.har = rstudent(model.unemployment_TS.har)
x11() # Use this for Mac computers
residual.plots(res.model.unemployment_TS.har, unemployment_TS, "harmonic model")

#Fit seasonal + Quadratic
t = time(unemployment_TS)
t2 = t^2
model5 = lm(unemployment_TS~ month. + t + t2 -1) # label the quadratic trend model as model1
summary(model5)

plot(ts(fitted(model5)), ylim = c(min(c(fitted(model5), as.vector(unemployment_TS))), max(c(fitted(model5),as.vector(unemployment_TS)))),
     ylab='y' , main = "Fitted seasonal plus quadratic curve", type="l",lty=2,col="red")
lines(as.vector(unemployment_TS),type="o")

# Residual analysis for seasonal + Quadratic
res.model5 = rstudent(model5)
x11() 
residual.plots(res.model5, unemployment_TS, "seasonal + quadratic model")


#Predicting the Future with seasonal Model
original.t <- time(unemployment_TS) 
n <- length(original.t) 
lastTimePoint <- original.t[n]
aheadTimes <- data.frame(
  month. = c("January", "February", "March", "April",
             "May", "June", "July", "August", "September",
             "October", "November", "December"))
unemploymentFC <- predict(model3, newdata = aheadTimes, interval = "prediction")
unemploymentFC

plot(unemployment_TS, xlim= c(1948,2022), ylim = c(0,12),
     ylab = "Unemployment Rate (%)",
     main = "Unemployment Forecast for next 12 months.")
for (i in 1:3) {
  colour <- c("red", "blue", "blue")
  lines(ts(as.vector(unemploymentFC[,i]), start = c(2020, 1), frequency = 12), col = colour[i], type = "l")
}
legend("bottomleft", lty = 1, pch = 1, col = c("black", "blue", "red"), text.width = 20,
       c("Data", "5% forecast limits", "Forecasts"))

#ARIMA Model Development

acf.pacf(unemployment_TS, acf = "Original ACF", pacf = "Original PACF")
#A decaying Wave-like pattern (supports non-stationary) shown in ACF and one extreme lag in PACF at 1 and another at lag 4.

adf.test(unemployment_TS, alternative = c("stationary"))
#p-value is borderline, being less than 0.05 and we therefore would reject the Null and claim it is stationary 

pp.test(unemployment_TS, alternative = c("stationary"))
#p-value is greater than 0.05, we therefore fail to reject the null. Data is non-stationary.

unemployment_TS_Diff<- diff(unemployment_TS, differences = 1)
par(mfrow=c(1,1))
plot(unemployment_TS_Diff, ylab = "Unemployment Rate (%)", xlab ="Time", main = "First Difference")
#Plot looking better, no real trend can be seen upon first difference, without transformation. 

acf.pacf(unemployment_TS_Diff, acf = "ACF of the first difference", pacf = "PACF of the first difference")

adf.test(unemployment_TS_Diff, alternative = c("stationary"))
#p-value is less than 0.05, being less than 0.05 and we therefore would reject the Null and claim it is stationary 

#Although, there is no immediate need to transform data, we will explore the idea.

unemployment_TS_Log<-log(unemployment_TS)
plot(unemployment_TS_Log, ylab = "Unemployment Rate", xlab = "Time", type = "o", main = "log Transofrmation")
unemployment_TS_Log_Diff<-diff(log(unemployment_TS), differences = 1)
plot(unemployment_TS_Log_Diff, xlab = "Time", ylab = "Unemployment Rate (%)", type = "o",
     main = "Log Transformation and first difference")
acf.pacf(unemployment_TS_Log_Diff, acf = "ACF of Log and first difference", pacf = "PACF of Log and first difference")

#ARIMA(5,1,6), (5,1,7)

adf.test(unemployment_TS_Log_Diff, alternative = c("stationary"))
#p-value is less than 0.05 and we therefore reject the Null.
# stationary 

#EACF
eacf(unemployment_TS_Log_Diff)

#ARIMA(1,1,5), (2,1,5), (1,1,6), (2,1,6)

#BIC Table
par(mfrow=c(1,1))
res = armasubsets(y=unemployment_TS_Log_Diff, nar=7 , nma=7, y.name='unemployment', ar.method='ols')
plot(res)
#P = 2,3; Q = 4, 5
#ARIMA(2,1,4), (2,1,5), (3,1,4), (3,1,5)

#Fitting the ARIMA Models

orders <- list(c(2,1,4), c(2,1,5), c(3,1,4), c(3,1,5), c(1,1,5), c(1,1,6), c(2,1,6), c(5,1,6), c(5,1,7))
for (i in 1:length(orders)) {
  for (r in c('CSS', 'ML')) {
    ord <- as.numeric(unlist(orders[i]))
    name <- paste0('model_', lapply(orders, `[[`, 1)[i], lapply(orders, `[[`, 2)[i], lapply(orders, `[[`, 3)[i], '_', r)
    model <- arima(unemployment_TS, order = ord, method = r)
    assign(name, model)
  }
}

#ARIMA (2,1,4) - significant
coeftest(model_214_CSS)
coeftest(model_214_ML)

#ARIMA (2,1,5) - insignificant
coeftest(model_215_CSS)
coeftest(model_215_ML)

#ARIMA (3,1,4) - insig
coeftest(model_314_CSS)
coeftest(model_314_ML)

#ARIMA (3,1,5) insig ML, Sig CSS
coeftest(model_315_CSS)
coeftest(model_315_ML)

#ARIMA (1,1,5) - insig
coeftest(model_115_CSS)
coeftest(model_115_ML)

#ARIMA (1,1,6) - insig
coeftest(model_116_CSS)
coeftest(model_116_ML)

#ARIMA (2,1,6) - insig
coeftest(model_216_CSS)
coeftest(model_216_ML)

#ARIMA (5,1,6) -insig
coeftest(model_516_CSS)
coeftest(model_516_ML)

#ARIMA (5,1,7)
coeftest(model_517_CSS)
coeftest(model_517_ML)

# AIC and BIC values
sort.score(AIC(model_517_ML,model_516_ML,model_116_ML,model_216_ML,model_215_ML,model_115_ML, model_315_ML, model_314_ML, model_214_ML), score = "aic")
sort.score(BIC(model_517_ML,model_516_ML,model_116_ML,model_216_ML,model_215_ML,model_115_ML, model_315_ML, model_314_ML, model_214_ML), score = "bic" )
#Model_214_ML is best, as it has fewer parameters and appears
#in the top models of both AIC and BIC. Model_516_ML look promising, but has many parameters.

#Residual Analysis Model_214
x11(); residual.analysis(model = model_214_ML)
x11(); residual.analysis(model = model_214_CSS)

#Residual Analysis Model_516
x11(); residual.analysis(model = model_516_ML)
x11(); residual.analysis(model = model_516_CSS)

#Residual Analysis Model_517
x11(); residual.analysis(model = model_517_ML)
x11(); residual.analysis(model = model_517_CSS)

#Residual Analysis Model_115
x11(); residual.analysis(model = model_115_ML)
x11(); residual.analysis(model = model_115_CSS)

#Residual Analysis Model_215
x11(); residual.analysis(model = model_215_ML)
x11(); residual.analysis(model = model_215_CSS)

# Error measures for fitted ARIMA models
orders <- list(c(2,1,4), c(5,1,6), c(5,1,7), c(1,1,5))
for (i in 1:length(orders)) {
  for (r in c('CSS', 'ML')) {
    ord <- as.numeric(unlist(orders[i]))
    name <- paste0('error', lapply(orders, `[[`, 1)[i], lapply(orders, `[[`, 2)[i], lapply(orders, `[[`, 3)[i], r)
    model <- Arima(unemployment_TS,order = ord, method = c(r))
    assign(name, model)
  }
}

summary(error214ML)
summary(error214CSS)
summary(error516ML)
summary(error516CSS)
summary(error517ML)
summary(error517CSS)
summary(error115ML)
summary(error115CSS)

#Forecasting using ARIMA(5,1,6) Model
fit = Arima(unemployment_TS,c(5,1,6))
fitFrc = forecast(fit,h=30)
fitFrc
par(mfrow=c(1,1))
plot(fitFrc, ylab = "Unemployment Rate (%)", xlab ="Year")


# SARIMA Modeling
acf.pacf(unemployment_TS, acf = 'ACF of Unemployment', pacf = 'PACF of Unemployment')
#seasonal behavior in plot requires differencing

adf.test(unemployment_TS)
#adf test says we don't need differencing, but it might benefit from some differencing

diff_TS_season = diff(unemployment_TS, period = 12)
par(mfrow = c(1,1))
plot(diff_TS_season)
acf.pacf(unemployment_TS)


diff_TS = diff(unemployment_TS)
par(mfrow=c(1,1))
plot(diff_TS)
acf.pacf(diff_TS)
# acf hints at correlations at lags 12, 24, and 36, meaning a potential period of 12

# doing seasonal difference D=1 since the plots the seasonal and seasonal+quadratic models
diff2_TS = diff(diff_TS, lag = 12)
par(mfrow = c(1,1))
plot(diff2_TS)
par(mfrow = c(1,2))
acf(diff2_TS, lag.max = 60)
pacf(diff2_TS, lag.max = 60)

#SARIMA(6,1,6)x(0,1,1)

m1 = arima(unemployment_TS, order = c(0,0,0), seasonal = list(order = c(0,1,0), period = 12))
res.m1 = residuals(m1)
plot(res.m1, xlab = 'Time', ylab = 'Standardised Residuals', main = 'First Seasonal Difference (Period 12)')
acf.pacf(res.m1, acf = 'ACF of First Seasonal Difference (Period 12)', pacf = 'PACF of First Seasonal Difference (Period 12)')

m2 = arima(unemployment_TS, order = c(0,1,0), seasonal = list(order = c(0,1,0), period = 12))
res.m2 = residuals(m2)
plot(res.m2, xlab = 'Time', ylab = 'Standardised Residuals', main = 'First Difference and First Seasonal Difference')
acf.pacf(res.m2, acf = 'ACF of SARIMA(0,1,0)x(0,1,0)_{12}', pacf = 'PACF of SARIMA(0,1,0)x(0,1,0)_{12}')

m3 = arima(unemployment_TS, order = c(0,1,0), seasonal = list(order = c(0,1,1), period = 12))
res.m3 = residuals(m3)
plot(res.m3, xlab = 'Time', ylab = 'Standardised Residuals', main = 'SARIMA(0,1,0)x(0,1,1)_{12}')
acf.pacf(res.m3, acf = 'ACF of SARIMA(0,1,0)x(0,1,1)_{12}', pacf = 'PACF of SARIMA(0,1,0)x(0,1,1)_{12}')

m4 = arima(unemployment_TS, order = c(0,1,0), seasonal = list(order = c(3,1,1), period = 12))
res.m4 = residuals(m4)
plot(res.m4, xlab = 'Time', ylab = 'Standardised Residuals', main = 'SARIMA(0,1,0)x(3,1,1)_{12}')
acf.pacf(res.m4, acf = 'ACF of SARIMA(0,1,0)x(3,1,1)_{12}', pacf = 'PACF of SARIMA(0,1,0)x(3,1,1)_{12}')

m5 = arima(unemployment_TS, order = c(6,1,8), seasonal = list(order = c(3,1,1), period = 12))
res.m5 = residuals(m5)
plot(res.m5, xlab = 'Time', ylab = 'Standardised Residuals', main = 'SARIMA(6,1,8)x(3,1,1)_{12}')
acf.pacf(res.m5, acf = 'ACF of SARIMA(6,1,8)x(3,1,1)_{12}', pacf = 'PACF of SARIMA(6,1,8)x(3,1,1)_{12}')

#SARIMA(6,1,8)x(3,1,1)_12 seems to be a good model so far

#Check eacf for 1 regular differencing and 1 seasonal differencing
eacf(res.m4)
# model found: SARIMA(3,1,6)x(3,1,1)_12
# model found: SARIMA(3,1,7)x(3,1,1)_12
# model found: SARIMA(4,1,6)x(3,1,1)_12
# model foundL SARIMA(4,1,7)x(3,1,1)_12

#fitting SARIMA models

orders <- list(c(6,1,8), c(3,1,5), c(3,1,6), c(4,1,5), c(4,1,6))
for (i in 1:length(orders)) {
  for (r in c('CSS', 'ML')) {
    ord <- as.numeric(unlist(orders[i]))
    name <- paste0('sarima_', lapply(orders, `[[`, 1)[i], lapply(orders, `[[`, 2)[i], lapply(orders, `[[`, 3)[i], '_', r)
    model <- arima(unemployment_TS, order = ord, seasonal = list(order = c(3,1,1), period = 12), method = r)
    assign(name, model)
  }
}

#SARIMA(6,1,8)x(3,1,1)_12
coeftest(sarima_618_CSS)
coeftest(sarima_618_ML)

#SARIMA(3,1,5)x(3,1,1)_12
coeftest(sarima_315_CSS)
coeftest(sarima_315_ML)

#SARIMA(3,1,6)x(3,1,1)_12
coeftest(sarima_316_CSS)
coeftest(sarima_316_ML)

#SARIMA(4,1,5)x(3,1,1)_12
coeftest(sarima_415_CSS)
coeftest(sarima_415_ML)

#SARIMA(4,1,6)x(3,1,1)_12
coeftest(sarima_416_CSS)
coeftest(sarima_416_ML)

# AIC and BIC values
sort.score(AIC(sarima_618_ML, sarima_315_ML, sarima_316_ML, sarima_415_ML, sarima_416_ML), score = "aic")
sort.score(BIC(sarima_618_ML, sarima_315_ML, sarima_316_ML, sarima_415_ML, sarima_416_ML), score = "bic")
#SARMA models are at the bottom of the AIC and BIC scores, meaning that they don't do as well

#Residual Analysis SARIMA_618
residual.analysis(sarima_618_ML)
residual.analysis(sarima_618_CSS)

#Residual Analysis SARIMA_315
residual.analysis(sarima_315_ML)
residual.analysis(sarima_315_CSS)

#Residual Analysis SARIMA_316
residual.analysis(sarima_316_ML)
residual.analysis(sarima_316_CSS)

#Residual Analysis SARIMA_415
residual.analysis(sarima_415_ML)
residual.analysis(sarima_415_CSS)

#Residual Analysis SARIMA_416
residual.analysis(sarima_416_ML)
residual.analysis(sarima_416_CSS)

# Error measures for fitted SARIMA models
orders <- list(c(3,1,5), c(3,1,6), c(4,1,5), c(4,1,6))
for (i in 1:length(orders)) {
  for (r in c('CSS', 'ML')) {
    ord <- as.numeric(unlist(orders[i]))
    name <- paste0('serror', lapply(orders, `[[`, 1)[i], lapply(orders, `[[`, 2)[i], lapply(orders, `[[`, 3)[i], r)
    model <- Arima(unemployment_TS,order = ord, seasonal = c(3, 1, 1), method = c(r))
    assign(name, model)
  }
}

summary(serror315ML)
summary(serror315CSS)
summary(serror316ML)
summary(serror316CSS)
summary(serror415ML)
summary(serror415CSS)
summary(serror416ML)
summary(serror416CSS)


#Forecasting with SARIMA
fit3 = Arima(unemployment_TS, order = c(3,1,5), seasonal = c(3,1,1))
fit3_forecast = forecast(fit3, h = 30)
fit3_forecast
par(mfrow=c(1,1))
plot(fit3_forecast, main = 'Forecasts from SARIMA(3,1,5)x(3,1,1)_{12}', xlab = 'Time', ylab = 'Unemployment Rate (%)')
mean(fit3_forecast$mean)



