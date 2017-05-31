S<-readRDS("c:\\temp\\ts_serie_filtrada.rds")
library(tseries)
library(forecast)
print(adf.test(testDF.inv))
print(kpss.test(testDF.inv))

acf(testDF.inv, lag.max=20)# es estacionaria por pvalor  menor a 0.05
pacf(testDF.inv, lag.max=20)# es estacionaria por pvalor  mayor a 0.05

##luego de comprobar que es estacionaria, debemos seleccionar el mejor modelo candidato
#para esto se necesita examinar el correlograma y el correlograma parcial

SARIMA <- arima(testDF.inv, order=c(5,0,0))
SARIMA
SFORECAST <- forecast.Arima(SARIMA, h=5)
SFORECAST
plot.forecast(SFORECAST)

#chequeamos los errores
acf(SFORECAST$residuals, lag.max=20)
Box.test(SFORECAST$residuals ,lag=20, type="Ljung-Box")

plot.ts(SFORECAST$residuals)

plotForecastErrors <- function(forecasterrors)
{
  
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(SFORECAST$residuals) #distribucion de los errores es normal
