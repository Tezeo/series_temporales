#descomposicion y forecast de serie filtrada

library(dplyr)
library(xts)
library(TTR)
library(forecast)

fechas <- data.frame(fechas= seq(as.Date("2016-04-01"), as.Date("2017-03-31"), by="days"))

agrupada_completa <- fechas %>% left_join(agrupada[,c(1,3)],by=c("fechas"="fecha"))


agrupada_ts <- ts(agrupada_completa[,c(2)],start = c(2016,4,1),frequency = 365)

plot(testDF.inv)
plot(agrupada_ts)

x <- xts(testDF.inv,agrupada$fecha)

plot(x)

componentes_agrupada <- as.ts(x)

plot.ts(componentes_agrupada)

plot.ts(ts(x,start=c(2016,4,1),frequency=365))

plot(componentes_agrupada)

attr(testDF.inv, 'frequency') <- 1

periodicity(x)             # check periodicity: weekly 
plot(decompose(x))  # Decompose after conversion to ts


xtimeseriescomponents <- decompose(as.ts(x))
xtimeseriesseasonallyadjusted <- as.ts(x) - xtimeseriescomponents$seasonal
plot(xtimeseriesseasonallyadjusted)

#forecast

cobrosseriesforecasts <- HoltWinters(x, beta=FALSE, gamma=FALSE)

cobrosseriesforecasts

cobrosseriesforecasts$fitted
cobrosseriesforecasts$SSE
plot(cobrosseriesforecasts)

library(forecast)
cobro.forecast <- forecast.HoltWinters(cobrosseriesforecasts,h=20)

plot(cobro.forecast)

plot.forecast(cobro.forecast)


#autocorrelacion de las serie

acf(cobro.forecast$residuals,na.action = na.pass,lag.max = 20)
Box.test(cobro.forecast$residuals, type="Ljung-Box")


plotForecastErrors <- function(forecasterrors) {
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid: mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors: points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(cobro.forecast$residuals)


#ver estacionalidad

library(tseries)

adf.test(x)

kpss.test(agrupada_ts)


#forecasting

fit.tslm <- tslm(componentes_agrupada ~ trend)
f <- forecast( fit.tslm, h=20,level=c(80,95),fan = TRUE) 
plot(f, ylab="x", xlab="tiempo")
lines ( fitted ( fit.tslm),col="blue")
summary(fit.tslm)

res <- ts(resid( fit.tslm))
plot.ts(res,ylab="res (x)") 
abline (0,0)
Acf(res)

library(lmtest)

print(dwtest(fit.tslm, alt="two.sided"))
par(mfrow=c(1,1))
bins <- hist(res, breaks="FD", xlab="Residuos",
             main="Histograma de residuos") 
xx <- -0.06:0.08
lines(xx, 1300*dnorm(xx,0,sd(res)),col=2)
