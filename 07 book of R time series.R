#ts analisis
library(dplyr)
library(xts)
library(TTR)

fechas <- data.frame(fechas= seq(as.Date("2016-04-01"), as.Date("2017-03-31"), by="days"))

agrupada_completa <- fechas %>% left_join(agrupada[,c(1,3)],by=c("fechas"="fecha"))
agrupada_completa$tasa_aciertos[is.na(agrupada_completa$tasa_aciertos)==TRUE] <- 0

agrupada_ts <- ts(agrupada_completa[,c(2)],start = c(2016,4,1),frequency = 365)

plot.ts(agrupada_ts)

x <- xts(agrupada_completa$tasa_aciertos,agrupada_completa$fechas)

plot(x)

xSMA3 <- SMA(x,n=3)
plot(xSMA3)

componentes_agrupada <- as.ts(x)

plot.ts(componentes_agrupada)
options(OutDec= ".")
plot.ts(ts(x,start=c(2016,4,1),frequency=365))

plot(x)

attr(x, 'frequency') <- 20 

periodicity(x)             # check periodicity: weekly 
plot(decompose(as.ts(x)))  # Decompose after conversion to ts


xtimeseriescomponents <- decompose(as.ts(x))
xtimeseriesseasonallyadjusted <- as.ts(x) - xtimeseriescomponents$seasonal
plot(xtimeseriesseasonallyadjusted)


#autocorrelacion de las serie

acf(x,lag.max = N)

