#codigo ordenado parte 1 # Sat May 27 14:40:15 2017 ------------------------------

#se plotea la serie

plot(agrupada$fecha,agrupada$tasa_aciertos,type = "l",xlab = "fecha",ylab = "tasa de aciertos")

#Se suaviza la serie con un filtro de media movil y se utiliza Parseval para ver si con 10 componenetes de la frecuencia se puede explicar la serie.

mv10 <- stats::filter(ts(agrupada[,3]), rep(1/10,10) , circular =TRUE)

plot(agrupada$fecha,mv10,type = "l",xlab = "fecha",ylab = "tasa de aciertos")

#se aplica filtro
fft_serie <- Mod(fft(mv10))

plot(fft_serie)

#se aplica el criterio de parseval
energia_t <- sum(mv10*mv10)

energia_fft <- sum(fft_serie*fft_serie)/length(fft_serie)

fft_serie_ord <-  sort(fft_serie,decreasing = TRUE)

head(fft_serie_ord,10)

valores <- unique(head(fft_serie_ord,10))

energia_acumulada <- sum(valores * valores) / length(fft_serie)

energia_acumulada/energia_t

#con 10 componentes de la frecuencia se puede explicar la serie ya que explica mas del 90%

#se crean los filtros pasa bajos
media_lean=Mod(fft_serie[1])/N.1

fft_serie <-(fft(mv10))
plot(Mod(fft_serie))
fft_serie[1]=0
plot(Mod(fft_serie))
filtroDF = rep(1,length(fft_serie))
filtroDF [1:10] = 0
filtroDF [(length(fft_serie)-10):length(fft_serie)] = 0
fft.test = filtroDF*fft_serie
plot(Mod(fft.test))
op = par(mfrow = c(1, 2))


N.1= length(fft_serie)

tiempo1= 0:(N.1-1)

plot(tiempo1, filtroDF , type="l") #pasa bajos

plot(Mod(fft_serie),type="l")

op = par(mfrow = c(1, 1))

plot(Mod(fft_serie)/N.1)

testDF.inv =  Re(fft((fft.test),inverse=TRUE)/N.1)

media_sumarla = rep(media_lean,length(fft_serie))


testDF.inv=testDF.inv+media_sumarla
op = par(mfrow = c(1, 1))


plot(agrupada$fecha,testDF.inv,type="l",xlab = "fecha",ylab = "tasa de acierto") 
lines(agrupada$fecha,mv10,col="red",type = 'l')

