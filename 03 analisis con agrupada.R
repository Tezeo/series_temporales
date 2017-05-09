#agrupada 

with(agrupada,plot(fecha,scale(tasa_aciertos,scale = TRUE),type='l'))
with(agrupada,lines(fecha,scale(cantidad_aciertos),type='l',col="red"))

with(agrupada,plot(fecha,tasa_aciertos,type='l'))

fft.anual.tasa <- fft(agrupada$tasa_aciertos)
plot(Mod(fft.anual.tasa),type = 'l') #se muestra solo el modulo y no la fase(argumento), la fase es dificil de explicar


# remover la componente de frecuencia cero (DC)
fft.anual.tasa[1] = 0 
plot(Mod(fft.anual.tasa),type='l')

#filtros
filtroDF = rep(1,length(fft.anual.tasa))
filtroDF [25:220] = 0
filtroDF [(length(fft.anual.tasa)-220):length(fft.anual.tasa)-25] = 0

fft.anual.tasaDF = filtroDF*fft.anual.tasa

op = par(mfrow = c(1, 2))


N.1= length(fft.anual.tasa)

tiempo1= 0:(N.1-1)

plot(tiempo1, filtroDF , type="l") #pasa altos

plot(Mod(fft.anual.tasaDF),type="l")

anualDF.inv =  Re(fft(fft.anual.tasaDF,inverse=TRUE)/N.1)
op = par(mfrow = c(1, 1))

plot(tiempo1,anualDF.inv,type="l") 
lines (tiempo1,agrupada$tasa_aciertos,col="red")
