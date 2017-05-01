sumatorio.cliente.habil <- aggregate(todos_cruces$target,FUN=sum,by=list(todos_cruces$N))
colnames(sumatorio.cliente.habil) <- c("dia","cantidad")

with(sumatorio.cliente.habil,plot(cantidad ~ dia,type='l'))

sumatorio.cliente.anual <- aggregate(todos_cruces$target,FUN=sum,by=list(todos_cruces$dias))
colnames(sumatorio.cliente.anual) <- c("dia","cantidad")

with(sumatorio.cliente.anual,plot(cantidad ~ dia,type='l'))
abline(h=mean(sumatorio.cliente.anual$cantidad),lty=2)
abline(h=mean(sumatorio.cliente.anual$cantidad)+2*sd(sumatorio.cliente.anual$cantidad),lty=3) 
abline(h=mean(sumatorio.cliente.anual$cantidad)-2*sd(sumatorio.cliente.anual$cantidad),lty=3)

boxplot(sumatorio.cliente.anual$cantidad)

hist(sumatorio.cliente.anual$cantidad)

#transformada de fourier (rapida)

fft.anual <- fft(sumatorio.cliente.anual$cantidad)

plot(Mod(fft.anual),type = 'l') #se muestra solo el modulo y no la fase(argumento), la fase es dificil de explicar

fft.anual.inv = Re(fft(fft.anual, inverse =TRUE)/N)
plot(sumatorio.cliente.anual$dia,fft.anual.inv,type = "l")

# remover la componente de frecuencia cero (DC)
fft.anual[1] = 0 
plot(Mod(fft.anual),type='l')

#filtros
filtroDF = rep(1,length(fft.anual))
filtroDF [1:15] = 0
filtroDF [(length(fft.anual)-15):length(fft.anual)] = 0

fft.anualDF = filtroDF*fft.anual

op = par(mfrow = c(1, 2))


N.1= length(fft.anual)

tiempo1= 0:(N.1-1)

plot(tiempo1, filtroDF , type="l") #pasa altos

plot(Mod(fft.anualDF),type="l")

anualDF.inv =  Re(fft(fft.anualDF,inverse=TRUE)/N.1)
op = par(mfrow = c(1, 1))

plot(tiempo1,anualDF.inv,type="l") 
lines (tiempo1,sumatorio.cliente.anual$cantidad,col="red")

#filtros media movil
sum.anual.MA3 =
  filter( sumatorio.cliente.anual$cantidad , rep(1/3,3) , circular =TRUE)
lines (sumatorio.cliente.anual$dia,sum.anual.MA3,col="red")
