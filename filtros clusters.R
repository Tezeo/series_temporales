#filtros en las fft

# remover la componente de frecuencia cero (DC)
fft.c1[1] = 0 
plot(Mod(fft.c1),type='l')

abline(h=mean(Mod(fft.c1)))

#filtros

filtroDF = rep(1,length(fft.c1))


filtroDF [(Mod(fft.c1)<mean(Mod(fft.c1)))] = 0

fft.c1.tasaDF = filtroDF*fft.c1

op = par(mfrow = c(1, 2))


N.1= length(fft.c1)

tiempo1= 0:(N.1-1)

plot(tiempo1, filtroDF , type="l") #pasa altos

plot(Mod(fft.c1.tasaDF),type="l")

fft.c1.tasaDF.inv =  Re(fft(fft.c1.tasaDF,inverse=TRUE)/N.1)
op = par(mfrow = c(1, 1))

plot(unique(agrupada_nueva_cuotas$dias),fft.c1.tasaDF.inv,type="l") 
with(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==1,],lines(tasa_acierto ~ dias,type='l',col='red'))

#filtro cluster 2

fft.c2[1] = 0 
plot(Mod(fft.c2),type='l')

abline(h=mean(Mod(fft.c2)))

#filtros

filtroDF = rep(1,length(fft.c2))


filtroDF [(Mod(fft.c2)<mean(Mod(fft.c2)))] = 0

fft.c2.tasaDF = filtroDF*fft.c2

op = par(mfrow = c(1, 2))


N.1= length(fft.c2)

tiempo1= 0:(N.1-1)

plot(tiempo1, filtroDF , type="l") #pasa altos

plot(Mod(fft.c2.tasaDF),type="l")

fft.c2.tasaDF.inv =  Re(fft(fft.c2.tasaDF,inverse=TRUE)/N.1)
op = par(mfrow = c(1, 1))

plot(unique(agrupada_nueva_cuotas$dias),fft.c2.tasaDF.inv,type="l") 
with(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==2,],lines(tasa_acierto ~ dias,type='l',col='red'))


#filtro 3

fft.c3[1] = 0 
plot(Mod(fft.c3),type='l')

abline(h=mean(Mod(fft.c3)))

#filtros

filtroDF = rep(1,length(fft.c3))


filtroDF [(Mod(fft.c3)<mean(Mod(fft.c3)))] = 0

fft.c3.tasaDF = filtroDF*fft.c3

op = par(mfrow = c(1, 2))


N.1= length(fft.c3)

tiempo1= 0:(N.1-1)

plot(tiempo1, filtroDF , type="l") #pasa altos

plot(Mod(fft.c3.tasaDF),type="l")

fft.c3.tasaDF.inv =  Re(fft(fft.c3.tasaDF,inverse=TRUE)/N.1)
op = par(mfrow = c(1, 1))

plot(unique(agrupada_nueva_cuotas$dias),fft.c3.tasaDF.inv,type="l") 
with(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==3,],lines(tasa_acierto ~ dias,type='l',col='red'))

plot(unique(agrupada_nueva_cuotas$dias),fft.c2.tasaDF.inv,type='l')
lines(unique(agrupada_nueva_cuotas$dias),fft.c1.tasaDF.inv,type='l',col='red')
lines(unique(agrupada_nueva_cuotas$dias),fft.c3.tasaDF.inv,type='l',col='yellow')

sacar <- fft.c1.tasaDF.inv
sacar[sacar<0] <- 0 
plot(unique(agrupada_nueva_cuotas$dias),sacar,type='l')


sacar2 <- fft.c2.tasaDF.inv
sacar2[sacar2<0] <- 0 
plot(unique(agrupada_nueva_cuotas$dias),sacar2,type='l')

sacar3 <- fft.c3.tasaDF.inv
sacar3[sacar3<0] <- 0 
plot(unique(agrupada_nueva_cuotas$dias),sacar3,type='l')

plot(unique(agrupada_nueva_cuotas$dias),sacar2,type='l')
lines(unique(agrupada_nueva_cuotas$dias),sacar,type='l',col='red')
lines(unique(agrupada_nueva_cuotas$dias),sacar3,type='l',col='yellow')

ggplot2::ggplot(data=data.frame())