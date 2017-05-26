
library(dplyr)
fft_serie <- Mod(fft(mv15))

energia_t <- sum(mv15*mv15)

energia_fft <- sum(fft_serie*fft_serie)/length(fft_serie)

fft_serie_ord <-  sort(fft_serie,decreasing = TRUE)

head(fft_serie_ord,10)

valores <- unique(head(fft_serie_ord,10))

energia_acumulada <- sum(valores * valores) / length(fft_serie)

energia_acumulada/energia_t
#tomar tanto valores hasta llegar al aprox 85% de explicacion. Y ahi se corta, criterio de parseval

fft_serie <- Mod(fft(d))
energia_t <- sum(d * d)
energia_fft <- sum(fft_serie * fft_serie) / length(fft_serie)

cat(sprintf('Energía en tiempo: %f. Energía FFT: %f.\n', energia_t, energia_fft))

mid <- ceiling((length(fft_serie))/2)
fft_serie <- c(fft_serie[(mid+1):length(fft_serie)],
               fft_serie[1:mid])

valores_maximos <- sort(fft_serie, decreasing = T)[1:(rm_max*2-1)]

energia_acumulada <- sum(valores_maximos * valores_maximos) / length(fft_serie)

cat(sprintf('PARSEVALL: %.2f%%\n', energia_acumulada / energia_fft * 100))


with(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==1,],plot(tasa_acierto ~ dias,type='l'))


agrupada_ts.246 <- ts(agrupada$tasa_aciertos)

mv30 <- SMA(agrupada_ts.246,n=30)
mv30 =  stats::filter(ts(agrupada[,3]), rep(1/20,20) , circular =TRUE)

mv15 <- stats::filter(ts(agrupada[,3]), rep(1/10,10) , circular =TRUE)
plot(mv30)
plot(ts(agrupada[,3]))
plot(mv15)


resta15 <- agrupada_ts.246 - mv15

fft.resta <- fft(resta15)

plot(Mod(fft.resta))


fft_serie <-(fft(resta15))

energia_t <- sum(resta15*resta15)

energia_fft <- sum(fft_serie*fft_serie)/length(fft_serie)

fft_serie_ord <-  sort(fft_serie,decreasing = TRUE)

head(fft_serie_ord,10)

valores <- unique(head(fft_serie_ord,100))

energia_acumulada <- sum(valores * valores) / length(fft_serie)

energia_acumulada/energia_t


fft_serie <- (fft(mv15))

plot(Mod(fft_serie))
energia_t <- sum(mv15*mv15)

energia_fft <- sum(fft_serie*fft_serie)/length(fft_serie)

fft_serie_ord <-  sort(Mod(fft_serie),decreasing = TRUE)

head(Mod(fft_serie_ord),10)

valores <- unique(head(Mod(fft_serie_ord),10))

energia_acumulada <- sum(valores * valores) / length(fft_serie)

energia_acumulada/energia_fft

#filtros
filtroDF
filtroDF = rep(1,length(fft_serie))

filtroDF [1:10] = 0
filtroDF [(length(fft_serie)-10):length(fft_serie)] = 0

fft.test = filtroDF*fft_serie

op = par(mfrow = c(1, 2))


N.1= length(fft_serie)

tiempo1= 0:(N.1-1)

plot(tiempo1, filtroDF , type="l") #pasa altos

plot(fft_serie,type="l")
media_sumarla = rep(mean(fft_serie),length(fft_serie))
plot(Mod(fft_serie)/N.1)
testDF.inv =  Re(fft(fft.test,inverse=TRUE)/N.1)

head(Mod(fft_serie))
head((fft_serie))
media_lean=Mod(fft_serie[1])/N.1
media_sumarla = rep(media_lean,length(fft_serie))


testDF.inv=testDF.inv+media_sumarla
op = par(mfrow = c(1, 1))


plot(agrupada$fecha,testDF.inv,type="l") 
lines(agrupada$fecha,mv15,col="red",type = 'l')

testDF.inv

plot(tiempo1,as.ts(mv15),type = 'l')
