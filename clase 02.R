tiempo <- seq(0,10,0.1)

N <- length(tiempo)

x <- rnorm(N,2,0.5)

mean(x)

sd(x)

plot(tiempo,x,type = 'l')

abline(h=mean(x),lty=2)

abline(h=mean(x)+2*sd(x),lty=3)
abline(h=mean(x)-2*sd(x),lty=3)


boxplot(x)

hist(x)

abline(v=mean(x),lty=2)

abline(v=mean(x)+2*sd(x),lty=3)
abline(v=mean(x)-2*sd(x),lty=3)


hx <- hist(x)

hist(x)

abline(v=mean(x),lty=2)

abline(v=mean(x)+2*sd(x),lty=3)
abline(v=mean(x)-2*sd(x),lty=3)


x.ts <- ts(rnorm(N,mean = 2,0.5),start = 0,10)

frequency(x.ts)

deltat(x.ts)

x.ts
time(x.ts)


#senoidales

tiempo <- seq(0,255,1)


ciclos <- 1

amplitud <- 1

seno1 <- amplitud*sin(ciclos*2*pi*tiempo/N)

plot(tiempo,seno1,'l')


ciclos <- 2

amplitud <- 1

seno2 <- amplitud*sin(ciclos*2*pi*tiempo/N)

plot(tiempo,seno2,'l')

ciclos <- 10

amplitud <- 2

seno10 <- amplitud*sin(ciclos*2*pi*tiempo/N)

plot(tiempo,seno10,'l')

ciclos <- 20

amplitud <- 0.5

seno20 <- amplitud*sin(ciclos*2*pi*tiempo/N)

plot(tiempo,seno20,'l')

niveldc <- rep(1,256)

plot(tiempo,niveldc,'l')

seno121020dc <- seno1+seno10+seno2+seno20+niveldc

plot(tiempo,seno121020dc,'l')

#transformada de fourier (rapida)

fft.seno1 <- fft(seno1)

plot(Mod(fft.seno1),type = 'l') #se muestra solo el modulo y no la fase(argumento), la fase es dificil de explicar

#como no existen posiciones de vectores negativos los representa en lado opuesto del grafico
#se usa tiempo 256 porque es 2 a la n, para aprovechar la redudancia.

seno1a <- Re(fft(fft.seno1,inverse = TRUE)/N) #tiene que estar la inversa para reconstruirla

plot(tiempo,seno1a,type = 'l')

fft.seno2 <- fft(seno2)

plot(Mod(fft.seno2),type = 'l') #se muestra solo el modulo y no la fase(argumento), la fase es dificil de explicar

fft.seno10 <- fft(seno10)

plot(Mod(fft.seno10),type = 'l') #se muestra solo el modulo y no la fase(argumento), la fase es dificil de explicar

fft.seno20 <- fft(seno20)

plot(Mod(fft.seno20),type = 'l') #se muestra solo el modulo y no la fase(argumento), la fase es dificil de explicar

fft.niveldc <- fft(niveldc)

plot(Mod(fft.niveldc),type = 'l') #se muestra solo el modulo y no la fase(argumento), la fase es dificil de explicar

fft.seno121020dc <- fft(seno121020dc)

plot(Mod(fft.seno121020dc),type = 'l') #prueba la propiedad de la transformada de fourier

#remover la componente de frecuencia cero (DC)

fft.seno121020dc[1] <- 0 

plot(Mod(fft.seno121020dc),type = 'l')

seno121020dc2 <- fft(fft.seno121020dc,inverse = TRUE)/N

plot(tiempo,Re(seno121020dc2),'l')

plot(tiempo,seno121020dc,'l',ylim = c(-4,5))

lines(tiempo,seno121020dc2)

abline(h=1,lty=2)
abline(h=0,lty=2)


#generacion de pulsos

p1 <- rep(0,N)
p1[1] <- 1

plot(tiempo,p1,type = 'l')

p2 <- rep(0,N)
p2[1:2] <- 1

plot(tiempo,p2,type = 'l')

p5 <- rep(0,N)
p5[1:5] <- 1

plot(tiempo,p5,type = 'l')

p10 <- rep(0,N)
p10[1:10] <- 1

plot(tiempo,p10,type = 'l')

p20 <- rep(0,N)
p20[1:20] <- 1

plot(tiempo,p20,type = 'l')

p50 <- rep(0,N)
p50[1:50] <- 1

plot(tiempo,p50,type = 'l')


#se calcula la transf de fourier

fft.p1 <- fft(p1)
plot(Mod(fft.p1),type = 'l')

fft.p2 <- fft(p2)
plot(Mod(fft.p2),type = 'l')

fft.p5 <- fft(p5)
plot(Mod(fft.p5),type = 'l')

fft.p10 <- fft(p10)
plot(Mod(fft.p10),type = 'l')

fft.p20 <- fft(p20)
plot(Mod(fft.p20),type = 'l')

fft.p50 <- fft(p50)
plot(Mod(fft.p50),type = 'l')

plot(Mod(fft.p2),type = 'l',ylim = c(0,50),xlim = c(0,64)) #se muestran las primera 64 componentes, la mitad de las 128

lines(Mod(fft.p5),type = 'l')
lines(Mod(fft.p10),type = 'l')
lines(Mod(fft.p20),type = 'l')
lines(Mod(fft.p50),type = 'l')

#se plotean normalizados
plot(Mod(fft.p2)/max(Mod(fft.p2)),type = 'l',ylim = c(0,1),xlim = c(0,64))

lines(Mod(fft.p5)/max(Mod(fft.p5)),type = 'l')
lines(Mod(fft.p10)/max(Mod(fft.p10)),type = 'l')
lines(Mod(fft.p20)/max(Mod(fft.p20)),type = 'l')
lines(Mod(fft.p50)/max(Mod(fft.p50)),type = 'l')
abline(h=0.7,lty=2) #70% valor de referencia que se usa para decir que hasta esa frecuencia

#fft de pulsos con eje de frecuencias calibrado

DeltaT <- 0.001 #segundos (intervalo de muestreo)
frecuenciamuestreo <- 1/DeltaT #Hertz (inversa de la frecuencia)
Deltafrecmuestreo <- frecuenciamuestreo/N
frecuencia <- Deltafrecmuestreo*tiempo

plot(frecuencia,Mod(fft.p2)/max(Mod(fft.p2)),type = 'l',ylim = c(0,1),xlim = c(0,250),ylab = 'Mod fft p2 a p50 (normalizado)',xlab = 'frecuencia (Hz)')
lines(frecuencia,Mod(fft.p5)/max(Mod(fft.p5)),type = 'l')
lines(frecuencia,Mod(fft.p10)/max(Mod(fft.p10)),type = 'l')
lines(frecuencia,Mod(fft.p20)/max(Mod(fft.p20)),type = 'l')
lines(frecuencia,Mod(fft.p50)/max(Mod(fft.p50)),type = 'l')
abline(h=0.7,lty=2) #70% valor de referencia que se usa para decir que hasta esa frecuencia


#teorema de parseval

s2 <- sin(2*2*pi*tiempo/N)

e.dt <- sum(s2*s2)

paste('Energia DT', e.dt)

fft.s2 <- fft(s2)

e.df <- sum(Mod(fft.s2)*Mod(fft.s2))/N

paste('Energia DF', e.dt)
