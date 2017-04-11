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

plot(Mod(fft.seno121020dc),type = 'l') #se muestra solo el modulo y no la fase(argumento), la fase es dificil de explicar
