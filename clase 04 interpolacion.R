DosLocales <- read_csv("~/Google Drive/Maestria/DM series temporales/Materia_Series_Temporales_UBA_2017/DosLocales.csv")

# aumentar la frecuencia de muestreo

N = length(DosLocales$localA)
print (paste("N =",N))
localAfal = approx(DosLocales$t,DosLocales$localA,n=3*N) #interpolacion lineal
localAfas = spline(DosLocales$t,DosLocales$localA,n=3*N) #interpolacion cubica


plot(DosLocales$t,DosLocales$localA,type = 'l',main='Ventas Locales A',xlab='dias',ylab='ventas k$' , xlim=c(45,55),ylim=c(8,17))
points(DosLocales$t,DosLocales$localA,pch=20,col='black')
for(j in 1:N) {
  abline (v=j, lty =2) }
lines ( localAfal$x, localAfal$y,col='red')
points( localAfal$x, localAfal$y,pch=20,col='red')
lines ( localAfas$x, localAfas$y, col='blue') 
points(localAfas$x,localAfas$y,pch=20,col='green')
