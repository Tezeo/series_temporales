dir = '/..../' # directorio del csv

dato=read.csv('EstudioEdadIngresos2.csv')
print(names(dato))
attach(dato)

covar.ActitudCompra.Edad=lm(ActitudCompraPre~Edad+Grupo)
print(summary(covar.ActitudCompra.Edad))
plot(Edad[Grupo=="A"],ActitudCompra[Grupo=="A"],
     xlim=c(20,70),ylim=c(min(ActitudCompra)-0.5,max(ActitudCompra)+0.5),
     main="",pch='A',
     xlab="Edad",ylab="Actitud de Compra")
abline(covar.ActitudCompra.Edad$coefficients[1],covar.ActitudCompra.Edad$coefficients[2])
points(dato$Edad[dato$Grupo=="B"],dato$ActitudCompra[dato$Grupo=="B"],pch='B')
abline(covar.ActitudCompra.Edad$coefficients[1]+covar.ActitudCompra.Edad$coefficients[3],
       covar.ActitudCompra.Edad$coefficients[2],lty="dashed")

print(confint(covar.ActitudCompra.Edad))
