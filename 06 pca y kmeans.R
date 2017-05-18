#pca con agrupacion_cuotas

#probar agrupaciones

library(FactoMineR)
library(dplyr)

#agrup_cuotas_id es una manera de agrupar por id los valores de las cuotas agregando campos propios del credito
creditos_pca <- prcomp(agrup_cuotas_id[,2:31],scale = TRUE)

plot(creditos_pca,type='l')
summary(creditos_pca)

biplot(creditos_pca)

comp <- creditos_pca$x[,1:5]

# Determine number of clusters
wss <- (nrow(comp)-1)*sum(apply(comp,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(comp,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


# From scree plot elbow occurs at k = 5
# Apply k-means with k=5
k <- kmeans(comp, 5, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)

creditos_cuotas <- creditos %>% filter(id_ficticio %in% agrup_cuotas_id$id_ficticio)
creditos_cuotas$cluster <- k$cluster

todos_cruces <- todos_cruces %>% filter(id_ficticio %in% creditos_cuotas$id_ficticio)

todos_cruces$cluster <- NULL

todos_cruces <- todos_cruces %>% left_join(creditos_cuotas[,c(1,19)],by="id_ficticio")

library(data.table)

agrupada_nueva_cuotas <- setDT(todos_cruces)[, .(Sum = sum(target), Count = .N), by = c('dias','cluster')]

agrupada_nueva_cuotas$tasa_acierto <- agrupada_nueva_cuotas$Sum/agrupada_nueva_cuotas$Count

fft.c1 <- fft(agrupada_nueva_cuotas$tasa_acierto[agrupada_nueva_cuotas$cluster==1])
fft.c2 <- fft(agrupada_nueva_cuotas$tasa_acierto[agrupada_nueva_cuotas$cluster==2])
fft.c3 <- fft(agrupada_nueva_cuotas$tasa_acierto[agrupada_nueva_cuotas$cluster==3])
fft.c4 <- fft(agrupada_nueva_cuotas$tasa_acierto[agrupada_nueva_cuotas$cluster==4])
fft.c5 <- fft(agrupada_nueva_cuotas$tasa_acierto[agrupada_nueva_cuotas$cluster==5])

with(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==1,],plot(tasa_acierto ~ dias,type='l'))
with(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==2,],lines(tasa_acierto ~ dias,type='l',col='red'))
with(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==3,],lines(tasa_acierto ~ dias,type='l',col='green'))
with(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==4,],lines(tasa_acierto ~ dias,type='l',col='blue'))
with(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==5,],lines(tasa_acierto ~ dias,type='l',col='grey'))

plot(Mod(fft.c2),type="l")
lines(Mod(fft.c1),type = 'l',col='red')
lines(Mod(fft.c3),type = 'l',col='green')
lines(Mod(fft.c4),type = 'l',col='blue')
lines(Mod(fft.c5),type = 'l',col='grey')


#media movil filtros antes de hacer fft para suavizar la serie # Tue May 16 19:34:25 2017 ------------------------------

agrupada1.MA3 =  filter(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==1,5], rep(1/3,3) , circular =TRUE)

with(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==1,],plot(tasa_acierto ~ dias,type='l'))
lines(unique(agrupada_nueva_cuotas$dias),agrupada1.MA3,col='red')

fft.c1 <- fft(agrupada1.MA3)
plot(Mod(fft.c1),type="l")

#grupo 2
agrupada2.MA3 =  filter(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==2,5], rep(1/3,3) , circular =TRUE)

with(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==2,],plot(tasa_acierto ~ dias,type='l'))
lines(unique(agrupada_nueva_cuotas$dias),agrupada2.MA3,col='red')

fft.c2 <- fft(agrupada2.MA3)
plot(Mod(fft.c2),type="l")

#grupo 3

agrupada3.MA3 =  filter(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==3,5], rep(1/3,3) , circular =TRUE)

with(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==3,],plot(tasa_acierto ~ dias,type='l'))
lines(unique(agrupada_nueva_cuotas$dias),agrupada3.MA3,col='red')

fft.c3 <- fft(agrupada3.MA3)
plot(Mod(fft.c3),type="l")

#grupo 4

agrupada4.MA3 =  filter(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==4,5], rep(1/3,3) , circular =TRUE)

with(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==4,],plot(tasa_acierto ~ dias,type='l'))
lines(unique(agrupada_nueva_cuotas$dias),agrupada4.MA3,col='red')

fft.c4 <- fft(agrupada4.MA3)
plot(Mod(fft.c4),type="l")


#grupo 5

agrupada5.MA3 =  filter(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==5,5], rep(1/3,3) , circular =TRUE)

with(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==5,],plot(tasa_acierto ~ dias,type='l'))
lines(unique(agrupada_nueva_cuotas$dias),agrupada5.MA3,col='red')

fft.c5 <- fft(agrupada5.MA3)
plot(Mod(fft.c5),type="l")


# remover la componente de frecuencia cero (DC)
fft.c1[1] = 0 
plot(Mod(fft.c1),type='l')

#filtros
filtroDF = rep(1,length(fft.c1))
filtroDF [c(1:50,200:(length(fft.c1)))] = 0
filtroDF [(length(fft.c1)-200):(length(fft.c1)-50)] = 0

fft.c1.tasaDF = filtroDF*fft.c1

op = par(mfrow = c(1, 2))


N.1= length(fft.c1)

tiempo1= 0:(N.1-1)

plot(tiempo1, filtroDF , type="l") #pasa altos

plot(Mod(fft.c1.tasaDF),type="l")

fft.c1.inv =  Re(fft(fft.c1.tasaDF,inverse=TRUE)/N.1)
op = par(mfrow = c(1, 1))

plot(unique(agrupada_nueva_cuotas$dias),fft.c1.inv,type="l") 
with(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==1,],lines(tasa_acierto ~ dias,type='l'))

with(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==1,],plot(tasa_acierto ~ dias,type='l'))


# remover la componente de frecuencia cero (DC)
fft.c2[1] = 0 
plot(Mod(fft.c2),type='l')

#filtros
filtroDF = rep(1,length(fft.c2))
filtroDF [c(1:50,200:(length(fft.c1)))] = 0
filtroDF [(length(fft.c2)-200):(length(fft.c2)-50)] = 0

fft.c2.tasaDF = filtroDF*fft.c2

op = par(mfrow = c(1, 2))


N.1= length(fft.c2)

tiempo1= 0:(N.1-1)

plot(tiempo1, filtroDF , type="l") #pasa altos

plot(Mod(fft.c2.tasaDF),type="l")

fft.c2.inv =  Re(fft(fft.c2.tasaDF,inverse=TRUE)/N.1)
op = par(mfrow = c(1, 1))

plot(unique(agrupada_nueva_cuotas$dias),fft.c2.inv,type="l") 
with(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==2,],lines(tasa_acierto ~ dias,type='l'))

with(agrupada_nueva_cuotas[agrupada_nueva_cuotas$cluster==2,],plot(tasa_acierto ~ dias,type='l'))
