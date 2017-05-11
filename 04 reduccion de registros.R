creditos <- readRDS("base_series.rds")

library(dplyr)

serie_reducida <- series_temp_dataset %>% filter(id_ficticio %in% creditos$id_ficticio)

#probar agrupaciones

library(FactoMineR)

creditos <- creditos %>% mutate(mes_otor=months(Fecha_Otorgado))

creditos_pca <- prcomp(creditos[,c(2:6,8,15)],scale = TRUE)

plot(creditos_pca,type='l')
summary(creditos_pca)

biplot(creditos_pca)

comp <- creditos_pca$x[,1:3]

# Determine number of clusters
wss <- (nrow(comp)-1)*sum(apply(comp,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(comp,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


# From scree plot elbow occurs at k = 4
# Apply k-means with k=4
k <- kmeans(comp, 3, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)


creditos$cluster <- k$cluster

#se agrega al dataset

serie_reducida <- serie_reducida %>% left_join(creditos[,c(1,19)],by="id_ficticio")


#prueba de agregar todos 0

clientes <- serie_reducida %>% distinct(id_ficticio)
library(sqldf)

todos_cruces <- sqldf("select * from clientes cross join dias")

todos_cruces$seq <- NULL

todos_cruces <- todos_cruces %>% left_join(DT.series,by="dias")

todos_cruces <- todos_cruces %>% left_join(serie_reducida[,c(1:3)],by=c("id_ficticio"="id_ficticio","dias"="FECH.ACRE"))

saveRDS(todos_cruces,"clientes_todas_fechas_nuevo.rds")
todos_cruces$mes.y <- NULL
write.table(todos_cruces,"clientes_todas_fechas.txt",row.names = FALSE)

todos_cruces$target<- ifelse(is.na(todos_cruces$target)==TRUE,0,todos_cruces$target)

todos_cruces <- todos_cruces %>% left_join(creditos[,c(1,19)],by="id_ficticio")


sumatorio.cliente.anual <- aggregate(todos_cruces$target,FUN=sum,by=list(todos_cruces$dias))
colnames(sumatorio.cliente.anual) <- c("dia","cantidad")


library(data.table)

agrupada_nueva <- setDT(todos_cruces)[, .(Sum = sum(target), Count = .N), by = c('dias','cluster')]

agrupada_nueva$tasa_acierto <- agrupada_nueva$Sum/agrupada_nueva$Count


fft.c1 <- fft(agrupada_nueva$tasa_acierto[agrupada_nueva$cluster==1])
fft.c2 <- fft(agrupada_nueva$tasa_acierto[agrupada_nueva$cluster==2])
fft.c3 <- fft(agrupada_nueva$tasa_acierto[agrupada_nueva$cluster==3])
"fft.c4 <- fft(agrupada_nueva$tasa_acierto[agrupada_nueva$cluster==4])
fft.c5 <- fft(agrupada_nueva$tasa_acierto[agrupada_nueva$cluster==5])
fft.c6 <- fft(agrupada_nueva$tasa_acierto[agrupada_nueva$cluster==6])
"

with(agrupada_nueva[agrupada_nueva$cluster==1,],plot(tasa_acierto ~ dias,type='l'))
with(agrupada_nueva[agrupada_nueva$cluster==2,],lines(tasa_acierto ~ dias,type='l',col='red'))
with(agrupada_nueva[agrupada_nueva$cluster==3,],lines(tasa_acierto ~ dias,type='l',col='green'))
#with(agrupada_nueva[agrupada_nueva$cluster==4,],lines(tasa_acierto ~ dias,type='l',col='blue'))
#with(agrupada_nueva[agrupada_nueva$cluster==5,],lines(tasa_acierto ~ dias,type='l',col='grey'))
#with(agrupada_nueva[agrupada_nueva$cluster==6,],lines(tasa_acierto ~ dias,type='l',col='yellow'))


plot(Mod(fft.c1),type="l")
lines(Mod(fft.c2),type = 'l',col='red')
lines(Mod(fft.c3),type = 'l',col='green')


