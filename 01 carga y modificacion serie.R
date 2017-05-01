#generar secuencia de dias por mes

library(dplyr)
library(data.table)
library(zoo)
library(sqldf)

dias <- unique(series_temp_dataset$FECH.ACRE)
dias <-  as.data.frame(dias)
dias$mes <- months(dias$dias)

dias <- dias %>% arrange(dias)

DT.series <- data.table(dias)
DT.series <- DT.series[, N := sequence(.N), by = rleid(mes)][]


DT.series$seq <- NULL

series_temp_dataset <- series_temp_dataset %>% left_join(DT.series,by=c("FECH.ACRE"="dias"))

plot(series_temp_dataset$target ~ series_temp_dataset$N,type='l')

sumatorio.dias <- aggregate(series_temp_dataset$target,FUN=sum,by=list(series_temp_dataset$N))

plot(sumatorio.dias$x ~ sumatorio.dias$Group.1,type='l')

sumatorio.cliente <- aggregate(series_temp_dataset$target,FUN=sum,by=list(series_temp_dataset$id_ficticio,series_temp_dataset$N))
colnames(sumatorio.cliente) <- c("cliente","dia","cantidad")

#prueba de agregar todos 0

clientes <- series_temp_dataset %>% distinct(id_ficticio)


todos_cruces <- sqldf("select * from clientes cross join dias")

todos_cruces$seq <- NULL

todos_cruces <- todos_cruces %>% left_join(DT.series,by="dias")

todos_cruces <- todos_cruces %>% left_join(series_temp_dataset[,c(1:3)],by=c("id_ficticio"="id_ficticio","dias"="FECH.ACRE"))

saveRDS(todos_cruces,"clientes_todas_fechas.rds")
todos_cruces$mes.y <- NULL
write.table(todos_cruces,"clientes_todas_fechas.txt",row.names = FALSE)

todos_cruces$target<- ifelse(is.na(todos_cruces$target)==TRUE,0,todos_cruces$target)
