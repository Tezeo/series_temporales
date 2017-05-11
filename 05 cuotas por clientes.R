#base para clustering

library(dplyr)
cuotas_cobradas <- readRDS("/Users/pablotempone/OneDrive/Series de tiempo/cuotas_para_cluster.rds")

cuotas_cobradas <- cuotas_cobradas %>% filter(id_ficticio %in% creditos$id_ficticio) %>% filter(Nro_Cuota<=48)

cuotas_cobradas$Fecha_Vencimiento <- as.Date(cuotas_cobradas$Fecha_Vencimiento)

cuotas_cobradas$Fecha_Cobro <- as.Date(cuotas_cobradas$Fecha_Cobro)


cuotas_cobradas$dif_fecha <- difftime(cuotas_cobradas$Fecha_Cobro,cuotas_cobradas$Fecha_Vencimiento,units = "days")

max_atraso <- cuotas_cobradas %>% group_by(id_ficticio) %>% summarise(max_dias_atraso=max(dif_fecha))

cuotas_cobradas$mes_cobro <- months(cuotas_cobradas$Fecha_Cobro)
cuotas_cobradas$mes_venc <- months(cuotas_cobradas$Fecha_Vencimiento)

cuotas_cobradas <- cuotas_cobradas %>% left_join(creditos,by="id_ficticio")

cuotas_cobradas$