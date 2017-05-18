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

cuotas_abril <- cuotas_cobradas %>% filter((Fecha_Cobro<'2016-05-01' | is.na(Fecha_Cobro)==TRUE) & Fecha_Vencimiento<'2016-05-01')
cuotas_abril <- cuotas_abril %>% group_by(id_ficticio) %>% summarise(max_cuota=max(Nro_Cuota),max_atraso=max(dif_fecha),min_atraso=min(dif_fecha),
                                                                     sum_cap_cob=sum(Capital_Cobrado),sum_int_cob=sum(Interes_Cobrado),sum_iva_cob=sum(IVA_Cobrado),
                                                                     min_cap_cob=min(Capital_Cobrado),min_int_cob=min(Interes_Cobrado),min_iva_cob=min(IVA_Cobrado),
                                                                     max_cap_cob=max(Capital_Cobrado),max_int_cob=max(Interes_Cobrado),max_iva_cob=max(IVA_Cobrado),
                                                                     sum_tot_cob=sum(TotalCuota_Cobrado),min_tot_cob=min(TotalCuota_Cobrado),max_tot_cob=max(TotalCuota_Cobrado),
                                                                     sum_pun_cob=sum(Punitorios_Cobrado),min_pun_cob=min(Punitorios_Cobrado),max_tot_cob=max(Punitorios_Cobrado)) %>% mutate(mes='abril')

cuotas_mayo <- cuotas_cobradas %>% filter((Fecha_Cobro<'2016-06-01' | is.na(Fecha_Cobro)==TRUE) & Fecha_Vencimiento<'2016-06-01')

cuotas_mayo <- cuotas_mayo %>% group_by(id_ficticio) %>% summarise(max_cuota=max(Nro_Cuota),max_atraso=max(dif_fecha),min_atraso=min(dif_fecha),
                                                                     sum_cap_cob=sum(Capital_Cobrado),sum_int_cob=sum(Interes_Cobrado),sum_iva_cob=sum(IVA_Cobrado),
                                                                     min_cap_cob=min(Capital_Cobrado),min_int_cob=min(Interes_Cobrado),min_iva_cob=min(IVA_Cobrado),
                                                                     max_cap_cob=max(Capital_Cobrado),max_int_cob=max(Interes_Cobrado),max_iva_cob=max(IVA_Cobrado),
                                                                     sum_tot_cob=sum(TotalCuota_Cobrado),min_tot_cob=min(TotalCuota_Cobrado),max_tot_cob=max(TotalCuota_Cobrado),
                                                                     sum_pun_cob=sum(Punitorios_Cobrado),min_pun_cob=min(Punitorios_Cobrado),max_tot_cob=max(Punitorios_Cobrado)) %>% mutate(mes='mayo')

cuotas_junio <- cuotas_cobradas %>% filter((Fecha_Cobro<'2016-07-01' | is.na(Fecha_Cobro)==TRUE) & Fecha_Vencimiento<'2016-07-01')

cuotas_junio <- cuotas_junio %>% group_by(id_ficticio) %>% summarise(max_cuota=max(Nro_Cuota),max_atraso=max(dif_fecha),min_atraso=min(dif_fecha),
                                                                   sum_cap_cob=sum(Capital_Cobrado),sum_int_cob=sum(Interes_Cobrado),sum_iva_cob=sum(IVA_Cobrado),
                                                                   min_cap_cob=min(Capital_Cobrado),min_int_cob=min(Interes_Cobrado),min_iva_cob=min(IVA_Cobrado),
                                                                   max_cap_cob=max(Capital_Cobrado),max_int_cob=max(Interes_Cobrado),max_iva_cob=max(IVA_Cobrado),
                                                                   sum_tot_cob=sum(TotalCuota_Cobrado),min_tot_cob=min(TotalCuota_Cobrado),max_tot_cob=max(TotalCuota_Cobrado),
                                                                   sum_pun_cob=sum(Punitorios_Cobrado),min_pun_cob=min(Punitorios_Cobrado),max_tot_cob=max(Punitorios_Cobrado)) %>% mutate(mes='junio')

cuotas_julio <- cuotas_cobradas %>% filter((Fecha_Cobro<'2016-08-01' | is.na(Fecha_Cobro)==TRUE) & Fecha_Vencimiento<'2016-08-01')

cuotas_julio <- cuotas_julio %>% group_by(id_ficticio) %>% summarise(max_cuota=max(Nro_Cuota),max_atraso=max(dif_fecha),min_atraso=min(dif_fecha),
                                                                   sum_cap_cob=sum(Capital_Cobrado),sum_int_cob=sum(Interes_Cobrado),sum_iva_cob=sum(IVA_Cobrado),
                                                                   min_cap_cob=min(Capital_Cobrado),min_int_cob=min(Interes_Cobrado),min_iva_cob=min(IVA_Cobrado),
                                                                   max_cap_cob=max(Capital_Cobrado),max_int_cob=max(Interes_Cobrado),max_iva_cob=max(IVA_Cobrado),
                                                                   sum_tot_cob=sum(TotalCuota_Cobrado),min_tot_cob=min(TotalCuota_Cobrado),max_tot_cob=max(TotalCuota_Cobrado),
                                                                   sum_pun_cob=sum(Punitorios_Cobrado),min_pun_cob=min(Punitorios_Cobrado),max_tot_cob=max(Punitorios_Cobrado)) %>% mutate(mes='julio')

cuotas_agosto <- cuotas_cobradas %>% filter((Fecha_Cobro<'2016-09-01' | is.na(Fecha_Cobro)==TRUE) & Fecha_Vencimiento<'2016-09-01')

cuotas_agosto <- cuotas_agosto %>% group_by(id_ficticio) %>% summarise(max_cuota=max(Nro_Cuota),max_atraso=max(dif_fecha),min_atraso=min(dif_fecha),
                                                                     sum_cap_cob=sum(Capital_Cobrado),sum_int_cob=sum(Interes_Cobrado),sum_iva_cob=sum(IVA_Cobrado),
                                                                     min_cap_cob=min(Capital_Cobrado),min_int_cob=min(Interes_Cobrado),min_iva_cob=min(IVA_Cobrado),
                                                                     max_cap_cob=max(Capital_Cobrado),max_int_cob=max(Interes_Cobrado),max_iva_cob=max(IVA_Cobrado),
                                                                     sum_tot_cob=sum(TotalCuota_Cobrado),min_tot_cob=min(TotalCuota_Cobrado),max_tot_cob=max(TotalCuota_Cobrado),
                                                                     sum_pun_cob=sum(Punitorios_Cobrado),min_pun_cob=min(Punitorios_Cobrado),max_tot_cob=max(Punitorios_Cobrado)) %>% mutate(mes='agosto')

#idea para agrupar clientes # Sat May 13 15:39:27 2017 ------------------------------

cuotas_cobradas$dia_cobro <- as.integer(substr(cuotas_cobradas$Fecha_Cobro,9,10))

agrup_cuotas_id <- cuotas_cobradas %>% group_by(id_ficticio,Plazo,Tasa,ValorCuota) %>% summarise(max_cuota=as.integer(max(Nro_Cuota)),max_atraso=as.numeric(max(dif_fecha)),min_atraso=as.numeric(min(dif_fecha)),mean_atraso=as.numeric(mean(dif_fecha)),
                                                                       sum_cap_cob=sum(Capital_Cobrado),sum_int_cob=sum(Interes_Cobrado),sum_iva_cob=sum(IVA_Cobrado),
                                                                       min_cap_cob=min(Capital_Cobrado),min_int_cob=min(Interes_Cobrado),min_iva_cob=min(IVA_Cobrado),
                                                                       max_cap_cob=max(Capital_Cobrado),max_int_cob=max(Interes_Cobrado),max_iva_cob=max(IVA_Cobrado),
                                                                       mean_cap_cob=mean(Capital_Cobrado),mean_int_cob=mean(Interes_Cobrado),mean_iva_cob=mean(IVA_Cobrado),
                                                                       sum_tot_cob=sum(TotalCuota_Cobrado),min_tot_cob=min(TotalCuota_Cobrado),max_tot_cob=max(TotalCuota_Cobrado),mean_tot_cob=mean(TotalCuota_Cobrado),
                                                                       sum_pun_cob=sum(Punitorios_Cobrado),min_pun_cob=min(Punitorios_Cobrado),max_pun_cob=max(Punitorios_Cobrado),mean_pun_cob=mean(Punitorios_Cobrado),
                                                                       max_dia=max(dia_cobro),min_dia=min(dia_cobro),mean_dia=mean(dia_cobro))
