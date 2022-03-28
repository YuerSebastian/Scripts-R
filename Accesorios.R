library(dplyr)

acce <- readr::read_delim("D:/Cosa/Accesorios_Paquete_Venta.txt",delim = "|")
acce <- select(acce, -TZFACCE_FLAG)
acce$MATRICULA <- as.character(acce$MATRICULA)
acce$MATRICULA2 <- acce$MATRICULA
acce$PROGRAMA2 <- acce$PROGRAMA
acce <- tidyr::unite(acce,MATPROG,MATRICULA2,PROGRAMA2,sep = "")
acce <- acce %>% group_by(MATPROG) %>% mutate(cont = row_number()) %>% filter(cont==1) %>% select(-cont)

#Agregando coñumnas con accesorios
acce$Titulo_intermedio <- if_else(grepl("(TIT|TITULO) INT",acce$PAQUETE_VENTA),3000,0)
acce$Doble_reconocimiento <- if_else(grepl("DOBLE REC",acce$PAQUETE_VENTA),15000,0)
acce$Apostilla <- if_else(grepl("APOSTILLA",acce$PAQUETE_VENTA),3000,0)
acce$Colegiatura_final <- if_else(grepl("COLEGIATURA FIN",acce$PAQUETE_VENTA),15000,0)

acce$Total <- acce$Titulo_intermedio+acce$Doble_reconocimiento+acce$Apostilla+acce$Colegiatura_final

write.csv(acce,"D:/Cosa/Accesorios/Accesorios_Paquete_Venta.csv",row.names = F)




desc <- list.files ("D:/Cosa/Actas/Descargas", full.names = TRUE)

for (i in 1:length(desc)) {
  Aux <- read.csv(desc[i])
  if(i==1) actas <- Aux else actas <- rbind(actas,Aux)
}



write.csv(actas,"D:/Cosa/Actas/actas.csv",row.names = F)













