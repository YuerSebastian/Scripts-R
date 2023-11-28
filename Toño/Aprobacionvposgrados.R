library(readxl)                      
library(dplyr)                              
library(lubridate)                           
library(stringi)                             
library(tidyr)  
library(stringr)

# sabana <- read.csv("D://Conectivida/Sabanas May-Jun21/Sabana_2021-05-04.csv")
# sabana$fecha_hora_extraccion <- "2021-05-03"
# write.csv(sabana,file="D://Conectivida/Sabanas May-Jun21/Sabana_2021-05-03.csv",row.names = F)


### Aprobacion####

mayo <- list.files ("//Tut-095/Aprobacion_Posgrados/Actual/", pattern = "*.csv", full.names = TRUE)

may <- data.frame(mayo)
a <- 1:length(mayo)
a <- data.frame(a)
may <- cbind(mayo,a)
may$mayo <- gsub("//Tut-095/Aprobacion_Posgrados/Actual/","",may$mayo)
may$mayo <- gsub("Sabana_POSGRADOS_","",may$mayo)
may$mayo <- gsub(".csv","",may$mayo)



for (i in 1:length(mayo)) {
  csv <-read.csv(mayo[i], header = TRUE,sep = ",",encoding="UTF-7",stringsAsFactors = FALSE)
  csv <- subset(csv,Status.Materia == "Activo")
  csv <- csv %>% filter((grepl('MATRICULADO', .$Estado.Banner)
                          | grepl('ADMITIDO', .$Estado.Banner)))
  csv$Inicio.de.curso <- ymd(csv$Inicio.de.curso)
  csv <- csv %>%
    filter(Inicio.de.curso == as.Date("2022-05-02"),Duración == "Bimestral")
  csv$a <- i
  csv <- select(csv,Matricula,Bloque.seguimiento,Clave,Asignatura,Inicio.de.curso,NR,a)
  csv <- mutate(csv,verificar = grepl("IEBSM|ONUM|SEM|UBAM|SEL|IEBSL|UNICL|COUMA|SSENM01|M1HB",Clave)) %>% filter(verificar == F)
  csv$verificar <- NULL
  if(i==1) bimmayo <- csv else bimmayo <- rbind(bimmayo,csv)
}


names(may)[1]="Fecha_creacion"

bimmayo <- left_join(bimmayo,may,by="a")


#bimmayo <- mutate_at(bimmayo,c("Tipo_Materia"),~replace(.,is.na(.),"NORMAL"))



bimmayo <- mutate(bimmayo,Fecha_creacion = ymd(Fecha_creacion))

semanas <- read_excel("D:/Aprobacion_Posgrados/semanas_aprobacionp.xlsx" )
semanas <- mutate(semanas,Inicio = ymd(Inicio),Fin = ymd(Fin)) 



bimmayo <- mutate_at(bimmayo,c("Bloque.seguimiento"),~replace(.,is.na(.),"-"))

seguimiento <- subset(bimmayo,Bloque.seguimiento == "-")
bimmayo <- subset(bimmayo,Bloque.seguimiento != "-")




sinbloque <- read_excel("D:/Aprobacion_Posgrados/sinbloquejulago21.xlsx" )
seguimiento <- left_join(seguimiento,sinbloque,by="Matricula")
seguimiento$Bloque.seguimiento <- seguimiento$Bimestre
seguimiento$Bimestre <- NULL
bimmayo <- rbind(bimmayo,seguimiento)







bimmayo <- mutate(bimmayo,Semana = "-",Bimestre = "-")

for (i in 1:length(rownames(semanas))) {
  # Rep <- mutate(Rep,Motivo Rechazo=if_else(Motivo Rechazo=="_",
  # if_else(grepl(Rep2[[i,1]],COMENTARIO,ignore.case = T),Rep2[[i,2]],"_"),Motivo Rechazo))
  bimmayo <- mutate(bimmayo,Semana = if_else(Semana == "-",
                                           if_else(Inicio.de.curso == semanas[[i,1]]
                                                   & Fecha_creacion >= semanas [[i,2]]
                                                   & Fecha_creacion <= semanas [[i,3]],semanas [[i,"Semana"]],
                                                   "-")
                                           ,Semana))
  
  bimmayo <- mutate(bimmayo,Bimestre = if_else(Bimestre == "-",
                                             if_else(Inicio.de.curso == semanas[[i,1]]
                                                     & Fecha_creacion >= semanas [[i,2]]
                                                     & Fecha_creacion <= semanas [[i,3]],semanas [[i,5]],
                                                     "-")
                                             ,Bimestre))
  

}


negocio <- read.csv("D:/Users/eescobev/Documents/REPORTES/ESTADOS/reglas_negocios.csv")
names(negocio)[1]="Matricula"
bimmayo <- left_join(bimmayo,negocio,by="Matricula")
bimmayo <- mutate_at(bimmayo,c("Regla_Negocio"),~replace(.,is.na(.),"-"))



tablaporcentajes <- count_(bimmayo,c("Asignatura","Bloque.seguimiento","NR","Fecha_creacion","Semana","Bimestre","Regla_Negocio"))
tablaporcentajes <- subset(tablaporcentajes,Semana != "-")
tablaporcentajes <- tidyr::spread(tablaporcentajes,
                                  key="NR",
                                  value="n")
tablaporcentajes <- mutate_at(tablaporcentajes,c("Aprobado","Np","Nuncas","Por aprobar","Reprobó"),~replace(.,is.na(.),0))


write.csv(tablaporcentajes,file="D://Aprobacion_Posgrados/actual.csv",row.names = F)


# ### Actual ####
# sabana <- read.csv("D:/Users/eescobev/Documents/REPORTES/FRONT/sabana.csv")
# sabana <- mutate(sabana,fecha_hora_extraccion = ymd(fecha_hora_extraccion))
# 
# 
# 
# 
# sabana <- mutate(sabana,Semana = "-",Bimestre = "-",Bloque = "-")
# 
# for (i in 1:length(rownames(semanas))) {
#   # Rep <- mutate(Rep,Motivo Rechazo=if_else(Motivo Rechazo=="_",
#   # if_else(grepl(Rep2[[i,1]],COMENTARIO,ignore.case = T),Rep2[[i,2]],"_"),Motivo Rechazo))
#   sabana <- mutate(sabana,Semana = if_else(Semana == "-",
#                                            if_else(inicio_plus == semanas[[i,1]]
#                                                    & fecha_hora_extraccion >= semanas [[i,2]]
#                                                    & fecha_hora_extraccion <= semanas [[i,3]],semanas [[i,4]],
#                                                    "-")
#                                            ,Semana))
#   
#   sabana <- mutate(sabana,Bimestre = if_else(Bimestre == "-",
#                                              if_else(inicio_plus == semanas[[i,1]]
#                                                      & fecha_hora_extraccion >= semanas [[i,2]]
#                                                      & fecha_hora_extraccion <= semanas [[i,3]],semanas [[i,5]],
#                                                      "-")
#                                              ,Bimestre))
#   
#   sabana <- mutate(sabana,Bloque = if_else(Bloque == "-",
#                                            if_else(inicio_plus == semanas[[i,1]]
#                                                    & fecha_hora_extraccion >= semanas [[i,2]]
#                                                    & fecha_hora_extraccion <= semanas [[i,3]],semanas [[i,6]],
#                                                    "-")
#                                            ,Bloque))
#   
# }
# 
# sabana <- left_join(sabana,negocio,by="matricula")
# 
# 
# 
# write.csv(sabana,file="D://Conectivida/sabana.csv",row.names = F)
# 

rm(list = ls())
