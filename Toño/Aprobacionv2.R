library(readxl)                      
library(dplyr)                              
library(lubridate)                           
library(stringi)                             
library(tidyr)  


# sabana <- read.csv("D://Conectivida/Sabanas Mayo/Sabana_2022-05-03.csv")
# sabana$fecha_hora_extraccion <- "2021-05-02"
# write.csv(sabana,file="D://Conectivida/Sabanas Mayo/Sabana_2022-05-02.csv",row.names = F)
# 

### Aprobacion####

mayo <- list.files ("D://Conectivida/Sabanas Mayo/", pattern = "*.csv", full.names = TRUE)

may <- data.frame(mayo)
a <- 1:length(mayo)
a <- data.frame(a)
may <- cbind(mayo,a)

for (i in 1:length(mayo)) {
  
  csv <-read.csv(mayo[i], header = TRUE,sep = ",",encoding="UTF-7",stringsAsFactors = FALSE)
  csv$links <- NULL
  csv <- subset(csv,edo_moodle == "Activo")
  csv <- csv %>% filter((grepl('MATRICULADO', .$stutus)
                         | grepl('ADMITIDO', .$stutus)))
  
  csv <- select(csv,matricula,Bloque.seguimiento,asignatura,inicio_plus,nivel_riesgo,idprof,profesor,gc,revisor,fecha_hora_extraccion)
  if(i==1) bimmayo <- csv else bimmayo <- rbind(bimmayo,csv)
}


foco_actual <- read.csv("D://Foco/materias_foco_actual.csv", header = TRUE, sep = ",",encoding = "UTF-7",stringsAsFactors = FALSE)
foco_actual$Aprobacion <- NULL
foco_actual$Activos <- NULL

bimmayo <- left_join(bimmayo,foco_actual,by="asignatura")
bimmayo <- mutate_at(bimmayo,c("Tipo_Materia"),~replace(.,is.na(.),"NORMAL"))



bimmayo <- mutate(bimmayo,fecha_hora_extraccion = ymd(fecha_hora_extraccion))

semanas <- read_excel("D:/Conectivida/semanas_aprobacion.xlsx" )
semanas <- mutate(semanas,Inicio = ymd(Inicio),Fin = ymd(Fin)) 

semanas2 <- read_excel("D:/Conectivida/semanas_aprobacion.xlsx" , sheet = "bloque")
semanas2 <- mutate(semanas2,Inicio = ymd(Inicio),Fin = ymd(Fin)) 

bimmayo <- mutate_at(bimmayo,c("Bloque.seguimiento"),~replace(.,is.na(.),"-"))

for (i in 1:length(rownames(semanas2))) {

  bimmayo <- mutate(bimmayo,Bloque.seguimiento = if_else(Bloque.seguimiento == "-",
                                             if_else(inicio_plus == semanas2[[i,1]]
                                                     & fecha_hora_extraccion >= semanas2 [[i,2]]
                                                     & fecha_hora_extraccion <= semanas2 [[i,3]],semanas2 [[i,4]],
                                                     "-")
                                             ,Bloque.seguimiento))
  

  
}






bimmayo <- mutate(bimmayo,Semana = "-",Bimestre = "-",Bloque = "-")

for (i in 1:length(rownames(semanas))) {
  # Rep <- mutate(Rep,Motivo Rechazo=if_else(Motivo Rechazo=="_",
  # if_else(grepl(Rep2[[i,1]],COMENTARIO,ignore.case = T),Rep2[[i,2]],"_"),Motivo Rechazo))
  bimmayo <- mutate(bimmayo,Semana = if_else(Semana == "-",
                                           if_else(inicio_plus == semanas[[i,1]]
                                                   & fecha_hora_extraccion >= semanas [[i,2]]
                                                   & fecha_hora_extraccion <= semanas [[i,3]],semanas [[i,"Semana"]],
                                                   "-")
                                           ,Semana))
  
  bimmayo <- mutate(bimmayo,Bimestre = if_else(Bimestre == "-",
                                             if_else(inicio_plus == semanas[[i,1]]
                                                     & fecha_hora_extraccion >= semanas [[i,2]]
                                                     & fecha_hora_extraccion <= semanas [[i,3]],semanas [[i,5]],
                                                     "-")
                                             ,Bimestre))
  
  bimmayo <- mutate(bimmayo,Bloque = if_else(Bloque == "-",
                                           if_else(inicio_plus == semanas[[i,1]]
                                                   & fecha_hora_extraccion >= semanas [[i,2]]
                                                   & fecha_hora_extraccion <= semanas [[i,3]],semanas [[i,6]],
                                                   "-")
                                           ,Bloque))
  
}


negocio <- read.csv("D:/Users/eescobev/Documents/REPORTES/ESTADOS/reglas_negocios.csv")
names(negocio)[1]="matricula"
bimmayo <- left_join(bimmayo,negocio,by="matricula")
bimmayo <- mutate_at(bimmayo,c("Regla_Negocio"),~replace(.,is.na(.),"-"))



tablaporcentajes <- count_(bimmayo,c("asignatura","Bloque.seguimiento","nivel_riesgo","Tipo_Materia","fecha_hora_extraccion","Semana","Bimestre","Bloque","Regla_Negocio"))
tablaporcentajes <- subset(tablaporcentajes,Semana != "-")
tablaporcentajes <- tidyr::spread(tablaporcentajes,
                                  key="nivel_riesgo",
                                  value="n")

tablaporcentajes <- mutate_at(tablaporcentajes,c("Aprobado","Np","Nuncas","Por aprobar","Reprobó"),~replace(.,is.na(.),0))


write.csv(tablaporcentajes,file="D://Conectivida/actual.csv",row.names = F)

#####--------------Docentes--------------#####
docentes <- count_(bimmayo,c("idprof","profesor","gc","revisor","asignatura","Bloque.seguimiento","nivel_riesgo","Tipo_Materia","fecha_hora_extraccion","Semana","Bimestre","Bloque","Regla_Negocio"))
docentes <- subset(docentes,Semana != "-")
docentes <- tidyr::spread(docentes,
                                  key="nivel_riesgo",
                                  value="n")

docentes <- mutate_at(docentes,c("Aprobado","Np","Nuncas","Por aprobar","Reprobó"),~replace(.,is.na(.),0))


write.csv(docentes,file="D://Conectivida/docentes.csv",row.names = F)


General <- count_(bimmayo,c("Bloque.seguimiento","nivel_riesgo","Tipo_Materia","fecha_hora_extraccion","Semana","Bimestre","Bloque","Regla_Negocio"))
General <- subset(General,Semana != "-")
General <- tidyr::spread(General,
                          key="nivel_riesgo",
                          value="n")

General <- mutate_at(General,c("Aprobado","Np","Nuncas","Por aprobar","Reprobó"),~replace(.,is.na(.),0))


write.csv(General,file="D://Conectivida/General.csv",row.names = F)
# 
#####----------------------------------------------#####



### Actual ####
sabana <- read.csv("D:/Users/eescobev/Documents/REPORTES/FRONT/sabana.csv")
sabana <- mutate(sabana,fecha_hora_extraccion = ymd(fecha_hora_extraccion))
sabana$links <- NULL



sabana <- mutate(sabana,Semana = "-",Bimestre = "-",Bloque = "-")

for (i in 1:length(rownames(semanas))) {
  # Rep <- mutate(Rep,Motivo Rechazo=if_else(Motivo Rechazo=="_",
  # if_else(grepl(Rep2[[i,1]],COMENTARIO,ignore.case = T),Rep2[[i,2]],"_"),Motivo Rechazo))
  sabana <- mutate(sabana,Semana = if_else(Semana == "-",
                                           if_else(inicio_plus == semanas[[i,1]]
                                                   & fecha_hora_extraccion >= semanas [[i,2]]
                                                   & fecha_hora_extraccion <= semanas [[i,3]],semanas [[i,4]],
                                                   "-")
                                           ,Semana))
  
  sabana <- mutate(sabana,Bimestre = if_else(Bimestre == "-",
                                             if_else(inicio_plus == semanas[[i,1]]
                                                     & fecha_hora_extraccion >= semanas [[i,2]]
                                                     & fecha_hora_extraccion <= semanas [[i,3]],semanas [[i,5]],
                                                     "-")
                                             ,Bimestre))
  
  sabana <- mutate(sabana,Bloque = if_else(Bloque == "-",
                                           if_else(inicio_plus == semanas[[i,1]]
                                                   & fecha_hora_extraccion >= semanas [[i,2]]
                                                   & fecha_hora_extraccion <= semanas [[i,3]],semanas [[i,6]],
                                                   "-")
                                           ,Bloque))
  
}

sabana <- left_join(sabana,negocio,by="matricula")



write.csv(sabana,file="D://Conectivida/sabana.csv",row.names = F)


rm(list = ls())
