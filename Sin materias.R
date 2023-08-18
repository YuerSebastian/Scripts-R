library(dplyr)
library(stringi)
library(MultiLEDS)
library(tidyr)
library(lubridate)
diremail("D:/Cosa")
#####----------------------------------------------------------Estatus general alumnos (limpiando)----------------------------------------------------------#####
Est_Gen <- leer(c("ESTADOGENERALSIR","Sin materias$"))
Est_Gen <- select(Est_Gen, MATRICULA, ESTUDIANTE, ESTATUS, PROGRAMA, NIVEL, CAMPUS, FECHAINICIO, PRIMER_INSCRIPCION, PERIODO_CATALOGO,TIPO,
                  TIPO_INGRESO, DECISION, USUARIO_DECISION)%>%
  mutate_all(~replace(.,is.na(.) | .=="" | .==" ","_")) %>% mutate_all(~stri_trim(.)) %>% filter(ESTATUS=="MATRICULADO"&FECHAINICIO=="02/01/23") %>% mutate(ESTUDIANTE=gsub("/"," ",ESTUDIANTE))

Est_Gen <- Est_Gen %>% group_by(MATRICULA) %>% mutate(cont = row_number()) %>% filter(cont==1) %>% select(-cont)
#####----------------------------------------------------------Sincronización materias----------------------------------------------------------#####
materias <- unificar(c("Materias$"))

materias <- mutate_all(materias, ~as.character(.)) %>% select(MATRICULA, MATERIA_LEGAL) %>% group_by(MATRICULA) %>% mutate(cont = row_number()) %>% filter(cont==1) %>% select(-cont)
names(materias)[2] <- "Materias"
materias$Materias <- "Con materias"
#Cruzando y ordenando primero los "Sin materia"
Est_Gen <- left_join(Est_Gen, materias, by="MATRICULA")
Est_Gen$Materias[is.na(Est_Gen$Materias)] <- "Sin materias"
Aux <- filter(Est_Gen, Materias=="Sin materias")
Est_Gen <- filter(Est_Gen, Materias=="Con materias")
Est_Gen <- rbind(Aux, Est_Gen)
#Ordenando columnas
Est_Gen <- Est_Gen[c(1,2,14,3,4,5,6,7,8,9,10,11,12,13)] %>% filter(Materias=="Sin materias")
Aux <- readxl::read_excel("D:/Cosa/Alumnos/Sin materias/Info.xlsx")
Est_Gen <- left_join(Est_Gen,Aux,by="CAMPUS")
#Alumnos por egresar
Aux <- unificar(c("Por egresar"),col_select=c(MATRICULA)) %>% mutate(Egresado="si")
Est_Gen <- left_join(Est_Gen,Aux,by="MATRICULA")
Est_Gen <- filter(Est_Gen,is.na(Egresado) & CAMPUS!="CAP") %>% mutate(Egresado="No")
#Alumnos por egresar
Aux <- leer(c("Historico_Decisiones","Sin materias"),col_select=c("MATRICULA","PROGRAMA","FECHA_DES")) %>% mutate(PROGRAMA=substring(PROGRAMA,1,10),FECHA_DES=dmy(FECHA_DES))%>%
  arrange(desc(FECHA_DES)) %>% unite(MATPROG,MATRICULA,PROGRAMA,sep="") %>% filter(!duplicated(MATPROG))

Est_Gen <- mutate(Est_Gen,PROGRAMA=substring(PROGRAMA,1,10)) %>% unite(MATPROG,MATRICULA,PROGRAMA,sep="",remove = F)
Est_Gen <- left_join(Est_Gen,Aux,by="MATPROG")

write.csv(Est_Gen, "D:/Cosa/Alumnos/Sin materias/Alumnos sin materias.csv", row.names = F,fileEncoding = "LATIN1")
























