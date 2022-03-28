library(dplyr)
library(stringi)
#####----------------------------------------------------------Estatus general alumnos (limpiando)----------------------------------------------------------#####
Est_Gen <- read.csv("D:/Cosa/Alumnos/Sin materias/Estatus_General_Alumno.csv") %>% mutate_all(~as.character(.) )
Est_Gen <- select(Est_Gen, MATRICULA, ESTUDIANTE, ESTATUS, PROGRAMA, NIVEL, CAMPUS, FECHAINICIO, PRIMER_INSCRIPCION_SIN_ESTATUS, PERIODO_CATALOGO, TIPO_INGRESO, DECISION, USUARIO_DECISION)%>%
  mutate_all(~replace(.,is.na(.) | .=="" | .==" ","_")) %>% mutate_all(~stri_trim(.)) %>% filter(ESTATUS=="MATRICULADO"&FECHAINICIO=="25/10/21") %>% mutate(ESTUDIANTE=gsub("/"," ",ESTUDIANTE))

Est_Gen <- Est_Gen %>% group_by(MATRICULA) %>% mutate(cont = row_number()) %>% filter(cont==1) %>% select(-cont)
#####----------------------------------------------------------Sincronización materias----------------------------------------------------------#####
descargas <- list.files("D:/Cosa/Alumnos/Sin materias/Materias", full.names = T)

for (i in 1:length(descargas)) {
  Aux <- read.csv(descargas[i])
  if(i==1) materias <- Aux else materias <- rbind(materias,Aux)
}
materias <- mutate_all(materias, ~as.character(.)) %>% select(MATRICULA, MATERIA_LEGAL) %>% group_by(MATRICULA) %>% mutate(cont = row_number()) %>% filter(cont==1) %>% select(-cont)
names(materias)[2] <- "Materias"
materias$Materias <- "Con materias"
#Cruzando y ordenando primero los "Sin materia"
Est_Gen <- left_join(Est_Gen, materias, by="MATRICULA")
Est_Gen$Materias[is.na(Est_Gen$Materias)] <- "Sin materias"
Aux <- filter(Est_Gen, Materias=="Sin materias")
Est_Gen <- filter(Est_Gen, Materias=="Con materias")
Est_Gen <- rbind(Aux, Est_Gen)
#Ordenando columnas e imprimiendo.
Est_Gen <- Est_Gen[c(1,2,13,3,4,5,6,7,8,9,10,11,12)] %>% filter(Materias=="Sin materias")
Aux <- readxl::read_excel("D:/Cosa/Alumnos/Sin materias/Info.xlsx")
Est_Gen <- left_join(Est_Gen,Aux,by="CAMPUS")

write.csv(Est_Gen, "D:/Cosa/Alumnos/Sin materias/Alumnos sin materias.csv", row.names = F)






