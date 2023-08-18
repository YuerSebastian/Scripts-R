library(MultiLEDS); library(googledrive); library(dplyr); library(tidyr);library(lubridate)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#####------------------------Inicio------------------------#####
#Unificando y filtrando
Base <- unificar(c("Bases$","_POS_|_LIC_"),
                 renom = c(matricula="Matrícula",Matricula="Matrícula",
                           Calificacion="Calificación",calificacion="Calificación",
                           clave_materia="Clave Materia",
                           inicio_plus="Inicio Plus",
                           nivel_riesgo="Nivel Riesgo",NR="Nivel Riesgo",
                           Status.Materia="Estado Moodle",edo_moodle="Estado Moodle",
                           Bloque.seguimiento="Bloque Seguimiento",`Bloque seguimiento`="Bloque Seguimiento",
                           Tipo.alumno="Tipo Alumno",Tipo.Alumno="Tipo Alumno",`Tipo alumno`="Tipo Alumno",
                           Estado.Asignacion="Asignación",
                           Estado_Banner="Estado Alumno",Estado.Banner="Estado Alumno",stutus="Estado Alumno",
                           Nivel="Modalidad"),
                 cols = c("Matrícula","Calificación","Inicio Plus","Clave Materia","Ciclo","Nivel Riesgo","Estado Moodle","Bloque Seguimiento","Tipo Alumno",
                          "Asignación","Estado Alumno","Modalidad")) %>% mutate(Matrícula=if_else(nchar(Matrícula)<9,paste("0",Matrícula,sep=""),Matrícula))%>%
  filter((Asignación=="Activo"|Asignación=="por_asignar"|is.na(Asignación)) & (`Estado Alumno`=="ADMITIDO"|`Estado Alumno`=="MATRICULADO") & `Estado Moodle`=="Activo")
#Arreglando columnas que no coinciden
Base <- mutate(Base,Modalidad=if_else(Modalidad=="_","Licenciatura",Modalidad),Nivel=if_else(Modalidad!="Licenciatura"|is.na(Modalidad),"Posgrado","Licenciatura"),
               `Inicio Plus`=if_else(`Inicio Plus`=="_",substring(Ciclo,9,12),`Inicio Plus`),`Clave Materia`=if_else(`Clave Materia`=="_",substring(Ciclo,14),`Clave Materia`),
               Calificación=as.numeric(Calificación)) %>% select(-Ciclo)
#Agregando rangos y Tipo Bloque
Base <- mutate(Base,Rango=if_else(Nivel=="Licenciatura",if_else(Calificación>=0.01 & Calificación<=2.99,"0.01 a 2.99",
                                                                if_else(Calificación>=3 & Calificación<=5.69,"3 a 5.69",`Nivel Riesgo`)),
                                  if_else(Calificación>=0.01 & Calificación<=3.99,"0.01 a 3.99",
                                          if_else(Calificación>=4 & Calificación<=6.5,"4 a 6.5",`Nivel Riesgo`))),
               Per=if_else(!is.na(`Bloque Seguimiento`),
                           if_else(grepl("Bimestre",`Bloque Seguimiento`),"Bim","Cua"),"Sin"),
               Num=if_else(Per=="Bim",as.integer(substring(`Bloque Seguimiento`,10,11)),
                           if_else(Per=="Cua",as.integer(substring(`Bloque Seguimiento`,14,15)),0L)),
               `Tipo Bloque`=if_else(Num>=5,
                                     if_else(Per=="Bim","Bimestre 05 y más","Cuatrimestre 05 y más"),"_"))
for (i in 1:5) {
  Base <- mutate(Base,`Tipo Bloque`=if_else(`Tipo Bloque`=="_",
                                            if_else(Num!=0,
                                                    if_else(Num==i & Per=="Bim",paste("Bimestre 0",i,sep = ""),
                                                            if_else(Num==i & Per=="Cua",paste("Cuatrimestre 0",i,sep = ""),`Tipo Bloque`)),"Sin bloque"),`Tipo Bloque`))
}
#Ordenando e imprimiendo
Base <- select(Base,Matrícula,`Estado Alumno`,`Tipo Alumno`,Nivel,Modalidad,`Inicio Plus`,`Clave Materia`,Calificación,`Nivel Riesgo`,Rango,`Bloque Seguimiento`,
               `Tipo Bloque`,Asignación) %>% filter(!is.na(`Bloque Seguimiento`)) %>% mutate_all(~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
escribir(Base,c("Análisis Reprobados.csv","Bases$"))

































