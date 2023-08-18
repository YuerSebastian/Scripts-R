library(MultiLEDS); library(dplyr); library(tidyr); library(lubridate)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#####------------------------Leyendo------------------------#####
BAct <- unificar(c("SIR$","ActaXRegla")) %>% mutate(inicio_plus=paste(substring(FECHA_INICIO,1,2),substring(FECHA_INICIO,4,5),sep=""))%>%
  unite(Clave,CLAVE_PROF,CVE_MATERIA,inicio_plus,GRUPO,remove = F,na.rm = T)%>%
  unite(Clave2,CLAVE_PROF,MATRICULA,CVE_MATERIA,inicio_plus,GRUPO,remove = F,na.rm = T) %>% add_count(Clave,name="No. Alumnos Act")

BSab <- leer(c("Sabana_2022-10-25_CIERREBIMESTRE","Sabana$")) %>% filter(edo_moodle=="Activo" & stutus=="MATRICULADO") %>% mutate(grupo=substring(grupo,7))%>%
  mutate_at(c("matricula","idprof"),~if_else(nchar(.)<9,paste("0",.,sep=""),.)) %>% unite(Clave,idprof,clave_materia,inicio_plus,grupo,remove = F,na.rm = T)%>%
  unite(Clave2,idprof,matricula,clave_materia,inicio_plus,grupo,remove = F,na.rm = T) %>% add_count(Clave,name="No. Alumnos Sab")
#####------------------------Cruces y cálculos------------------------#####
Rep <- select(BSab,Clave,`No. Alumnos Sab`) %>% filter(!duplicated(Clave))
BAct <- left_join(BAct,Rep,by="Clave")
Rep <- select(BSab,Clave2) %>% mutate(`Alumno Encontrado Sab`="Si") %>% filter(!duplicated(Clave2))
BAct <- left_join(BAct,Rep,by="Clave2") %>% mutate(`No. Alumnos Sab`=if_else(is.na(`No. Alumnos Sab`),0L,`No. Alumnos Sab`),
                                                   Diferencia=`No. Alumnos Act`-`No. Alumnos Sab`,Diferencia=if_else(Diferencia<0,Diferencia*-1L,Diferencia),
                                                   Validación=if_else(`No. Alumnos Sab`!=0,
                                                                      if_else(`No. Alumnos Sab`==`No. Alumnos Act`,"Correcto","Incorrecto"),"No encontrado en Sab"),
                                                   `Alumno Encontrado Sab`=if_else(is.na(`Alumno Encontrado Sab`),"No",`Alumno Encontrado Sab`))

Rep <- select(BAct,Clave,`No. Alumnos Act`) %>% filter(!duplicated(Clave))
BSab <- left_join(BSab,Rep,by="Clave")
Rep <- select(BAct,Clave2) %>% mutate(`Alumno Encontrado Act`="Si") %>% filter(!duplicated(Clave2))
BSab <- left_join(BSab,Rep,by="Clave2") %>% mutate(`No. Alumnos Act`=if_else(is.na(`No. Alumnos Act`),0L,`No. Alumnos Act`),
                                                   Diferencia=`No. Alumnos Sab`-`No. Alumnos Act`,Diferencia=if_else(Diferencia<0,Diferencia*-1L,Diferencia),
                                                   Validación=if_else(`No. Alumnos Act`!=0,
                                                                      if_else(`No. Alumnos Act`==`No. Alumnos Sab`,"Correcto","Incorrecto"),"No encontrado en Act"),
                                                   `Alumno Encontrado Act`=if_else(is.na(`Alumno Encontrado Act`),"No",`Alumno Encontrado Act`))
#####------------------------Ordenando e Imprimiendo------------------------#####
#Actas
Rep <- leer(c("Info General","gsheet"),sheet="Orden Base Academia",secc="Actas (Actas X Regla)")
BAct <- `colnames<-`(BAct,Rep$Nuevo) %>% .[c(Rep$Incluir[!is.na(Rep$Incluir)])]
BAct <- mutate_all(BAct,~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
#Sabana
Rep <- leer(c("Info General","gsheet"),sheet="Orden Base Academia",secc="Actas (Sabana)")
BSab <- `colnames<-`(BSab,Rep$Nuevo) %>% .[c(Rep$Incluir[!is.na(Rep$Incluir)])]
BSab <- mutate_all(BSab,~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
#Imprimiendo
escribir(BSab,c("Validación Actas Sabana.csv","Principales$"))
escribir(BAct,c("Validación Actas ActasXRegla.csv","Principales$"))








