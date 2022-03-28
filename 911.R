library(dplyr)
library(tidyr)
library(lubridate)
library(stringi)
library(readr)
library(readxl)
#Variables principales
c_G <- "D:/Cosa/911"
c_D <- paste(c_G,"Descargas",sep="/")
c_I <- paste(c_G,"Info",sep="/")
#####-------------------------------------------------------Estatus general-------------------------------------------------------#####
Base <- read_csv(paste(c_D,"Estatus_General_Alumno.csv",sep="/"),locale=locale(encoding = "LATIN1"),na=c(""," "))%>%
  mutate_all(~replace(.,is.na(.),"_")) %>% mutate_at(c("FECHA_ESTATUS","FECHAINICIO"),~dmy(.)) %>% arrange(desc(FECHA_ESTATUS),desc(FECHAINICIO))%>%
  mutate(CLAVE_PROG=substring(PROGRAMA,1,10),PROGRAMA=substring(PROGRAMA,12)) %>% unite(MATPROG,MATRICULA,CLAVE_PROG,sep="",remove=F) %>% mutate(MATPROG=gsub("_","",MATPROG))%>%
  group_by(MATPROG) %>% mutate(cont=row_number()) %>% as.data.frame() %>% filter(cont==1) %>% select(-cont)
#Tip edad
Base <- mutate(Base,FECHA_NAC=dmy(FECHA_NAC),EDAD=as.integer(if_else(EDAD=="_","0",EDAD)),Tip_edad=as.integer((Sys.Date() - FECHA_NAC)/365),
               Tip_edad=if_else((Tip_edad<18 & EDAD>17),EDAD,Tip_edad),Tip_edad=if_else(!is.na(FECHA_NAC),if_else(Tip_edad<18,"menos de 18 años",
                                                                                                                  if_else(Tip_edad<20,"18 y 19 años",
                                                                                                                          if_else(Tip_edad<26,"20 a 25 años",
                                                                                                                                  if_else(Tip_edad<30,"26 a 29 años",
                                                                                                                                          "30 años o más")))),"Sin fecha"))
#####-------------------------------------------------------Cruce con info general-------------------------------------------------------#####
#Programas y RVOE
Rep <- read_excel(paste(c_I,"Info_general.xlsx",sep="/"),sheet="Programas") %>% select(-Nivel,-Campus)
Base <- left_join(Base,Rep,by="CLAVE_PROG")
#Campus
Rep <- read_excel(paste(c_I,"Info_general.xlsx",sep="/"),sheet="Campus") %>% select(-Fecha_campus)
Base <- left_join(Base,Rep,by="CAMPUS")
#Nivel
Rep <- read_excel(paste(c_I,"Info_general.xlsx",sep="/"),sheet="Nivel")
Base <- left_join(Base,Rep,by="NIVEL")
#####-------------------------------------------------------Re ordenando e imprimiendo-------------------------------------------------------#####
Rep <- read_excel(paste(c_I,"Info_general.xlsx",sep="/"),sheet="Orden")
Base <- Base[Rep$Incluir] %>% mutate_all(~as.character(.)) %>% mutate_all(~replace(.,is.na(.),"_"))
write.csv(Base,paste(c_G,"911.csv",sep="/"),row.names = F)












