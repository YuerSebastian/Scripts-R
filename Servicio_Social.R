library(dplyr)
library(tidyr)
library(lubridate)
library(stringi)
library(readr)
library(readxl)
library(MultiLEDS)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#####---------------------------------Servicio Social---------------------------------#####
c_Gen <- "D:/Cosa/SIR/Servicio social/"
c_Desc <- paste(c_Gen,"Descargas/",sep = "")
c_Info <- paste(c_Gen,"Info/",sep = "")
Base_SS <- read_csv(paste(c_Desc,"Servicio_Social.csv",sep=""),col_types = cols(MATRICULA = col_character(),PORCENTAJE_AVANCE_CURRICULAR = col_character(),SALDO = col_character(), DEPENDENCIA = col_skip()))
Base_SS <- mutate(Base_SS, ESTATUS_DEL_SERVICIO_SOCIAL = if_else(ESTATUS_DEL_SERVICIO_SOCIAL=="NO INICIADO" & is.na(TIPO_SERVICIO_SOCIAL)==F & is.na(FECHA_LIBERACION)==F,"LIBERADO",
                                                                 if_else(ESTATUS_DEL_SERVICIO_SOCIAL=="NO INICIADO" & is.na(TIPO_SERVICIO_SOCIAL)==F & is.na(FECHA_LIBERACION)==T,"EN PROCESO",
                                                                         ESTATUS_DEL_SERVICIO_SOCIAL)))
#####---------------------------------Avance---------------------------------#####
Base_SS <- unite(Base_SS, MATPROG, MATRICULA, PROGRAMA, remove = F, sep = "")%>%
  mutate(PORCENTAJE_AVANCE_CURRICULAR=as.numeric(PORCENTAJE_AVANCE_CURRICULAR),
         Avance=if_else(PORCENTAJE_AVANCE_CURRICULAR>=0 & PORCENTAJE_AVANCE_CURRICULAR<=55,"0-55",
                        if_else(PORCENTAJE_AVANCE_CURRICULAR>=56 & PORCENTAJE_AVANCE_CURRICULAR<=69,"56-69","70-100")))
#####---------------------------------Urgentes, Urgentes 2 y ESTATUS_TIT---------------------------------#####
Aux <- read.csv(paste(c_Gen,"Avance SS.csv",sep = "")) %>% select(MATPROG, Urgentes, Urgentes_2, ESTATUS_TIT) %>% unique()
# Aux <- mutate(Aux, MATRICULA=as.character(MATRICULA), MATRICULA=if_else(nchar(MATRICULA)==8,paste("0",MATRICULA,sep = ""),MATRICULA)) %>%
#   unite(MATPROG,MATRICULA,PROGRAMA,sep = "",remove = F) %>% select(MATPROG, Urgentes, Urgentes_2, ESTATUS_TIT) %>% unique()
Base_SS <- left_join(Base_SS, Aux, by="MATPROG")
#####---------------------------------SS_Doc---------------------------------#####
Aux <- read_csv(paste(c_Desc,"SS_Doc.csv",sep=""),col_types = cols(STUDY_PATH = col_character(),MATRICULA = col_character()))

Aux <- mutate(Aux, Doc = if_else((ESTATUS=="VALIDADO" & ESTATUS4=="VALIDADO" & ESTATUS8=="VALIDADO")|
                                   (ESTATUS1=="VALIDADO" & ESTATUS5=="VALIDADO" & ESTATUS9=="VALIDADO")|
                                   (ESTATUS2=="VALIDADO" | ESTATUS6=="VALIDADO")|
                                   (ESTATUS3=="VALIDADO" | ESTATUS7=="VALIDADO"),"Liberado","_"))%>%
  filter(Doc=="Liberado") %>% select(MATRICULA,Doc) %>% unique()
Base_SS <- left_join(Base_SS, Aux, by="MATRICULA")
#####---------------------------------Cert_Tit_Dig---------------------------------#####
Aux <- read_csv(paste(c_Desc,"Cert_Tit_Dig.csv",sep=""),col_types = cols(NO_SOLCITUD = col_character(),NUM_TRANS = col_character()))%>%
  select(MATRICULA, CVE_PROGRAMA, TIPO) %>% unite(MATPROG, MATRICULA, CVE_PROGRAMA, sep = "") %>% unique() %>% arrange(MATPROG,desc(TIPO)) %>% group_by(MATPROG)%>%
  mutate(cont = row_number()) %>% mutate(TIPO = if_else(cont==2,"Ti??tulo",TIPO)) %>% select(-cont) %>% unique() %>% mutate(TIPO=if_else(TIPO=="Ti??tulo","Título",TIPO))%>%
  mutate(TITULO=if_else(TIPO=="Título",TIPO,"Pendiente"), CERTIFICADO=if_else(TIPO=="Certificado",TIPO,"Pendiente")) %>% select(MATPROG,TITULO,CERTIFICADO)
Base_SS <- left_join(Base_SS, Aux, by="MATPROG") %>% mutate_at(c("TITULO","CERTIFICADO"),~replace(.,is.na(.),"Pendiente"))

#####---------------------------------ACCION y GLOBAL---------------------------------#####
Base_SS <- mutate(Base_SS, ACCION = if_else(ESTATUS_DEL_SERVICIO_SOCIAL=="LIBERADO","Liberado",
                                            if_else(ESTATUS_DEL_SERVICIO_SOCIAL=="EN PROCESO" | ESTATUS_DEL_SERVICIO_SOCIAL=="CONCLUIDO","En proceso",
                                                    ifelse(ESTATUS_DEL_SERVICIO_SOCIAL=="NO INICIADO" & TITULO=="Pendiente","Pendiente",
                                                           if_else(ESTATUS_DEL_SERVICIO_SOCIAL=="NO INICIADO" & TITULO=="Título","Alinear/Liberado","_")))), GLOBAL=ACCION)
#Reemlazando todos los NA
Base_SS <- mutate_all(Base_SS,~replace(.,is.na(.),"_"))
#####---------------------------------Pendientes (completo, incompleto)---------------------------------#####
Base_SS$SALDO <- as.numeric(Base_SS$SALDO)
Base_SS$SALDO[is.na(Base_SS$SALDO)] <- 0
Base_SS <- mutate(Base_SS, Pendientes = if_else(ACCION=="Pendiente" & Avance=="70-100",if_else(ACTA_ORIGINAL=="VALIDADO" & CERT_TOTAL_BACHILLERAT_ORIG=="VALIDADO" & SALDO<=2000 &
                                                                                                 ESTATUS_DEL_SERVICIO_SOCIAL=="NO INICIADO", "Completo", "Incompleto"),"_"))
#####---------------------------------Contactación---------------------------------#####
Aux <- read_excel(paste(c_Info,"Contactación.xlsx",sep = "")) %>% mutate_all(~as.character(.)) %>% mutate_all(~stri_trim(.))
Aux <- mutate(Aux, MATRICULA=if_else(nchar(MATRICULA)==8,paste("0",MATRICULA,sep = ""),MATRICULA)) %>% select(MATRICULA, RESPONSABLE, COMENTARIO)
#%>% unite(MATPROG,MATRICULA,PROGRAMA,sep = "",remove = F) %>% select(MATPROG, RESPONSABLE, COMENTARIO) %>% mutate_all(~stri_trim(.))
Aux$COMENTARIO[is.na(Aux$COMENTARIO)] <- "No contactado"
#cont <- count(Aux,COMENTARIO)
Base_SS <- left_join(Base_SS,Aux,by="MATRICULA")
Base_SS$RESPONSABLE[is.na(Base_SS$RESPONSABLE)] <- "_"
Base_SS$COMENTARIO[is.na(Base_SS$COMENTARIO)] <- "_"
#####---------------------------------CRM Interacciones---------------------------------#####
Aux <- read_csv(paste(c_Gen,"CRM_Interacciónes_His.csv",sep=""),locale = locale(encoding = "LATIN1"))%>%
  arrange(desc(fecha_hora)) %>% group_by(matricula) %>% mutate(cont=row_number()) %>% as.data.frame() %>% filter(cont==1)%>%
  select(-cont)
Aux2 <- read_excel(paste(c_Info,"Info_general.xlsx",sep="/")) %>% mutate(Si="Si") %>% reshape::rename(c(Responsable="agente"))
Aux <- left_join(Aux,Aux2,by="agente") %>% filter(Si=="Si")%>%
  select(matricula,agente,id_llamada,fecha_hora,estado_llamada,tipifica_oper,clasificacion_primaria) %>% mutate_all(~as.character(.))%>%
  mutate_all(~replace(.,is.na(.),"_")) %>% reshape::rename(c(matricula="MATRICULA"))
Base_SS <- left_join(Base_SS,Aux,by="MATRICULA")
Base_SS <- mutate_all(Base_SS,~replace(.,is.na(.),"_"))
#####---------------------------------Campus---------------------------------#####
Aux <- leer(c("Info General","gsheet"),secc = "Campus") %>% select("CAMPUS"=`Clave Campus`,`Tipo Campus`)
Base_SS <- left_join(Base_SS,Aux,by="CAMPUS")
Base_SS <- filter(Base_SS,!`Tipo Campus` %in% c("OPM","Alianzas UTL"))
#####---------------------------------Imprimiendo---------------------------------#####
Aux <- read_excel(paste(c_Info,"Info_general.xlsx",sep = ""),sheet = "Orden")
Aux <- Aux$Incluir
Base_SS <- Base_SS[Aux]
write.csv(Base_SS,paste(c_Gen,"Avance SS.csv",sep=""),row.names = F,fileEncoding = "LATIN1")



















