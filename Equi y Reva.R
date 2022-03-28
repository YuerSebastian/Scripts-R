library(dplyr)
library(tidyr)
library(stringi)
library(lubridate)
library(readxl)
library(readr)
library(googlesheets4)
#Variables iniciales
c_G <- "D:/Cosa/Equi y Reva"
c_D <- paste(c_G,"Descargas",sep = "/")
c_I <- paste(c_G,"Info",sep = "/")
gs4_auth(email = "jsalinba@utel.edu.mx")#Iniciando google
#C <- list.dirs("D:/Cosa/SIR/KPI")
#Funciones
Rep_nom <- function(repet,tam){
  x <- as.integer(length(tam)/length(repet))
  x <- rep(repet,x)
  if((length(tam)-length(x))!=0) x <- append(x,x[c(1:(length(tam)-length(x)))])
  return(x)
}
#####---------------------------------------Documentos Recolección---------------------------------------#####
Docs <- read_csv(paste(c_D,"Documento_Recoleccion.csv",sep="/"),na = c(""," ","NA"),show_col_types = F)%>%
  select(CAMPUS,NIVEL,PERIODO_DE_CATALOGO,PROGRAMA,DESCRIPCION_PROGRAMA,MATRICULA,TIPO_DE_INGRESO,DECISION,USUARIO_DECISION,FECHA_DECISION,FECHA_INICIO,
         ACTA_ORIG,CERTIFICADO_PARCIAL, CERT_TOTAL_BACHILLERAT_ORIG, CERTIF_TOT_LIC_ORIG, CERT_TOT_MAES_ORIG, CARTA_PODER, DICTAMEN_SEP_DIG, DICTAMEN_SEP_ORIGINAL,
         EQUIVALENCIA_DE_ESTUDIOS_DIG, EQUIVALENCIA_DE_ESTUDIOS_ORIG, DICTAMEN_REVALIDACION_DIG, DICTAMEN_REVALIDACION_ORG) %>% mutate_all(~as.character(.))%>%
  unite(MATPROG,MATRICULA,PROGRAMA,sep = "",remove = F) %>% mutate_at(c("FECHA_DECISION","FECHA_INICIO"),~parse_date_time(.,c("ymd")))%>%
  arrange(desc(FECHA_DECISION),desc(FECHA_INICIO)) %>% group_by(MATPROG) %>% mutate(cont=row_number()) %>% as.data.frame() %>%  filter(cont==1) %>% select(-cont)%>%
  reshape::rename(c(TIPO_DE_INGRESO="TIPO"))
#####---------------------------------------Estatus general alumnos---------------------------------------#####
Rep <- read_csv(paste(c_D,"Estatus_General_Alumno.csv",sep="/"),na = c(""," ","NA"),show_col_types = F)%>%
  select(MATRICULA, PROGRAMA, ESTUDIANTE, FECHAINICIO, TIPO_INGRESO, PRIMER_INSCRIPCION_SIN_ESTATUS, FECHA_ESTATUS, ESTATUS) %>% mutate_all(~replace(.,is.na(.),"_"))%>%
  mutate(CLAVE_PROG=substring(PROGRAMA,1,10)) %>% unite(MATPROG,MATRICULA,CLAVE_PROG,sep = "") %>% mutate(MATPROG=gsub("_","",MATPROG))%>%
  mutate_at(c("FECHA_ESTATUS","FECHAINICIO"),~dmy(.)) %>% arrange(desc(FECHA_ESTATUS),desc(FECHAINICIO))%>%
  group_by(MATPROG) %>% mutate(cont=row_number()) %>% as.data.frame() %>% filter(cont==1) %>% select(-cont,-PROGRAMA)
Docs <- left_join(Docs,Rep,by="MATPROG") %>% filter(TIPO_INGRESO=="REVALIDACION" | TIPO_INGRESO=="EQUIVALENCIA")%>%#Filtrando solo rev y equi
  mutate_all(~as.character(.)) %>% mutate(ESTUDIANTE=gsub("/"," ",ESTUDIANTE))
#####---------------------------------------Avance curricular---------------------------------------#####
Rep <- read_csv(paste(c_D,"Avance_Curricular.csv",sep="/"),na = c(""," ","NA"),show_col_types = F) %>% select(MATRICULA,PROGRAMA,AVANCE_CURRICULAR)%>%
  reshape::rename(c(AVANCE_CURRICULAR="AVANCE")) %>% unite(MATPROG,MATRICULA,PROGRAMA,sep = "") %>% mutate(Tip_Avance=if_else(AVANCE<26,"0-25",
                                                                                                                              if_else(AVANCE<51,"26-50",
                                                                                                                                      if_else(AVANCE<76,"51-75","76-100"))))
Docs <- left_join(Docs,Rep,by="MATPROG")
#####---------------------------------------Reporte SSB (Pago Tit, Equi y Rev)---------------------------------------#####
Rep <- read_csv(paste(c_D,"Solicitudes_Generadas_por_SSB.csv",sep="/"),na = c(""," ","NA"),show_col_types = F)%>%
  select(SERVICIO, SEQ_NO, MATRICULA, PROGRAMA, ESTATUS_SOLC)%>%
  filter((SERVICIO=="TITULACION"|SERVICIO=="COLEGIATURA FINAL"|SERVICIO=="OBTENCION DE GRADO"|SERVICIO=="EQUIVALENCIA"|SERVICIO=="REVALIDACION")&
           (ESTATUS_SOLC=="PAGADO")) %>% mutate(CLAVE_PROG=substring(PROGRAMA,1,10)) %>% unite(MATPROG,MATRICULA,CLAVE_PROG,sep = "") %>% arrange(desc(SEQ_NO))%>%
  group_by(MATPROG) %>% mutate(cont=row_number()) %>% as.data.frame() %>% filter(cont==1)%>%
  mutate(Pago_Equi_Rev_SSB=if_else(SERVICIO=="EQUIVALENCIA" | SERVICIO=="REVALIDACION","Pagado","_"),
         Pago_Tit_SSB=if_else(SERVICIO!="EQUIVALENCIA" & SERVICIO!="REVALIDACION","Pagado","_")) %>% select(MATPROG,SERVICIO,Pago_Equi_Rev_SSB,Pago_Tit_SSB)
Docs <- left_join(Docs,Rep,by="MATPROG")
#####---------------------------------------Tip Documentos---------------------------------------#####
Docs <- mutate_all(Docs,~replace(.,is.na(.),"_"))
# Docs <- mutate(Docs,Tip_Docs_Equi_Rev=if_else(DICTAMEN_SEP_DIG=="VALIDADO" | DICTAMEN_SEP_ORIGINAL=="VALIDADO","Finalizado",
#                                     if_else(ACTA_ORIG=="VALIDADO" & CERTIFICADO_PARCIAL=="VALIDADO" & CERT_TOTAL_BACHILLERAT_ORIG=="VALIDADO" & CARTA_PODER=="VALIDADO", "Completos",
#                                             if_else(TIPO_INGRESO=="REVALIDACION" & CERT_TOTAL_BACHILLERAT_ORIG=="NOACEPTADO", "Revisar","Incompletos"))))
Docs <- mutate(Docs,Tip_Docs_Equi_Rev=if_else(DICTAMEN_SEP_DIG=="VALIDADO" | DICTAMEN_SEP_ORIGINAL=="VALIDADO","Finalizado",
                                              if_else(ACTA_ORIG=="VALIDADO" & CERTIFICADO_PARCIAL=="VALIDADO" & CERT_TOTAL_BACHILLERAT_ORIG=="VALIDADO" & CARTA_PODER=="VALIDADO",
                                                      "Completos","Incompletos")))
#Todos los Finalizados pasan a Pagados en la columna SSB para Tit
Docs <- mutate(Docs,Pago_Tit_SSB=if_else(Tip_Docs_Equi_Rev=="Finalizado","Pagado",Pago_Tit_SSB))
#####---------------------------------------Pago equivalencias---------------------------------------#####
Rep <- read_excel(paste(c_I,"Equivalencias.xlsx",sep="/")) %>% mutate_all(~as.character(.)) %>% arrange(desc(NUMERO_TRANSACCION)) %>% select(MATRICULA,PAGO)%>%
  mutate(MATRICULA=if_else(nchar(MATRICULA)==8,paste("0",MATRICULA,sep = ""),MATRICULA)) %>% group_by(MATRICULA) %>% mutate(cont=row_number()) %>% as.data.frame()%>%
  filter(cont==1) %>% select(-cont) %>% reshape::rename(c(PAGO="Pago_Equi"))
Docs <- left_join(Docs,Rep,by="MATRICULA") %>% mutate_all(~replace(.,is.na(.),"_"))
Docs <- mutate(Docs,Tip_pago_equi=if_else(Pago_Equi=="COMPLETO","Pagado","Sin pago"))
#####---------------------------------------Certificados títulos digitales---------------------------------------#####
Rep <- read_csv(paste(c_D,"Cert_Tit_Dig.csv",sep="/"),na = c(""," ","NA"),show_col_types = F) %>% select(MATRICULA,CVE_PROGRAMA,TIPO)%>%
  unite(MATPROG,MATRICULA,CVE_PROGRAMA,sep = "") %>% unique() %>% arrange(desc(TIPO)) %>% group_by(MATPROG) %>% mutate(cont=row_number()) %>% as.data.frame()%>%
  filter(cont==1) %>% select(-cont) %>% mutate(TIPO=if_else(TIPO=="Ti??tulo","Titulado","Pendiente")) %>% reshape::rename(c(TIPO="Estatus_Tit"))
Docs <- left_join(Docs,Rep,by="MATPROG")
#####---------------------------------------Tip Pago equivalencia y acción---------------------------------------#####
Docs <- mutate_at(Docs,c("Tip_Docs_Equi_Rev","Estatus_Tit"),~replace(.,is.na(.),"_"))
Docs <- mutate(Docs,AVANCE=as.numeric(AVANCE),Acción=if_else(Tip_Docs_Equi_Rev=="Finalizado" | Estatus_Tit=="Titulado","Completo",
                                                             if_else(Tip_Docs_Equi_Rev=="Completos" & Estatus_Tit!="Titulado","Ingresar trámite",
                                                                     if_else((Tip_Docs_Equi_Rev=="Completos" & Tip_pago_equi=="Sin pago") | (Tip_Docs_Equi_Rev=="Incompletos"),
                                                                             if_else(AVANCE>51,"Front","Recolección"),"_"))))
#####---------------------------------------Reporte CRM Interacciones---------------------------------------#####
#Actualizando histórico CRM
# Rep2 <- read_excel(paste(c_D,"CRM_Interacciónes_DA.xlsx",sep="/"),skip = 3) %>% mutate_all(~as.character(.)) %>% mutate_all(~replace(.,.==""|.==" "|is.na(.),"_"))%>%
#   filter(departamento_agente=="Servicios Escolares" | departamento_agente=="Recoleccion")
Rep <- read_csv(paste(c_G,"CRM_Interacciones_His.csv",sep="/"),locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c"),lazy = F)%>%
  select(-`No. llamadas`,-Equipo)# %>% rbind(Rep2)
Rep <- Rep[!duplicated(Rep$id_llamada),]#Por si hay duplicados
cont <- count(Rep,matricula) %>% rename("No. llamadas"=n)
Rep <- left_join(Rep,cont,by="matricula")
Rep2 <- read_excel(paste(c_I,"Info_general.xlsx",sep="/"),sheet = "Agentes")
Rep <- left_join(Rep,Rep2,by="agente")

write.csv(Rep,paste(c_G,"CRM_Interacciones_His.csv",sep="/"),row.names = F)
#Cruzando info
Rep <- select(Rep,matricula,agente,fecha_hora,id_llamada,estado_llamada,tipifica_oper)%>%
  mutate(fecha_hora=ymd_hms(fecha_hora),matricula=if_else(nchar(matricula)==8,paste("0",matricula,sep = ""),matricula)) %>% arrange(desc(fecha_hora))%>%
  group_by(matricula) %>% mutate(cont=row_number()) %>% as.data.frame() %>% filter(cont==1) %>% select(-cont) %>% mutate_all(~as.character(.))%>%
  reshape::rename(c(matricula="MATRICULA",agente="Agente_LL",fecha_hora="Fecha_LL",id_llamada="Id_llamada",estado_llamada="Estado_LL",tipifica_oper="Tip_llamada"))
Docs <- left_join(Docs,Rep,by="MATRICULA")
#####---------------------------------------Reporte CRM Correos---------------------------------------#####
Rep <- read_excel(paste(c_D,"CRM_Correos.xlsx",sep="/"),skip = 3) %>% select(matricula,agente,fecha,status)%>%
  mutate_all(~replace(.,is.na(.),"_")) %>% mutate_all(~as.character(.)) %>% mutate(fecha=ymd_hms(fecha),status="Enviado",
                                                                                   matricula=if_else(nchar(matricula)==8,paste("0",matricula,sep = ""),matricula))%>%
  arrange(desc(fecha)) %>% group_by(matricula) %>% mutate(cont=row_number()) %>% as.data.frame() %>% filter(cont==1) %>% select(-cont)%>%
  reshape::rename(c(matricula="MATRICULA",agente="Agente_C",fecha="Fecha_C",status="Estatus_C"))
Docs <- left_join(Docs,Rep,by="MATRICULA")
#####---------------------------------------Tip contacto---------------------------------------#####
Docs <- mutate(Docs,Tip_contactacion=if_else(!is.na(Tip_llamada) & !is.na(Estatus_C),"Contactado",
                                             if_else(is.na(Tip_llamada) & is.na(Estatus_C),"Sin contacto",
                                                     if_else(!is.na(Tip_llamada) & is.na(Estatus_C),"Contactado",
                                                             if_else(is.na(Tip_llamada) & !is.na(Estatus_C),"Solo correo","_")))))
#####---------------------------------------Asignación Responsables---------------------------------------#####
Rep <- read_csv(paste(c_G,"Equi y Reva.csv",sep="/"),locale=locale(encoding="LATIN1")) %>% select(MATPROG,Responsable)
Docs <- left_join(Docs,Rep,by="MATPROG")
Rep2 <- filter(Docs,is.na(Responsable))
#Asignando nuevos
Rep2 <- mutate(Rep2,Responsable=if_else(Acción=="Ingresar trámite","Pendiente",
                                        if_else(Acción=="Recolección","front","front")))
Rep <- read_excel(paste(c_I,"Info_general.xlsx",sep="/"),sheet = "Responsables")
Rep <- Rep$Responsable
Rep3 <- filter(Rep2,Responsable=="front") %>% arrange(Acción,Tip_contactacion,ESTATUS) %>% mutate(Responsable=Rep_nom(Rep,rownames(.)))
Rep2 <- filter(Rep2,Responsable!="front") %>% rbind(Rep3)
Docs <- filter(Docs,!is.na(Responsable)) %>% rbind(Rep2)
remove(Rep2,Rep3)
#####---------------------------------------Equipos---------------------------------------#####
#Llamadas
Rep <- read_excel(paste(c_I,"Info_general.xlsx",sep="/"),sheet = "Nombres") %>% select(Nombre,Área)%>%
  reshape::rename(c(Nombre="Agente_LL",Área="Equipo_LL"))
Docs <- left_join(Docs,Rep,by="Agente_LL")
#correos
Rep <- reshape::rename(Rep,c(Agente_LL="Agente_C",Equipo_LL="Equipo_C"))
Docs <- left_join(Docs,Rep,by="Agente_C")
Docs <- mutate_at(Docs,c("Equipo_LL","Equipo_C"),~replace(.,is.na(.),"Otro"))
#####---------------------------------------Incidencias---------------------------------------#####
Rep <- range_read("1S38DxEh0c03-S3Mrfqni8xPCkZ933JWjU6c1NAdCNrg",sheet=1,col_types="c") %>% unite(MATPROG,Matrícula,`Clave programa`,sep="")%>%
  select(-Responsable) %>% filter(!duplicated(MATPROG)) %>% mutate(Incidencia="Si")
Docs <- left_join(Docs,Rep,by="MATPROG") %>% mutate(Incidencia=if_else(is.na(Incidencia),"No",Incidencia))
#####---------------------------------------Documentos faltantes (avisos) Arreglar después---------------------------------------#####
#Docs <- select(Docs,-Documentos_faltantes)
#Docs <- select(Docs,-Doc)
x <- Docs[c(13:24)]
x1 <- c("Acta-Original","Certificado-Parcial","Certificado-total-bachillerato-original","Certificado-total-licenciatura-original","Certificado-total-maestría-original",
        "Carta-poder","Dictamen-SEP-digital","Dictamen-SEP-original","Equivalencia-de-estudios-digital","Equivalencia-de-estudios-original","Dictamen-revalidación-digital",
        "Dictamen-revalidación-original")

Docs$Documentos_faltantes <- ""
for (i in 1:(length(x))) {
  Docs$Doc <- ifelse(x[,i]=="VALIDADO" | x[,i]=="_","",x1[i])
  Docs <- unite(Docs, Documentos_faltantes, Documentos_faltantes, Doc, sep = " ")
}
Docs$Documentos_faltantes <- stringi::stri_trim(Docs$Documentos_faltantes)
Docs$Documentos_faltantes <- gsub(" |  |   |    |     |      |       |        |         |          |           "," | ",Docs$Documentos_faltantes)
Docs <- mutate(Docs, Documentos_faltantes=replace(Documentos_faltantes,Documentos_faltantes=="","_"))
Docs$Documentos_faltantes <- gsub("-"," ",Docs$Documentos_faltantes)
remove(x)#-----X
#####---------------------------------------Docs Moy---------------------------------------#####
Rep <- range_speedread("1HJJyzyAWIz7TxqzEOFsAxmVBmgaMZpl_Jp9Nv3JmDbc",sheet="Documentos",col_types = cols(.default = "c"))%>%
  mutate(MATRICULA=if_else(nchar(MATRICULA)<9,paste("0",MATRICULA,sep=""),MATRICULA)) %>% unite(MATPROG,MATRICULA,PROGRAMA,sep="")%>%
  reshape::rename(c(`estatus expediente`="Docs_físicos",`expediente digital`="Docs_digitales",`Fecha recu`="Fecha_DF",`Fecha dig`="Fecha_DD"))%>%
  arrange(desc(Fecha_DD),desc(Fecha_DF)) %>% filter(!duplicated(MATPROG)) %>% select(MATPROG,Equivalencia,Tipo_Ingreso)
Docs <- left_join(Docs,Rep,by="MATPROG") %>% mutate(Equivalencia=if_else(is.na(Equivalencia),"Incompleto",Equivalencia))
#####---------------------------------------Re ordenando e imprimiendo---------------------------------------#####
Docs <- mutate(Docs,Ult_Act=as.character(now()),PRIMER_INSCRIPCION_SIN_ESTATUS=dmy(PRIMER_INSCRIPCION_SIN_ESTATUS))
Rep <- read_excel(paste(c_I,"Info_general.xlsx",sep="/"),sheet = "Orden")
Rep <- Rep$Incluir
Docs <- Docs[Rep] %>% mutate_all(~as.character(.)) %>% mutate_all(~replace(.,is.na(.),"_"))
write.csv(Docs,paste(c_G,"Equi y Reva2.csv",sep="/"),row.names = F,na = "_")






















