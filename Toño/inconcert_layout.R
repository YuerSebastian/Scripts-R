library(lubridate)
library(dplyr)
library(tidyr)
library(readxl)
library(stringi)
library(MultiLEDS)


### Tablitas para cruces ####
reglas_negocios <- read.csv("D:/Users/eescobev/Documents/REPORTES/ESTADOS/reglas_negocios.csv")
reglas_negocios <- rename(reglas_negocios,matricula = MATRICULA) %>% mutate(Regla_Negocio = trimws(Regla_Negocio, whitespace = "[ \t\r\n]"))
alianza_modalidad <- read.csv("D:/Users/eescobev/Documents/REPORTES/ESTADOS/alianzamodalida.csv")
alianzaposgra <- alianza_modalidad
alianza_modalidad <- select(alianza_modalidad,-Modalidad)
heads <- read_excel("D:/Users/jvelazhe/Desktop/Licenciaturas/Inconcert/head.xlsx")
tablita_reglamx <- read_excel("D:/Users/jvelazhe/Desktop/Licenciaturas/Inconcert/tablita_reglamx.xlsx")
tablita_peaps <- read_excel("D:/Users/jvelazhe/Desktop/Licenciaturas/Inconcert/tablita_peaps.xlsx")
financiero <- read.csv("D:/Users/eescobev/Documents/REPORTES/ESTADOS/FinancieroGeneral.csv")
financiero <- select(financiero,MATRICULA,SALDO_VENCIDO,MORA,AVANCE_CURRICULAR,PROGRAMA_CODE) %>% mutate(MATRICULA = as.numeric(as.character(MATRICULA))) %>% 
  unite(.,"Matr",c(MATRICULA,PROGRAMA_CODE),sep = "") %>% group_by(Matr)%>% mutate(conteos = row_number()) %>% filter(conteos == 1) %>% ungroup(.)
dinamicos <- read.csv("D:/Users/eescobev/Documents/REPORTES/ESTADOS/Estatus_General_Alumno_PD.csv")
dinamicos <-mutate(dinamicos,CODEP = substr(PROGRAMA,start = 1,stop = 10)) %>% unite(.,"Matr",c(MATRICULA,CODEP),sep="") %>% select(.,Matr,FORMA_ADQUISICION) %>%
  group_by(Matr)%>% mutate(conteos = row_number()) %>% filter(conteos == 1) %>% ungroup(.)
  


diremail("D:/Users/eescobev/Documents/REPORTES", "jvelazhe@utel.edu.mx")
sabana <-leer(c("indicadoresbi", "FRONT$"))
sabana <- mutate(sabana,matricula = as.numeric(matricula))
sabana <- select(sabana,matricula,inicio_plus,`Fecha Inicio`) %>% mutate(mat = matricula) %>% unite(.,"AID",c(mat,inicio_plus),sep=":") %>% group_by(AID) %>% 
  mutate(conteos = row_number()) %>% filter(conteos == 1) %>%   ungroup(.)%>% select(.,-conteos,-AID)

PEAP <- read.csv("D:/Users/eescobev/Documents/REPORTES/ESTADOS/PEAP.csv")
peaploco <- filter(PEAP,Tipo == "Atn_Psicologica") %>% group_by(Matricula) %>% mutate(cuenta = row_number()) %>% filter(.,cuenta == 1) %>% 
  select(.,Matricula,Tipo) %>% rename(.,matricula = Matricula)

peapacademico <- filter(PEAP,Tipo != "Atn_Psicologica") %>% group_by(Matricula) %>% mutate(cuenta = row_number()) %>% filter(.,cuenta == 1) %>% 
  select(.,Matricula,Tipo) %>% rename(.,matricula = Matricula,Tipa = Tipo)
x <- names(heads)

retencion <- read.csv("D:/Users/jvelazhe/Desktop/Licenciaturas/Retencion/Retencion.csv",stringsAsFactors = F) %>% 
  filter(grepl("Proceso",Homologado2)) %>% select(Matricula,Homologado2) %>% rename(matricula = Matricula)


tipisftp <- read_excel("D:/Users/eescobev/Documents/REPORTES/ESTADOS/tipi_ftp.xlsx")




#Lic Utel #####
diremail("//Tut-095/REPORTES", "jvelazhe@utel.edu.mx")
nrlic <-leer(c("niveles_de_riesgo", "FRONT$"))
nrlic <- mutate(nrlic,matricula = as.numeric(matricula))



nrlic[nrlic == "\","]<-" "

nrlic <- mutate(nrlic,Grado = "Licenciatura",mat = matricula) %>%  unite(.,"Matr",c(mat,`Clave programa`),sep="") %>% 
  left_join(.,retencion,by="matricula") %>% 
  mutate_at(.,c("Homologado2"),~replace(.,is.na(.),"Sinproceso")) %>%
  mutate(edomoddle = ifelse(Homologado2 == "Sinproceso", as.character(edomoddle),"Retencion")) %>% select(-Homologado2) %>%
  left_join(tipisftp,by = 'matricula') %>% 
  mutate_at(.,c("tipo"),~replace(.,is.na(.),"Sinproceso")) %>%
  mutate(edomoddle = if_else(tipo == "Sinproceso", as.character(edomoddle),tipo)) %>% 
  mutate(edomoddle = if_else(edomoddle == "Preventivo" & Semaforo != "Alto riesgo académico","Activo",
                             if_else(edomoddle == "Contención" & Semaforo != "Alto riesgo académico","Activo",
                             as.character(edomoddle)))) %>% select(-tipo) %>% 
  left_join(.,alianza_modalidad,by="Matr") %>% left_join(.,reglas_negocios,by="matricula") %>% 
  left_join(.,tablita_reglamx,by="Regla_Negocio") %>% left_join(.,financiero,by="Matr") %>% left_join(.,dinamicos,by="Matr") %>% 
  left_join(.,peaploco,by="matricula") %>% left_join(.,peapacademico,by="matricula") %>% mutate(.,Tipo = as.character(Tipo),Tipa = as.character(Tipa)) %>% 
  mutate_at(.,c("Tipo","Tipa"),~replace(.,is.na(.),"")) %>% unite(.,"PEAP",c(Tipo,Tipa),sep = "") %>% left_join(.,tablita_peaps,by="PEAP") %>% 
  left_join(.,sabana,by="matricula") %>% 
  select(.,matricula,`Bloque seguimiento`,edomoddle,NR,Nombre,Apellidos,Correo,`Correo alternativo`,Telefono,documentos,edoescolares,
         SALDO_VENCIDO,MORA,`bloque ingreso`,Jornada,Semaforo,Acesso,Tutor,Supervisor,`Materias Reprobadas`,Facultad,Homologado,Grado,
         Alianza,AVANCE_CURRICULAR,Modalidad,Peap_S,FORMA_ADQUISICION,`Fecha Inicio`) %>% mutate(FORMA_ADQUISICION = gsub(",","",FORMA_ADQUISICION)) %>% 
  mutate(FORMA_ADQUISICION = stri_trim(FORMA_ADQUISICION),FORMA_ADQUISICION=if_else(FORMA_ADQUISICION == "","ONLINE",FORMA_ADQUISICION)) %>% `colnames<-`(x) %>% 
    mutate_at(.,c("Bloque seguimiento","Estado en Moodle","Nivel de Riesgo","Nombre","Apellidos",
                  "Correo","Correo alternativo","documentos","Estatus Escolares","Bloque ingreso",
                  "Jornada","Semáforo","Tutor","Supervisor","Facultad","Programa de Estudios","Grado",
                  "Línea de negocio","Avance curricular","Modalidad","Servicios PEAP","Alianzas"),~replace(.,is.na(.),"")) %>%
  mutate_at(.,c("Matricula","Telefono","Adeudo","Mora","Materias Reprobadas"),~replace(.,is.na(.),"")) %>% ungroup(.) %>% 
  mutate(Telefono=gsub("Sin registro"," ",Telefono),Adeudo = gsub("Sin registro","",Adeudo),Telefono=gsub(0,"",Telefono),
`Correo alternativo` = gsub(",","",`Correo alternativo`),Apellidos = gsub(",","",Apellidos),`Fecha inicio bimestre`=gsub("NA","",`Fecha inicio bimestre`),
digitos = nchar(Matricula),matri = if_else(digitos == 8,"0","")) %>% unite("Matricula",c(matri,Matricula),sep="") %>% select(-digitos)

nrlic[nrlic == "Sin registro"]<-""
nrlic[nrlic == "Sin Registro"]<-""
nrlic[nrlic == ","]<-";"




online <- nrlic %>% mutate(ordenar = xtfrm(`Fecha inicio bimestre`)) %>% 
  arrange(desc(ordenar)) %>%  group_by(Matricula) %>% mutate(cuenta= row_number()) %>% filter(cuenta == 1) %>% select(-ordenar,-cuenta)

online <- online[!duplicated(online),]
paises <- list("Mex_","Col_","Per_","Chl_","Ecu_","Arg_","Dom_","Gua_","Pan_","Sal_","Bol_","Uru_","Usa_","Espa_","Para_","Inter_")


Mex_  <- filter(online,`Línea de negocio` == "UTEL")
Col_ <- filter(online,`Línea de negocio` == "UTEL Colombia")
Per_ <- filter(online,`Línea de negocio` == "UTEL Perú")
Chl_ <- filter(online,`Línea de negocio` == "UTEL Chile")
Ecu_ <- filter(online,`Línea de negocio` == "UTEL Ecuador")
Arg_ <- filter(online,`Línea de negocio` == "UTEL Argentina")
Dom_ <- filter(online,`Línea de negocio` == "UTEL Dominicana")
Gua_ <- filter(online,`Línea de negocio` == "UTEL Guatemala")
Pan_ <- filter(online,`Línea de negocio` == "UTEL Panama")
Sal_ <- filter(online,`Línea de negocio` == "UTEL El Salvador")
Bol_ <- filter(online,`Línea de negocio` == "UTEL Bolivia")
Uru_ <- filter(online,`Línea de negocio` == "UTEL Uruguay")
Usa_ <- filter(online,`Línea de negocio` == "UTEL USA")
Espa_ <- filter(online,`Línea de negocio` == "UTEL España")
Para_ <- filter(online,`Línea de negocio` == "UTEL Paraguay")
Inter_ <- filter(online,`Línea de negocio` == "UTEL Internacional")


url1 <- c("D:/Users/jvelazhe/Desktop/Licenciaturas/Inconcert/upload/")
url1b <- c("G:/Mi unidad/levels_of_risk/")
dia <- Sys.Date()
url2 <- c(".csv")
for(p in paises){
  url3 <- paste(url1,p,dia,url2,sep="")
  df <- eval(parse(text = p))
  write.table(df, file=url3, sep=",",row.names = F,quote = F) 
}
for(p in paises){
  url3 <- paste(url1b,p,dia,url2,sep="")
  df <- eval(parse(text = p))
  write.table(df, file=url3, sep=",",row.names = F,quote = F) 
}


source("D:/Users/jvelazhe/Desktop/R/inconcertposgrados.R")





shell("D://Users/jvelazhe/Desktop/Development/Python_BI/sftpconetsion.py")
# 
# 

carga <- list.files ("D:/Users/jvelazhe/Desktop/Licenciaturas/Inconcert/upload/subir", pattern = "*.csv", full.names = TRUE)
archivos = length(carga)
if(archivos == 0)
  archivos == 1

for (i in 1:archivos) {

  file.remove(carga[i])
}
print('Se realizo la carga de archivos con exito')


carga <- list.files ("D:/Users/jvelazhe/Desktop/Licenciaturas/Inconcert/upload", pattern = "*.csv", full.names = TRUE)
archivos = length(carga)
if(archivos == 0)
  archivos == 1

for (i in 1:archivos) {
  
  file.remove(carga[i])
}
print('Se realizo la carga de archivos con exito')




# #Posgra Utel #####
# nrposgra <- read.csv("D:/Users/eescobev/Documents/REPORTES/FRONT/niveles_de_riesgoMAE.csv")
# nrposgra <- mutate(nrposgra,mat = Matricula) %>% rename(matricula = Matricula) %>% 
#   unite(.,"Matr",c(mat,Clave.Programa),sep="") %>% 
#   left_join(.,alianzaposgra,by="Matr") %>% left_join(.,reglas_negocios,by="matricula") %>% 
#   left_join(.,tablita_reglamx,by="Regla_Negocio") %>% left_join(.,financiero,by="Matr") %>% left_join(.,dinamicos,by="Matr") %>% 
#   left_join(.,peaploco,by="matricula") %>% left_join(.,peapacademico,by="matricula") %>% mutate(.,Tipo = as.character(Tipo),Tipa = as.character(Tipa)) %>% 
#   mutate_at(.,c("Tipo","Tipa"),~replace(.,is.na(.),"")) %>% unite(.,"PEAP",c(Tipo,Tipa),sep = ",") %>% left_join(.,tablita_peaps,by="PEAP") %>% 
#   left_join(.,sabana,by="matricula") %>% mutate(Correo.alternativo = "-") %>% 
#   select(.,matricula,Bloque,edo.moodle,NR,Nombre,Apellidos,Correo,Correo.alternativo,Telefono,documentos,Estado.Banner,
#          SALDO_VENCIDO,MORA,Bloque.Ingreso,Jornada,Semaforo,Acc.Mater?a,Tutor,Supervisor,Mat.Reprobadas,Facultad,Homologado,Modalidad.y,
#          Alianza,AVANCE_CURRICULAR,Modalidad,Peap_S,FORMA_ADQUISICION,Ciclo) %>% mutate(FORMA_ADQUISICION = gsub(",","",FORMA_ADQUISICION)) %>% 
#     mutate(FORMA_ADQUISICION = stri_trim(FORMA_ADQUISICION),FORMA_ADQUISICION=if_else(FORMA_ADQUISICION == "","ONLINE",FORMA_ADQUISICION))
#   
# x <- names(heads)
# colnames(nrposgra) <- x
# write.csv(nrposgra,file="D:/Users/jvelazhe/Desktop/Licenciaturas/Inconcert/nrposgra.csv",row.names = F)
# 
# inconertutel <- rbind(nrlic,nrposgra)
# write.csv(inconertutel,file="D:/Users/jvelazhe/Desktop/Licenciaturas/Inconcert/inconcert_utel.csv",row.names = F)
# 
