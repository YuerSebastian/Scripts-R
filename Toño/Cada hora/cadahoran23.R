library(RMySQL)
library(DBI)
library(dplyr)
library(MultiLEDS)
library(readxl)
library(tidyverse)



source("D://Users/jvelazhe/Desktop/R/IDcategoryn23.R")

source("D://Users/jvelazhe/Desktop/R/Consultas_MySQLn.R")

source("D://Users/jvelazhe/Desktop/R/reportescompletos.R")
source("D://Users/jvelazhe/Desktop/R/reportescompletosposgra.R")


# #### Reporte Completo#####
# cons <- dbListConnections(MySQL())
# for(con in cons)dbDisconnect(con)
# 
# cruces= c('completo[,5] <- as.numeric(as.character(completo[,5]))
# completo[,13] <- as.numeric(as.character(completo[,13]))
# completo$calificacion[is.na(completo$calificacion)]<- 0
# modalida[,4] <- as.numeric(as.character(modalida[,4]))
# modalida <- dplyr::select(modalida,matricula,clave,modalidad)
# modalida <- tidyr::unite(modalida,"Con",c(matricula,clave),sep="_")
# completo$matr <- completo$matricula
# completo$clav <- completo$clave
# completo <- tidyr::unite(completo,"Con",c(matr,clav),sep="_")  
# completo <- dplyr::left_join(completo,modalida,by="Con")
# completo$modalidad[is.na(completo$modalidad)]<-"Sin Modalidad"')
# 
# reporte_completo <- NULL  # Inicializa la variable fuera del bucle
# 
# 
# try({for (i in 1:10) {
#   conetsion <- paste("c",i,sep="")
#   conetsion <- get(conetsion)
#   idcat <- paste("ID_Category",i,sep = "")
#   idcat <- eval(parse(text = idcat))
#   conexion <- paste(a,conetsion,sep="")
#   conexion <- eval(parse(text = conexion))
#   reportecompleto=paste(queryallLic1,idcat,queryallLic2,idcat,sep=" ")
#   completo<- dbGetQuery(conexion,statement=reportecompleto)
#   modeva1<-paste(ModEva,idcat,sep=" ")
#   modalida<- dbGetQuery(conexion,statement=modeva1)
#   cruz <- paste("cruces",i,sep = "")
#   cruz <- eval(parse(text = cruces))
#   aula <- ifelse(i < 10,paste("Aula","0",i,sep=""),paste("Aula",i,sep=""))
#   completo$aula <- aula
#   
#   if(i==1) reporte_completo <- completo else reporte_completo <- rbind(reporte_completo,completo)
#   cons <- dbListConnections(MySQL())
#   for(con in cons)dbDisconnect(con)
# }
# })
# reporte_completo <- subset(reporte_completo,grupo != "Grupo_O")
# reporte_completo <- subset(reporte_completo,grupo != "Grupo_S")
# reporte_completo$Nom <- reporte_completo$Nombre
# reporte_completo$Apell <- reporte_completo$Apellidos
# reporte_completo <- tidyr::unite(reporte_completo,"Nombre.Alumno",c(Nom,Apell),sep=" ")
# reporte_completo <- reporte_completo[!is.na(reporte_completo$matricula),]
# reportecompleto <- rbind(reporte_completo)
# reportecompleto <- reportecompleto %>% filter(!grepl("23_AC_I_PD",clave))
# 
# enrolados <- reporte_completo %>% select(status_plataforma,status,matricula,clave,grupo,`Fecha enrolado`,ultimo_acceso_m,aula)
# reportecompleto <- dplyr::select(reportecompleto,status_plataforma,status,institucion,id,matricula,ciudad,Nombre,Apellidos,Nombre.Alumno,correo,materia_id,clave,
#                                   asignatura,grupo,modalidad,calificacion,ultimo_acceso_m,ultimo_acceso_auvi,aula,fecha_inicio,fecha_fin,rol)
# 
# 
# write.csv(enrolados, file = "G:/Mi unidad/Reports/enrolados.csv", row.names = F,fileEncoding = "latin1" )
# 
# write.csv(reportecompleto, file = "G:/Mi unidad/Reports/reporte_completo_Lic.csv", row.names = F,fileEncoding = "latin1" )
# 

# 
# 
# #### Reporte Completo Maestrias#####
# cruces= c('completo$aula <- "Aula14"
# completo[,4] <- as.numeric(as.character(completo[,4]))
# completo[,11] <- as.numeric(as.character(completo[,11]))
# completo$calificacion[is.na(completo$calificacion)]<- 0
# aula14<- completo')
# 
# try(for (i in 11:15) {
#   conetsion <- paste("c",i,sep="")
#   conetsion <- eval(parse(text = conetsion))
#   idcat <- paste("ID_Category",i,sep = "")
#   idcat <- eval(parse(text = idcat))
#   conexion <- paste(a,conetsion,sep="")
#   conexion <- eval(parse(text = conexion))
#   queryallmae <- ifelse(i < 10,"queryallmae1","queryallmae2")
#   queryallmae <- eval(parse(text = queryallmae))
#   reportecompleto=paste(queryallmae,idcat,sep=" ")
#   completo<- dbGetQuery(conexion,statement=reportecompleto)
#   cruz <- paste("cruces",i,sep = "")
#   cruz <- eval(parse(text = cruces))
#   aula <- ifelse(i < 10,paste("Aula","0",i,sep=""),paste("Aula",i,sep=""))
#   completo$aula <- aula
#   
#   if(i==11) Posgrados <- completo else Posgrados <- rbind(Posgrados,completo)
#   cons <- dbListConnections(MySQL())
#   for(con in cons)dbDisconnect(con)
#   })
# Posgrados$clavel <- Posgrados$clave
# Posgrados <- tidyr::separate(Posgrados,clavel,c("ciclo","b","c","d"))
# Posgrados$b <- NULL
# Posgrados$c <- NULL
# Posgrados$d <- NULL
# 
# 
# catalogoalianza <- read_xlsx("G:/Mi unidad/Reports/CatalgoAlianzas.xlsx")
# nivel <-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/nivel_maestrias.csv", header = TRUE,sep = ",",fileEncoding = "latin1")
# Posgrados <- dplyr::left_join(Posgrados,nivel,by="ciclo")
# Posgrados$ciclo <- NULL
# Posgrados[,20] <- as.character(as.character(Posgrados[,20]))
# Posgrados$Nivel[is.na(Posgrados$nivel)]<-"Sin_Definir"
# names(Posgrados)[20]="nivel"
# Posgrados <- dplyr::select(Posgrados,status_plataforma,status,institucion,nivel,matricula,Nombre,Apellidos,correo,materia_id,clave,grupo,
#                            calificacion,rol,asignatura,`Fecha enrolado`,`Fecha mdf enrol`,ultimo_acceso_m,`fecha add grupo`,ultimo_acceso_auvi,aula)
# Posgrados <- subset(Posgrados,grupo != "Grupo_O")
# Posgrados <- subset(Posgrados,grupo != "Grupo_S")
# 
# Posgrados <- Posgrados %>% mutate(claves =sub(".+_", "",clave)) %>% left_join(catalogoalianza,by="claves") %>% 
#   mutate(Inst = replace_na(Inst,"Curricular")) %>% select(-claves) %>% filter(!is.na(matricula))
# 
# 
# write.csv(Posgrados, file = "G:/Mi unidad/Reports/reporte_completo_Mae.csv", row.names = F,fileEncoding = "latin1")
# 
# # source("D://Users/jvelazhe/Desktop/R/cadahoran23nuevobim.R")

# 
# 
# ### Activities ####
# 
# cruces= c('completo[,1] <- as.numeric(as.character(completo[,1]))
#           completo[,13] <- as.numeric(as.character(completo[,13]))
#           completo$num_archivos[is.na(completo$num_archivos)]<-0')
# 
# 
# try(for (i in 1:10) {
#   conetsion <- paste("c",i,sep="")
#   conetsion <- eval(parse(text = conetsion))
#   idcat <- paste("ID_Category",i,sep = "")
#   idcat <- eval(parse(text = idcat))
#   conexion <- paste(a,conetsion,sep="")
#   conexion <- eval(parse(text = conexion))
#   reportecompleto=paste(Queryactivities,idcat,sep=" ")
#   completo<- dbGetQuery(conexion,statement=reportecompleto)
#   cruz <- paste("cruces",i,sep = "")
#   cruz <- eval(parse(text = cruces))
#   aula <- ifelse(i < 10,paste("Aula","0",i,sep=""),paste("Aula",i,sep=""))
#   completo$aula <- aula
#   
#   if(i==1) ActivitiesLic <- completo else ActivitiesLic <- rbind(ActivitiesLic,completo)
#   cons <- dbListConnections(MySQL())
#   for(con in cons)dbDisconnect(con)
#   
# })
# 
# ActivitiesLic <- rbind(ActivitiesLic)
# 
# 
# write.csv(ActivitiesLic, file = "G:/Mi unidad/Reports/Actividades_Lic.csv", row.names = F,fileEncoding = "UTF-8")
# source("D://Users/jvelazhe/Desktop/R/actividades_docentes.R")
# 
# # ### Activities Maestr?as####
# 
# cruces= c('completo[,2] <- as.numeric(as.character(completo[,2]))
# completo[,11] <- as.numeric(as.character(completo[,11]))
# completo$calificacion[is.na(completo$calificacion)]<- 0
# completo[,14] <- as.numeric(as.character(completo[,14]))
# completo$num_archivos[is.na(completo$num_archivos)]<- 0')
# 
# try(for (i in 11:14) {
#   conetsion <- paste("c",i,sep="")
#   conetsion <- eval(parse(text = conetsion))
#   idcat <- paste("ID_Category",i,sep = "")
#   idcat <- eval(parse(text = idcat))
#   conexion <- paste(a,conetsion,sep="")
#   conexion <- eval(parse(text = conexion))
#   queryact <- ifelse(i < 10,"QueryactivitiesM","QueryactivitiesM14")
#   queryact <- eval(parse(text = queryact))
#   reportecompleto=paste(queryact,idcat,sep=" ")
#   completo<- dbGetQuery(conexion,statement=reportecompleto)
#   cruz <- paste("cruces",i,sep = "")
#   cruz <- eval(parse(text = cruces))
#   aula <- ifelse(i < 10,paste("Aula","0",i,sep=""),paste("Aula",i,sep=""))
#   completo$aula <- aula
#   
#   if(i==11) ActivitiesM <- completo else ActivitiesM <- rbind(ActivitiesM,completo)
#   cons <- dbListConnections(MySQL())
#   for(con in cons)dbDisconnect(con)
# })
# 
# ActivitiesM <- subset(ActivitiesM,grupo != "Grupo_O")
# ActivitiesM <- subset(ActivitiesM,grupo != "Grupo_S")
# 
# write.csv(ActivitiesM, file = "G:/Mi unidad/Reports/Activities_Mae.csv", row.names = F,fileEncoding = "UTF-8")
# 
# source("D://Users/jvelazhe/Desktop/R/actividades_docentesP.R")
# 
# 
# 
# # ### Calificacion Examenes Maestr?as #####
# 
# cruces= c('completo[,1] <- as.numeric(as.character(completo[,1]))
# completo[,12] <- as.numeric(as.character(completo[,12]))
# completo$calificacion[is.na(completo$calificacion)]<- -1')
# 
# ##Aula11##
# conexion <- paste(a,c11,sep="")
# conexion <- eval(parse(text = conexion))
# reportecompleto=paste(querycalexamae14,ID_Category11,sep=" ")
# completo<- dbGetQuery(conexion,statement=reportecompleto)
# cons <- dbListConnections(MySQL())
# for(con in cons)dbDisconnect(con)
# 
# cruces1 <- eval(parse(text = cruces))
# completo$aula <- "Aula11"
# aula11 <- completo
# 
# 
# 
# 
# 
# 
# 
# ##Aula12##
# conexion <- paste(a,c12,sep="")
# conexion <- eval(parse(text = conexion))
# reportecompleto=paste(querycalexamae14,ID_Category12,sep=" ")
# completo<- dbGetQuery(conexion,statement=reportecompleto)
# cons <- dbListConnections(MySQL())
# for(con in cons)dbDisconnect(con)
# 
# cruces1 <- eval(parse(text = cruces))
# completo$aula <- "Aula12"
# aula12 <- completo
# 
# #Aula13##
# conexion <- paste(a,c13,sep="")
# conexion <- eval(parse(text = conexion))
# reportecompleto=paste(querycalexamae14,ID_Category13,sep=" ")
# completo<- dbGetQuery(conexion,statement=reportecompleto)
# cons <- dbListConnections(MySQL())
# for(con in cons)dbDisconnect(con)
# 
# 
# cruces1 <- eval(parse(text = cruces))
# completo$aula <- "Aula13"
# aula13 <- completo
# 
# conexion <- paste(a,c14,sep="")
# conexion <- eval(parse(text = conexion))
# reportecompleto=paste(querycalexamae14,ID_Category14,sep=" ")
# completo<- dbGetQuery(conexion,statement=reportecompleto)
# cons <- dbListConnections(MySQL())
# for(con in cons)dbDisconnect(con)
# 
# cruces1 <- eval(parse(text = cruces))
# completo$aula <- "Aula14"
# aula14 <- completo
# 
# 
# 
# ## Generamos Calificacion examenes##
# Cal_exaM <- rbind(aula11,aula12,aula13,aula14)
# 
# Cal_exaM <- subset(Cal_exaM,Grupo != "Grupo_O")
# Cal_exaM <- subset(Cal_exaM,Grupo != "Grupo_S")
# 
# write.csv(Cal_exaM, file = "G:/Mi unidad/Reports/cal_exas_Mae.csv", row.names = F,fileEncoding = "UTF-8")
# 
# 
# 
# 
# 
# 
# 
# # source("D://Users/jvelazhe/Desktop/R/docencio.R")
# 
