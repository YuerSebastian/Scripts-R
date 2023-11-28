library(RMySQL)
library(DBI)
library(dplyr)

source("D://Users/jvelazhe/Desktop/R/IDcategory.R")

source("D://Users/jvelazhe/Desktop/R/Consultas_MySQL.R")




#### Reporte Completo#####
cons <- dbListConnections(MySQL())
for(con in cons)dbDisconnect(con)

cruces= c('completo[,5] <- as.numeric(as.character(completo[,5]))
completo[,13] <- as.numeric(as.character(completo[,13]))
completo$calificacion[is.na(completo$calificacion)]<- 0
modalida[,4] <- as.numeric(as.character(modalida[,4]))
modalida <- dplyr::select(modalida,matricula,clave,modalidad)
modalida <- tidyr::unite(modalida,"Con",c(matricula,clave),sep="_")
completo$matr <- completo$matricula
completo$clav <- completo$clave
completo <- tidyr::unite(completo,"Con",c(matr,clav),sep="_")  
completo <- dplyr::left_join(completo,modalida,by="Con")
completo$modalidad[is.na(completo$modalidad)]<-"Sin Modalidad"')

try(for (i in 1:10) {
  conetsion <- paste("c",i,sep="")
  conetsion <- eval(parse(text = conetsion))
  idcat <- paste("ID_Category",i,sep = "")
  idcat <- eval(parse(text = idcat))
  conexion <- paste(a,conetsion,sep="")
  conexion <- eval(parse(text = conexion))
  reportecompleto=paste(queryallLic1,idcat,queryallLic2,idcat,sep=" ")
  completo<- dbGetQuery(conexion,statement=reportecompleto)
  modeva1<-paste(ModEva,idcat,sep=" ")
  modalida<- dbGetQuery(conexion,statement=modeva1)
  cruz <- paste("cruces",i,sep = "")
  cruz <- eval(parse(text = cruces))
  aula <- ifelse(i < 10,paste("Aula","0",i,sep=""),paste("Aula",i,sep=""))
  completo$aula <- aula
  
  if(i==1) reporte_completo <- completo else reporte_completo <- rbind(reporte_completo,completo)
  cons <- dbListConnections(MySQL())
  for(con in cons)dbDisconnect(con)
  
})
reporte_completo <- subset(reporte_completo,grupo != "Grupo_O")
reporte_completo <- subset(reporte_completo,grupo != "Grupo_S")
reporte_completo$Nom <- reporte_completo$Nombre
reporte_completo$Apell <- reporte_completo$Apellidos
reporte_completo <- tidyr::unite(reporte_completo,"Nombre.Alumno",c(Nom,Apell),sep=" ")
reporte_completo <- dplyr::select(reporte_completo,status_plataforma,status,institucion,id,matricula,ciudad,Nombre,Apellidos,Nombre.Alumno,correo,materia_id,clave,
                                 asignatura,grupo,modalidad,calificacion,ultimo_acceso_m,ultimo_acceso_auvi,aula,fecha_inicio,fecha_fin,rol)
reporte_completo <- reporte_completo[!is.na(reporte_completo$matricula),]
write.csv(reporte_completo, file = "G:/Mi unidad/Reports/reporte_completo_Lic.csv", row.names = F)

#### Reporte Completo Maestrias#####
cruces= c('completo$aula <- "Aula14"
completo[,4] <- as.numeric(as.character(completo[,4]))
completo[,11] <- as.numeric(as.character(completo[,11]))
completo$calificacion[is.na(completo$calificacion)]<- 0
aula14<- completo')

try(for (i in 12:14) {
  conetsion <- paste("c",i,sep="")
  conetsion <- eval(parse(text = conetsion))
  idcat <- paste("ID_Category",i,sep = "")
  idcat <- eval(parse(text = idcat))
  conexion <- paste(a,conetsion,sep="")
  conexion <- eval(parse(text = conexion))
  queryallmae <- ifelse(i < 13,"queryallmae1","queryallmae2")
  queryallmae <- eval(parse(text = queryallmae))
  reportecompleto=paste(queryallmae,idcat,sep=" ")
  completo<- dbGetQuery(conexion,statement=reportecompleto)
  cruz <- paste("cruces",i,sep = "")
  cruz <- eval(parse(text = cruces))
  aula <- ifelse(i < 10,paste("Aula","0",i,sep=""),paste("Aula",i,sep=""))
  completo$aula <- aula
  
  if(i==12) Posgrados <- completo else Posgrados <- rbind(Posgrados,completo)
  cons <- dbListConnections(MySQL())
  for(con in cons)dbDisconnect(con)
  })
Posgrados$clavel <- Posgrados$clave
Posgrados <- tidyr::separate(Posgrados,clavel,c("ciclo","b","c","d"))
Posgrados$b <- NULL
Posgrados$c <- NULL
Posgrados$d <- NULL

nivel <-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/nivel_maestrias.csv", header = TRUE,sep = ",",encoding="UTF-7")
Posgrados <- dplyr::left_join(Posgrados,nivel,by="ciclo")
Posgrados$ciclo <- NULL
Posgrados[,20] <- as.character(as.character(Posgrados[,20]))
Posgrados$Nivel[is.na(Posgrados$Nivel)]<-"Sin_Definir"
names(Posgrados)[20]="nivel"
Posgrados <- dplyr::select(Posgrados,status_plataforma,status,institucion,nivel,matricula,Nombre,Apellidos,correo,materia_id,clave,grupo,
                           calificacion,rol,asignatura,`Fecha enrolado`,`Fecha mdf enrol`,ultimo_acceso_m,`fecha add grupo`,ultimo_acceso_auvi,aula)
Posgrados <- subset(Posgrados,grupo != "Grupo_O")
Posgrados <- subset(Posgrados,grupo != "Grupo_S")
write.csv(Posgrados, file = "G:/Mi unidad/Reports/reporte_completo_Mae.csv", row.names = F)


source("D://Users/jvelazhe/Desktop/R/docencio.R")

