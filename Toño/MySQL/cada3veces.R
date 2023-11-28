library(RMySQL)
library(DBI)
library(dplyr)

###ID Category####
source("D://Users/jvelazhe/Desktop/R/IDcategory.R")
source("D://Users/jvelazhe/Desktop/R/Consultas_MySQL.R")

### Calificacion Examenes ####

cruces= c('completo[,3] <- as.numeric(as.character(completo[,3]))
completo[,14] <- as.numeric(as.character(completo[,14]))
completo$calificacion[is.na(completo$calificacion)]<- 0')

try(for (i in 1:10) {
  conetsion <- paste("c",i,sep="")
  conetsion <- eval(parse(text = conetsion))
  idcat <- paste("ID_Category",i,sep = "")
  idcat <- eval(parse(text = idcat))
  conexion <- paste(a,conetsion,sep="")
  conexion <- eval(parse(text = conexion))
  reportecompleto=paste(querycalexa,idcat,sep=" ")
  completo<- dbGetQuery(conexion,statement=reportecompleto)
  cruz <- paste("cruces",i,sep = "")
  cruz <- eval(parse(text = cruces))
  aula <- ifelse(i < 10,paste("Aula","0",i,sep=""),paste("Aula",i,sep=""))
  completo$aula <- aula

  if(i==1) Cal_exa <- completo else Cal_exa <- rbind(Cal_exa,completo)
  cons <- dbListConnections(MySQL())
  for(con in cons)dbDisconnect(con)

})
Cal_exa <- subset(Cal_exa,grupo != "Grupo_O")
Cal_exa <- subset(Cal_exa,grupo != "Grupo_S")

write.csv(Cal_exa, file = "G:/Mi unidad/Reports/cal_exas_Lic.csv", row.names = F)

tipo <-googlesheets4::read_sheet(ss="1LVCW2OwMuirnTrEq7E30CQxUQmwaSsOmeq39edoWMZY",range="A1:C5000",col_types = "ccc") %>% 
  select(Clave,Estatus) %>% filter(!is.na(Estatus),!duplicated(Clave)) %>% rename(clave = Clave)

calexas <- Cal_exa %>% select(-grupo) %>% left_join(tipo,by='clave')

calexas <- calexas[!duplicated(calexas),]

write.csv(calexas, file = "G:/Mi unidad/Reports/cal_exas_LicD.csv", row.names = F)


### Activities ####

cruces= c('completo[,1] <- as.numeric(as.character(completo[,1]))
          completo[,13] <- as.numeric(as.character(completo[,13]))
          completo$num_archivos[is.na(completo$num_archivos)]<-0')


try(for (i in 1:10) {
  conetsion <- paste("c",i,sep="")
  conetsion <- eval(parse(text = conetsion))
  idcat <- paste("ID_Category",i,sep = "")
  idcat <- eval(parse(text = idcat))
  conexion <- paste(a,conetsion,sep="")
  conexion <- eval(parse(text = conexion))
  reportecompleto=paste(Queryactivities,idcat,sep=" ")
  completo<- dbGetQuery(conexion,statement=reportecompleto)
  cruz <- paste("cruces",i,sep = "")
  cruz <- eval(parse(text = cruces))
  aula <- ifelse(i < 10,paste("Aula","0",i,sep=""),paste("Aula",i,sep=""))
  completo$aula <- aula

  if(i==1) ActivitiesLic <- completo else ActivitiesLic <- rbind(ActivitiesLic,completo)
  cons <- dbListConnections(MySQL())
  for(con in cons)dbDisconnect(con)

})

write.csv(ActivitiesLic, file = "G:/Mi unidad/Reports/Actividades_Lic.csv", row.names = F)
source("D://Users/jvelazhe/Desktop/R/actividades_docentes.R")




### Acc Profesores####

cruces= c('completo[,4]<-as.numeric(as.character(completo[,4]))')


try(for (i in 1:10) {
  conetsion <- paste("c",i,sep="")
  conetsion <- eval(parse(text = conetsion))
  idcat <- paste("ID_Category",i,sep = "")
  idcat <- eval(parse(text = idcat))
  conexion <- paste(a,conetsion,sep="")
  conexion <- eval(parse(text = conexion))
  reportecompleto=paste(queryaccprof,idcat,accprof2,sep=" ")
  completo<- dbGetQuery(conexion,statement=reportecompleto)
  cruz <- paste("cruces",i,sep = "")
  cruz <- eval(parse(text = cruces))
  aula <- ifelse(i < 10,paste("Aula","0",i,sep=""),paste("Aula",i,sep=""))
  completo$aula <- aula

  if(i==1) accProfesores <- completo else accProfesores <- rbind(accProfesores,completo)
  cons <- dbListConnections(MySQL())
  for(con in cons)dbDisconnect(con)

})

accProfesores <- subset(accProfesores,grupo != "Grupo_O")
accProfesores <- subset(accProfesores,grupo != "Grupo_S")
accProfesores <- accProfesores[!is.na(accProfesores$matricula),]
write.csv(accProfesores, file = "G:/Mi unidad/Reports/Acc_Prof_Lic.csv", row.names = F)





### Acc Profesores Mae####

cruces= c('completo[,3]<-as.numeric(as.character(completo[,3]))')


try(for (i in 12:14) {
  conetsion <- paste("c",i,sep="")
  conetsion <- eval(parse(text = conetsion))
  idcat <- paste("ID_Category",i,sep = "")
  idcat <- eval(parse(text = idcat))
  conexion <- paste(a,conetsion,sep="")
  conexion <- eval(parse(text = conexion))
  queryaccprof <- ifelse(i < 13,"queryaccprofM","queryaccprofM14")
  queryaccprof <- eval(parse(text = queryaccprof))
  reportecompleto=paste(queryaccprof,idcat,accprof2,sep=" ")
  completo<- dbGetQuery(conexion,statement=reportecompleto)
  cruz <- paste("cruces",i,sep = "")
  cruz <- eval(parse(text = cruces))
  aula <- ifelse(i < 10,paste("Aula","0",i,sep=""),paste("Aula",i,sep=""))
  completo$aula <- aula
  
  if(i==12) acccprofM <- completo else acccprofM <- rbind(acccprofM,completo)
  cons <- dbListConnections(MySQL())
  for(con in cons)dbDisconnect(con)
})

acccprofM <- subset(acccprofM,grupo != "Grupo_O")
acccprofM <- subset(acccprofM,grupo != "Grupo_S")
acccprofM<- acccprofM[!is.na(acccprofM$matricula),]
write.csv(acccprofM, file = "G:/Mi unidad/Reports/Acc_Prof_Mae.csv", row.names = F)





### Activities Maestrías####

cruces= c('completo[,2] <- as.numeric(as.character(completo[,2]))
completo[,11] <- as.numeric(as.character(completo[,11]))
completo$calificacion[is.na(completo$calificacion)]<- 0
completo[,14] <- as.numeric(as.character(completo[,14]))
completo$num_archivos[is.na(completo$num_archivos)]<- 0')

try(for (i in 12:14) {
  conetsion <- paste("c",i,sep="")
  conetsion <- eval(parse(text = conetsion))
  idcat <- paste("ID_Category",i,sep = "")
  idcat <- eval(parse(text = idcat))
  conexion <- paste(a,conetsion,sep="")
  conexion <- eval(parse(text = conexion))
  queryact <- ifelse(i < 13,"QueryactivitiesM","QueryactivitiesM14")
  queryact <- eval(parse(text = queryact))
  reportecompleto=paste(queryact,idcat,sep=" ")
  completo<- dbGetQuery(conexion,statement=reportecompleto)
  cruz <- paste("cruces",i,sep = "")
  cruz <- eval(parse(text = cruces))
  aula <- ifelse(i < 10,paste("Aula","0",i,sep=""),paste("Aula",i,sep=""))
  completo$aula <- aula
  
  if(i==12) ActivitiesM <- completo else ActivitiesM <- rbind(ActivitiesM,completo)
  cons <- dbListConnections(MySQL())
  for(con in cons)dbDisconnect(con)
})

ActivitiesM <- subset(ActivitiesM,grupo != "Grupo_O")
ActivitiesM <- subset(ActivitiesM,grupo != "Grupo_S")

write.csv(ActivitiesM, file = "G:/Mi unidad/Reports/Activities_Mae.csv", row.names = F)

source("D://Users/jvelazhe/Desktop/R/actividades_docentesP.R")


### Calificacion Examenes Maestrías #####

cruces= c('completo[,1] <- as.numeric(as.character(completo[,1]))
completo[,12] <- as.numeric(as.character(completo[,12]))
completo$calificacion[is.na(completo$calificacion)]<- -1')


##Aula12##
conexion <- paste(a,c12,sep="")
conexion <- eval(parse(text = conexion))
reportecompleto=paste(querycalexamae,ID_Category12,sep=" ")
completo<- dbGetQuery(conexion,statement=reportecompleto)
cons <- dbListConnections(MySQL())
for(con in cons)dbDisconnect(con)

cruces1 <- eval(parse(text = cruces))
completo$aula <- "Aula12"
aula12 <- completo

# conexion <- paste(a,c13,sep="")
# conexion <- eval(parse(text = conexion))
# reportecompleto=paste(querycalexamae14,ID_Category13,sep=" ")
# completo<- dbGetQuery(conexion,statement=reportecompleto)
# cons <- dbListConnections(MySQL())
# for(con in cons)dbDisconnect(con)

cruces1 <- eval(parse(text = cruces))
completo$aula <- "Aula13"
aula13 <- completo

conexion <- paste(a,c14,sep="")
conexion <- eval(parse(text = conexion))
reportecompleto=paste(querycalexamae14,ID_Category14,sep=" ")
completo<- dbGetQuery(conexion,statement=reportecompleto)
cons <- dbListConnections(MySQL())
for(con in cons)dbDisconnect(con)

cruces1 <- eval(parse(text = cruces))
completo$aula <- "Aula14"
aula14 <- completo



## Generamos Calificacion examenes##
Cal_exaM <- rbind(aula12,aula14)

Cal_exaM <- subset(Cal_exaM,Grupo != "Grupo_O")
Cal_exaM <- subset(Cal_exaM,Grupo != "Grupo_S")

write.csv(Cal_exaM, file = "G:/Mi unidad/Reports/cal_exas_Mae.csv", row.names = F)



evauliaciondocente <- read.csv("G:/Mi unidad/Reports/cal_exas_Lic.csv", stringsAsFactors = F) %>% 
  filter(grepl('Docente',actividad))

write.csv(evauliaciondocente, file = "G:/Mi unidad/Reports/evauliaciondocente.csv", row.names = F)

