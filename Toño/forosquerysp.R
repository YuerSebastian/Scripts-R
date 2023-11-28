library(RMySQL)
library(DBI)

library(dplyr)

###ID Category####

# source("D://Users/jvelazhe/Desktop/R/forossviejos.R")
source("D://Users/jvelazhe/Desktop/R/IDcategoryn23nbim.R")
source("D://Users/jvelazhe/Desktop/R/Consultas_MySQLn.R")

### Foros Presentacion####

cruces= c('completo[,9] <- as.numeric(as.character(completo[,9]))')

#contencion = c11, idcat = ID_Category11, conexion = dbConnect(MySQL(),c(user="jvelazhe",host="162.209.35.80",port=3306,password="68iCQ2deKn",dbname="moodle_aula11n23"))

#reportecompleto = 


try(for (i in 11:13) {
  conetsion <- paste("c",i,sep="")
  conetsion <- eval(parse(text = conetsion))
  idcat <- paste("ID_Category",i,sep = "")
  idcat <- eval(parse(text = idcat))
  conexion <- paste(a,conetsion,sep="")
  conexion <- eval(parse(text = conexion))
  reportecompleto=paste(queryforopresentacionp,idcat,sep=" ")
  completo<- dbGetQuery(conexion,statement=reportecompleto)
  cruz <- paste("cruces",i,sep = "")
  cruz <- eval(parse(text = cruces))
  aula <- ifelse(i < 10,paste("Aula","0",i,sep=""),paste("Aula",i,sep=""))
  completo$aula <- aula
  
  if(i==11) ForosP <- completo else ForosP <- rbind(ForosP,completo)
  cons <- dbListConnections(MySQL())
  for(con in cons)dbDisconnect(con)
  
})

ForosP <- rbind(ForosP)
write.csv(ForosP, file = "G:/Mi unidad/Reports/Foros_Posgrapresentacion.csv", row.names = F,fileEncoding = "UTF-8")


### Foros Consultasycoment####

cruces= c('completo[,9] <- as.numeric(as.character(completo[,9]))')


try(for (i in 1:10) {
  conetsion <- paste("c",i,sep="")
  conetsion <- eval(parse(text = conetsion))
  idcat <- paste("ID_Category",i,sep = "")
  idcat <- eval(parse(text = idcat))
  conexion <- paste(a,conetsion,sep="")
  conexion <- eval(parse(text = conexion))
  reportecompleto=paste(queryforoconsultas,idcat,sep=" ")
  completo<- dbGetQuery(conexion,statement=reportecompleto)
  cruz <- paste("cruces",i,sep = "")
  cruz <- eval(parse(text = cruces))
  aula <- ifelse(i < 10,paste("Aula","0",i,sep=""),paste("Aula",i,sep=""))
  completo$aula <- aula
  
  if(i==1) ForosLic <- completo else ForosLic <- rbind(ForosLic,completo)
  cons <- dbListConnections(MySQL())
  for(con in cons)dbDisconnect(con)
  
})

ForosLic <- rbind(ForosLic)
write.csv(ForosLic, file = "G:/Mi unidad/Reports/Foros_Licconsultas.csv", row.names = F,fileEncoding = "UTF-8")
