library(RMySQL)
library(DBI)

library(tidyverse)

###ID Category####

# source("D://Users/jvelazhe/Desktop/R/forossviejos.R")
source("D://Users/jvelazhe/Desktop/R/IDcategoryn23.R")
source("D://Users/jvelazhe/Desktop/R/Consultas_MySQLn.R")

### Foros####

cruces= c('completo[,9] <- as.numeric(as.character(completo[,9]))')


try(for (i in 11:13) {
  conetsion <- paste("c",i,sep="")
  conetsion <- eval(parse(text = conetsion))
  idcat <- paste("ID_Category",i,sep = "")
  idcat <- eval(parse(text = idcat))
  conexion <- paste(a,conetsion,sep="")
  conexion <- eval(parse(text = conexion))
  reportecompleto=paste(queryforop,idcat,sep=" ")
  completo<- dbGetQuery(conexion,statement=reportecompleto)
  aula <- ifelse(i < 10,paste("Aula","0",i,sep=""),paste("Aula",i,sep=""))
  completo$aula <- aula 
  mdlrating=paste(queryforoprating,sep="")
  rating<- dbGetQuery(conexion,statement=mdlrating)
  rating$aula <- aula
  rating <- rating %>% mutate(itemid=as.character(itemid)) %>% unite("ID",c(aula,itemid),sep="_",remove = T)
  completo <- completo %>% mutate(idrating=as.character(idrating)) %>% unite("ID",c(aula,idrating),sep="_",remove = F) %>% left_join(rating,by = "ID")
  if(i==11) ForosPosgra <- completo else ForosPosgra <- rbind(ForosPosgra,completo)
  cons <- dbListConnections(MySQL())
  for(con in cons)dbDisconnect(con)
  
})



ForosPosgra <- rbind(ForosPosgra)
write.csv(ForosPosgra, file = "G:/Mi unidad/Reports/Foros_posgra.csv", row.names = F,fileEncoding = "UTF-8")

