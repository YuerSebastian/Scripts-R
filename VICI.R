library(MultiLEDS);library(dplyr);library(lubridate);library(tidyr);library(readr);library(readxl)
#Variables principales.
c <- list.dirs("D:/Trabajo")
#Funciones
leer_drive <- function(nom,hoja=NULL,...){
  library(googledrive); library(googlesheets4); drive_auth("jsalinba@utel.edu.mx"); gs4_auth("jsalinba@utel.edu.mx")
  y <- drive_find(nom,type = "spreadsheet")
  y <- as.character(y$id)
  x <- range_speedread(y,col_types = cols(.default = "c"),sheet=hoja,...)
  return(x)
}
leer_arch <- function(c,tipo="csv",arch=NULL,pat=NULL,...){
  #Resume la lectura que se usa siempre para los reportes, se pueden usar opciones adicionales de las funciones internas.
  if (tipo=="csv") {
    x <- read_csv(paste(c[grep(pat,c)],paste(arch,tipo,sep = "."),sep = "/"),locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c"),...)
  }else if (tipo=="xlsx"){
    x <- read_excel(paste(c[grep(pat,c)],paste(arch,tipo,sep = "."),sep = "/"),col_types = "text",...)
  }else if (tipo=="txt"){
    x <- read_delim(paste(c[grep(pat,c)],paste(arch,tipo,sep = "."),sep = "/"),locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c"),...)
  }else if (tipo=="gsheet"){
    x <- range_speedread(c[length(c)],col_types = cols(.default = "c"),...)
  }else{x <- "Tipo incorrecto."}
  return(x)
}
impr_arch <- function(c,x,tipo="csv",arch=NULL,pat=NULL,...){
  write.csv(x,paste(c[grep(pat,c)],paste(arch,tipo,sep="."),sep="/"),row.names = F,...)
}
#Inicio
Base <- leer_arch(c,"txt","VICI","VICI$",delim="\t")
Rep <- leer_drive("Info General","VICI",range="A2:C") %>% rename("user"=Usuario)
Base <- left_join(Base,Rep,by="user")
Rep <- leer_drive("Info General","VICI",range="D2:E") %>% rename("status"=`Clave Estado`)
Base <- left_join(Base,Rep,by="status")
Rep <- leer_drive("Info General","VICI",range="F2:G") %>% rename("campaign_id"=`Clave Campaña`)
Base <- left_join(Base,Rep,by="campaign_id")
Base <- mutate(Base,call_date=ymd_hms(call_date)) %>% arrange(desc(call_date))
Rep <- count(Base,lead_id,name = "Llamadas Totales")
Base <- left_join(Base,Rep,by="lead_id")

Rep <- leer_arch(c,"xlsx","Orden base","Trabajo$",sheet="VICI")
Base <- `colnames<-`(Base,Rep$Nuevo)
Rep <- Rep$Incluir[!is.na(Rep$Incluir)]
Base <- Base[Rep] %>% mutate_all(~as.character(.)) %>% mutate_all(~replace_na(.,"_"))

impr_arch(c,Base,"csv","Registros VICI","Principales$")
