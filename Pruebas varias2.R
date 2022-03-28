library(dplyr)#library(googledrive,include.only = c("drive_auth","drive_upload"))
library(tidyr)
library(lubridate)
library(stringi)
library(readr)
library(readxl)
library(googlesheets4)
library(googledrive)
library(reshape2)
#Variables principales
gs4_auth("jsalinba@utel.edu.mx")
drive_auth("jsalinba@utel.edu.mx")
c <- list.dirs("D:/Trabajo")
c <- c(c,"1HJJyzyAWIz7TxqzEOFsAxmVBmgaMZpl_Jp9Nv3JmDbc") #Se añade al último solo si se requiere una sheet de google
#Funciones
esc_gsheet <- function(base,ss,hoja){
  range_clear(ss,hoja,reformat = F); sheet_resize(ss,hoja,2,2,T)
  range_write(ss,base,hoja,reformat = F); range_autofit(ss,hoja)
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
#Cert y Tit (Contactación)
Base <- leer_arch(c,"gsheet",sheet="Cert y Tit",range="A2:M") %>% mutate(Fecha=parse_date_time(Fecha,c("%m/%Y")))%>%
  melt(id.vars="Fecha",measure.vars=2:length(.),value.name="Totales",variable.name="Motivo") %>% arrange(desc(Fecha))%>%
  mutate(`Tipo Contacto`=if_else(Motivo=="Remarcaciones","Remarcaciónes","Contactadas"))
Base <- Base[c("Fecha","Motivo","Tipo Contacto","Totales")]

# Base <- read_excel(paste(c[grep("Trabajo$",c)],"Info general.xlsx",sep="/"),sheet = 3)
# 
# esc_gsheet(Base,"1pBrYvy9yiu9hgheFrDkeGcd4QsvvVpiysq24ga6uYzE","Nuevos")
# 
# drive_upload(paste(c[grep("Escolares$",c)],"Materias_Predictamen.csv",sep="/"),"Cosa/",type = "csv",overwrite = T)


