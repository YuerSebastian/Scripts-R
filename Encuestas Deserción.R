library(readr);library(lubridate);library(dplyr);library(tidyr); library(googledrive); library(googlesheets4)
#Variables
c <- list.dirs("D:/Trabajo")
drive_auth("jsalinba@utel.edu.mx"); gs4_auth("jsalinba@utel.edu.mx")
#Funciones
leer_drive <- function(nom,hoja=NULL,...){
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

Base <- range_speedread("1f4ywqZMreDOjYkUve4j5uIs_7e5f0Datzb4gUlZ1kOM",col_types = cols(.default = "c"))
Rep <- leer_drive("Info General","Orden Base",range = "Y2:AB")
Base <- `colnames<-`(Base,Rep$Nuevo) %>% mutate(Fecha=dmy_hms(Fecha))
Base <- mutate(Base,Edad=as.integer(gsub("[[:alpha:][:punct:]\\pS]","",Edad)),
               `Rango Edad`=if_else(!is.na(Edad),
                                    if_else(Edad<20,"Menores de 20",
                                            if_else(Edad<26,"20 a 25 años",
                                                    if_else(Edad<31,"26 a 30 años","Mayores a 30 años"))),"_"),
               Bimestre=if_else(Avance=="1 al 10 %","B1 y B2",
                                ifelse(Avance=="11 al 25 %","B3",
                                       if_else(Avance=="26 al 50 %","B4 a B6",
                                               if_else(Avance=="51 al 75 %","B7 y B8","B9 o mayor")))))
impr_arch(c,Base,"csv","Encuesta Deserción","auxiliares$")



