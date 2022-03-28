library(dplyr);library(lubridate);library(tidyr);library(googlesheets4);library(readr);library(readxl)
#Variables
c <- list.dirs("D:/Trabajo")
desc <- list.files(c[grep("FLOKZU Global$",c)],full.names = T)
#Funciones
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
for (i in 1:length(desc)) {
  Rep <- read_excel(desc[i],skip=1,col_types="text") %>% reshape::rename(c(`Date when ongoing Task was assigned`="Fecha Tarea",`Student Services Analyst`="Analista SER",
                                                                           `Student email`="Correo",`Prospect email`="Correo"))
  if(!hasName(Rep,"Analista SER")) Rep <- mutate(Rep,`Analista SER`="_")
  if(!hasName(Rep,"Student ID")) Rep <- mutate(Rep,`Student ID`="_")
  if (length(rownames(Rep))!=0) {
    #if(!hasName(Rep,"Matricula")) Rep <- rename(Rep,"Matricula"=SIU);
    if(!hasName(Rep,"Correo")) Rep <- mutate(Rep,Correo="_")
    Rep <- select(Rep,"Identificador"=Identifier,"Matrícula"=`Student ID`,Correo,"Fecha Inicio"=`Initiation Date`,"Finalizado"=Completed,"Fecha Finalización"=`Completion date`,
                  "Tarea Actual"=`Ongoing Task`,`Fecha Tarea`,`Analista SER`)
    if(exists("Base")) Base <- rbind(Base,Rep) else Base <- Rep
  }
}










