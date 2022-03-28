library(dplyr);library(lubridate);library(tidyr);library(googlesheets4);library(readr);library(readxl)
#Variables principales.
c <- list.dirs("D:/Trabajo")
c <- c(c,"1BrBMTo2j14lTF-Cn9DP6rWtYJFxLW5OlYhWsvnahEJ0") #Se añade al último solo si se requiere una sheet de google
gs4_auth(email = "jsalinba@utel.edu.mx")#Iniciando google
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
#####----------------------------------------------------Inicio----------------------------------------------------#####
Base <- leer_arch(c,"xlsx","-PROSER-","Originales$",skip=1) %>% select(Identificador,"Matrícula"=`Matricula del estudiante`,"Correo"=`Correo del estudiante`,`Fecha Inicio`,
                                                                       Finalizado,`Fecha Finalización`,`Tarea Actual`,"Fecha Tarea"=`Fecha asignación Tarea actual`,
                                                                       `Persona que apoya`,`Area destino`,Tema,`Tipo de ticket`)
#####----------------------------------------------------Cruce base anterior----------------------------------------------------#####
Rep <- leer_arch(c,"csv","Base PROSER","Principales$", lazy = F,col_select = c("Identificador","Responsable","Fecha Asignación"))
t_a <- length(rownames(Rep))#Tamaño de la base anterior
Base <- left_join(Base,Rep,by = "Identificador") %>% mutate(nuevo=if_else(is.na(Responsable),if_else(`Area destino`=="SERVICIOS ESCOLARES","N","x"),"_"),
                                                            `Fecha Asignación`=if_else(nuevo!="x",
                                                                                       if_else(is.na(`Fecha Asignación`),as.character(now()),`Fecha Asignación`),"_"))%>%
  mutate_at(c(grep("^Fecha",names(Base))),~if_else(nchar(.)==16,paste(.,":02",sep=""),.))#Fechas con 0 segundos
#####----------------------------------------------------Asignación----------------------------------------------------#####
Asig <- filter(Base,nuevo=="N")
if (length(rownames(Asig))!=0) {
  Rep <- leer_arch(c,"gsheet",sheet = wday(today(),week_start = 1)) %>% filter(Excepciones != "N") %>% select(Responsable,PROSER) %>% filter(PROSER!="_")
  cont <- count(Base,Responsable) %>% arrange(n)
  x <- left_join(Rep,cont,by = "Responsable") %>% mutate(n=if_else(is.na(n),0L,n)) %>% arrange(n) %>% .[[1]]
  Base <- mutate(Base,Responsable=if_else(is.na(Responsable),if_else(nuevo=="N",rep_len(x,length(rownames(Base))),"_"),Responsable))
}
Base <- select(Base,-nuevo) %>% mutate_all(~replace_na(.,"_"))
#####----------------------------------------------------Re ordenando e imprimiendo----------------------------------------------------#####
Rep <- leer_arch(c,"xlsx","Orden base","Trabajo$",sheet="Ticket PROSER")
Base <- `colnames<-`(Base,Rep$Nuevo) %>% .[c(Rep$Incluir[!is.na(Rep$Incluir)])]
impr_arch(c,Base,"csv","Base PROSER","Principales$")










