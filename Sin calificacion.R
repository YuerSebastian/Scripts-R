library(dplyr); library(tidyr) ;library(lubridate); library(stringr); library(readr); library(readxl); library(googledrive); library(googlesheets4);
#Variables principales.
drive_auth("jsalinba@utel.edu.mx"); gs4_auth("jsalinba@utel.edu.mx")
c <- list.dirs("D:/Trabajo")
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
Rep <- leer_drive("Info General","General",range="F2:H") %>% rename("CAMPUS"=Campus)
Mat <- leer_arch(c,"csv","Materias_Inscritas_PPDP","Escolares$") %>% mutate_all(~replace_na(.,"_"))%>%
  left_join(Rep,by="CAMPUS") %>% filter(`Tipo Campus`!="OPM") %>% unite(MATPROGMATE,MATRICULA,CODE_PROG,MATERIA,remove=F) %>% mutate(FECHA_INICIO=dmy(FECHA_INICIO))%>%
  arrange(desc(FECHA_INICIO)) %>% filter(!duplicated(MATPROGMATE))
# #Fechas fin
# x <- read_excel(paste(c_I,"Info_general.xlsx",sep="/"),sheet="Banner")
# Rep <- read_csv(paste(c_D,"Forma de fechas de inicio.csv",sep="/"),col_names=F,col_types=cols(X12="_",X13="_",X14="_")) %>% `colnames<-`(x$Nombre_columnas)%>%
#   mutate(perido=if_else(nchar(perido)==5,paste("0",perido,sep=""),perido)) %>% unite(PROG_PER_PAPER,program,perido,`parte de periodo`,remove=F)%>%
#   arrange(desc(perido),desc(`fecha inicio`)) %>% mutate(dup=if_else(duplicated(PROG_PER_PAPER),"si","no")) %>% filter(dup=="no")%>%
#   select(PROG_PER_PAPER,`fecha inicio`,`fecha fin`)
# 
# 
# Rep <- mutate(Rep,Fecha_inicio=gsub("AGO","AUG",Fecha_inicio))
# 
# 
# 
# Rep <- mutate(Rep,cosa=dmy(Fecha_inicio,locale = "Spanish_Mexico.1252"))
# 
# 
# 
# Mat <- unite(Mat,PROG_PER_PAPER,CODE_PROG,PERIODO,PARTE_PERIODO,remove=F)
# Mat <- left_join(Mat,Rep,by="PROG_PER_PAPER")
# cont <- Mat[is.na(Mat$`fecha fin`),]
# 
# 
#   #mutate(Rep, actual=if_else(`fecha fin`<Sys.Date(),"si","no"))


#Tip evaluación materia y Tip calificación
Mat <- mutate(Mat,Tip_eval_materia=if_else(grepl("SESO",MATERIA_PADRE),"Servicio Social",
                                           if_else(MATERIA_LEGAL=="M1HB401","Introdicción al aula",
                                                   if_else(str_sub(PERIODO,-2,-2)=="8","Nivelación","Ordinario"))),#Tip materia
              Tip_calificacion=if_else(FECHA_INICIO>"2022-01-03","En curso",
                                       if_else(ESTATUS_MAT!="RE","Baja",
                                               if_else(CALIFICACION!="0","Con calificación","Sin calificación"))))%>%#Tip calificación
  mutate_all(~as.character(.)) %>% mutate_all(~replace(.,is.na(.),"_"))
#Ordenando e imprimiendo
Rep <- leer_arch(c,"xlsx","Orden base","Trabajo$",sheet="Alumnos sin calif")
Rep <- Rep$Incluir
Mat <- Mat[Rep]
impr_arch(c,Mat,"csv","Alumnos sin calificación","Principales$")









