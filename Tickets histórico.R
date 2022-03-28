library(dplyr);library(lubridate);library(tidyr);library(readr);library(readxl)
#Variables principales.
c <- list.dirs("D:/Trabajo")
desc <- list.files(c[grep("FLOKZU$",c)],full.names = T)
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
#####----------------------------------------------------Unificando FLOKZUS----------------------------------------------------#####
for (i in 1:length(desc)) {
  Rep <- read_excel(desc[i],skip=1,col_types="text") %>% reshape::rename(c(`Fecha asignación Tarea actual`="Fecha Tarea",`A.analista SER`="Analista SER",
                                                                           `Correo del estudiante`="Correo",`Correo del prospecto`="Correo"))
  if (length(rownames(Rep))!=0) {
    if(!hasName(Rep,"Matricula")) Rep <- rename(Rep,"Matricula"=SIU); if(!hasName(Rep,"Correo")) Rep <- mutate(Rep,Correo="_")
    Rep <- select(Rep,Identificador,"Matrícula"=Matricula,Correo,`Fecha Inicio`,Finalizado,`Fecha Finalización`,`Tarea Actual`,`Fecha Tarea`,`Analista SER`)
    if(exists("Base")) Base <- rbind(Base,Rep) else Base <- Rep
  }
}
Base <- mutate(Base,Proceso=gsub("-[0-9]+","",Identificador)) %>% mutate_all(~stringi::stri_trim(.)) %>% mutate_all(~gsub("\n","",.)) %>% filter(!duplicated(Identificador))%>%
  mutate(`Analista SER`= gsub("Francisco Javier Sandoval Cevantes","Francisco Javier Sandoval Cervantes",`Analista SER`),#Nombres erroneos
         `Analista SER`= gsub("Jocelyn Velarde Hernandez","Jocelyn Velarde Hernández",`Analista SER`))
#####----------------------------------------------------Cruce con histórico anterior----------------------------------------------------#####
Rep <- leer_arch(c,"csv","Tickets histórico","Históricos$",lazy=F,
                 col_select = c("Identificador","Responsable","Responsable Apoyo","EstadoA"=Estado,"Fecha Asignación","FFRA"=`Fecha Fin Real`))
Base <- left_join(Base,Rep,by = "Identificador") %>% filter(!is.na(Responsable))%>%
  mutate_at(c(grep("^Fecha|FFRA",names(Base))),~if_else(nchar(.)==16,paste(.,":02",sep=""),.))#Fechas con 0 segundos
#####----------------------------------------------------Cruce con Tareas (Estado)----------------------------------------------------#####
Rep <- leer_arch(c,"xlsx","Info general","Trabajo$",sheet = "Tareas") %>% mutate_all(~stringi::stri_trim(.)) %>% unique()
Base <- left_join(Base,Rep,by = "Tarea Actual") %>% mutate(Estado = if_else(is.na(Estado),"Sin tip",Estado),
                                                           Estado=if_else(Estado=="Activo",#Regresados
                                                                          if_else(!is.na(EstadoA)&(EstadoA!="Activo"|EstadoA=="Activo (regresado)"),
                                                                                  "Activo (regresado)",Estado),Estado))
#Actualización de fecha a los regresados
Base <- mutate(Base,`Fecha Asignación`=if_else(EstadoA!="Activo" & EstadoA!="Activo (regresado)",
                                               if_else(Estado=="Activo (regresado)",as.character(now()),`Fecha Asignación`),`Fecha Asignación`))
#####----------------------------------------------------Fechas (cálculos)----------------------------------------------------#####
Base <- mutate_all(Base,~replace_na(.,"_")) %>% mutate(`Fecha Fin Real`=if_else(Estado!="Activo" & Estado!="Activo (regresado)" & Estado!="Ayuda",#Fecha Fin Real
                                                                                if_else(`Fecha Finalización`=="_",
                                                                                        `Fecha Tarea`,if_else(FFRA=="_" | FFRA==`Fecha Finalización`,
                                                                                                              `Fecha Finalización`,FFRA)),"_"))%>%
  mutate_at(c(grep("^Fecha",names(.))),~ymd_hms(.))%>%
  mutate(Trabajado=if_else(Estado!="Activo",#Trabajado
                           if_else((`Fecha Asignación` > `Fecha Fin Real`) & (!is.na(`Fecha Fin Real`) & !is.na(`Fecha Asignación`)),"X",
                                   if_else(Responsable==`Analista SER`,"Si",
                                           if_else(`Responsable Apoyo`==`Analista SER`,"Si (Apoyo)","No"))),"_"),
         Días=as.integer(difftime(now(),`Fecha Tarea`,units="days")),Días=if_else(is.na(Días) | !is.na(`Fecha Fin Real`),-1L,Días),#Días
         Atraso=if_else(Días!=-1,if_else(Días<3,"En tiempo",#Atraso
                                         if_else(Días<5,"Atraso",
                                                 if_else(Días<8,"Urgente","Prioridad"))),"_"),
         `Rango Cerrado`=if_else(Días==-1,if_else(difftime(`Fecha Fin Real`,`Fecha Asignación`,units="days")<3,"0 a 2 días",#Rango Cerrado
                                                  if_else(difftime(`Fecha Fin Real`,`Fecha Asignación`,units="days")<5,"3 a 4 días",
                                                          if_else(difftime(`Fecha Fin Real`,`Fecha Asignación`,units="days")<8,"5 a 7 días","8 días o más"))),"_"),
         `Atraso Cerrado`=if_else(Días==-1,if_else(`Rango Cerrado`=="0 a 2 días","En tiempo",#Atraso Cerrado
                                                   if_else(`Rango Cerrado`=="3 a 4 días","Atraso",
                                                           if_else(`Rango Cerrado`=="5 a 7 días","Urgente","Prioridad"))),"_"),
         Contingencia=if_else(`Fecha Inicio`<"2020-03-23","Antes","Contingencia"),
         `Asignación Real`=if_else(`Fecha Asignación`<"2021-03-20","No","Si"))#Antes del 20/03/2021 no se generaba fecha de asignación real.
Base <- select(Base,-EstadoA,-FFRA) %>% mutate_all(~as.character(.)) %>% mutate_all(~replace_na(.,"_"))


#------
Base <- mutate(Base,Responsable=if_else(Identificador=="TEQREV-555","Viviana Ingrid Nava Sánchez",Responsable))
#-----

#Imprimiendo
impr_arch(c,Base,"csv","Tickets Histórico","Históricos$")






#googledrive::drive_upload()









