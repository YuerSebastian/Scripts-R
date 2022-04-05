library(dplyr);library(lubridate);library(tidyr);library(googlesheets4);library(readr);library(readxl)
#Variables principales.
c <- list.dirs("D:/Trabajo")
c <- c(c,"1BrBMTo2j14lTF-Cn9DP6rWtYJFxLW5OlYhWsvnahEJ0") #Se a�ade al �ltimo solo si se requiere una sheet de google
desc <- list.files(c[grep("FLOKZU$",c)],full.names = T)
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
#####----------------------------------------------------Unificando FLOKZUS----------------------------------------------------#####
for (i in 1:length(desc)) {
  Rep <- read_excel(desc[i],skip=1,col_types="text") %>% reshape::rename(c(`Fecha asignaci�n Tarea actual`="Fecha Tarea",`A.analista SER`="Analista SER",
                                                                           `Correo del estudiante`="Correo",`Correo del prospecto`="Correo"))
  if (length(rownames(Rep))!=0) {
    if(!hasName(Rep,"Matricula")) Rep <- rename(Rep,"Matricula"=SIU); if(!hasName(Rep,"Correo")) Rep <- mutate(Rep,Correo="_")
    Rep <- select(Rep,Identificador,"Matr�cula"=Matricula,Correo,`Fecha Inicio`,Finalizado,`Fecha Finalizaci�n`,`Tarea Actual`,`Fecha Tarea`,`Analista SER`)
    if(exists("Base")) Base <- rbind(Base,Rep) else Base <- Rep
  }
}
Base <- mutate(Base,Proceso=gsub("-[0-9]+","",Identificador)) %>% mutate_all(~stringi::stri_trim(.)) %>% mutate_all(~gsub("\n","",.)) %>% filter(!duplicated(Identificador))%>%
  mutate(`Analista SER`= gsub("Francisco Javier Sandoval Cevantes","Francisco Javier Sandoval Cervantes",`Analista SER`),#Nombres erroneos
         `Analista SER`= gsub("Jocelyn Velarde Hernandez","Jocelyn Velarde Hern�ndez",`Analista SER`))
#####----------------------------------------------------Cruce base anterior----------------------------------------------------#####
Rep <- leer_arch(c,"csv","Tickets Res�men","auxiliares$", lazy = F,
                 col_select = c("Identificador","Responsable","Responsable Apoyo","EstadoA"=Estado,"Fecha Asignaci�n","FFRA"=`Fecha Fin Real`))
t_a <- length(rownames(Rep))#Tama�o de la base anterior
Base <- left_join(Base,Rep,by = "Identificador") %>% mutate(`Fecha Asignaci�n`=if_else(is.na(`Fecha Asignaci�n`),as.character(now()),`Fecha Asignaci�n`),
                                                            nuevo=if_else(is.na(Responsable),if_else(grepl("SER]|SERVICIOS|ARCHIVO]",`Tarea Actual`),"N","x"),"_"))%>%
  filter(nuevo!="x") %>% mutate_at(c(grep("^Fecha|FFRA",names(Base))),~if_else(nchar(.)==16,paste(.,":02",sep=""),.))#Fechas con 0 segundos
#####----------------------------------------------------Cruce con Tareas (Estado)----------------------------------------------------#####
Rep <- leer_arch(c,"xlsx","Info general","Trabajo$",sheet = "Tareas") %>% mutate_all(~stringi::stri_trim(.)) %>% unique()
Base <- left_join(Base,Rep,by = "Tarea Actual") %>% mutate(Estado = if_else(is.na(Estado),"Sin tip",Estado),
                                                           Estado=if_else(Estado=="Activo",#Regresados
                                                                          if_else(!is.na(EstadoA)&(EstadoA!="Activo"|EstadoA=="Activo (regresado)"),
                                                                                  "Activo (regresado)",Estado),Estado))
#Actualizaci�n de fecha a los regresados
Base <- mutate(Base,`Fecha Asignaci�n`=if_else(EstadoA!="Activo" & EstadoA!="Activo (regresado)",
                                               if_else(Estado=="Activo (regresado)",as.character(now()),`Fecha Asignaci�n`),`Fecha Asignaci�n`))
#####----------------------------------------------------Asignaci�n----------------------------------------------------#####
Asig <- filter(Base,nuevo=="N")
if (length(rownames(Asig))!=0) {#Solo si existen nuevos
  #Contando asignaci�n del a�o actual para ordenar analistas, se excluyen cancelados, se cuentan todos los trabajados.
  Rep <- leer_arch(c,"gsheet",sheet = wday(today(),week_start = 1)) %>% filter(Excepciones != "N") %>% select(-Excepciones)
  for (i in 2:length(Rep)) {
    Rep2 <- filter(Asig,Proceso==names(Rep)[i])
    if (length(rownames(Rep2))!=0) { #Si existen procesos que asignar...
      cont <- filter(Base,year(`Fecha Asignaci�n`) == year(now()) & Estado != "Cancelado SER" & Proceso == names(Rep)[i]) %>% count(Responsable) %>% arrange(n)
      x <- subset.data.frame(Rep,Rep[i]!="_",c(1,i)) %>% left_join(cont,by = "Responsable") %>% arrange(n) %>% .[[1]]
      Rep2 <- mutate(Rep2,Responsable=rep_len(x,length(rownames(Rep2)))); Asig <- filter(Asig,Proceso!=names(Rep)[i]) %>% rbind(Rep2)
    }
  }
  Base <- filter(Base,nuevo!="N") %>% rbind(Asig)
}
#####----------------------------------------------------Asignando Apoyos (Lunes, S1 y S2)----------------------------------------------------#####
if (wday(today(),week_start = 1)==1) {
  #Identificando los apoyos a asignar
  Rep <- leer_arch(c,"gsheet",sheet = "Procesos",col_select=c("Proceso","Especial"))
  Base <- left_join(Base,Rep,by="Proceso") %>% mutate(Especial=if_else(Especial=="Apoyo",
                                                                       if_else(is.na(`Responsable Apoyo`)&(Estado=="Activo"|Estado=="Activo (regresado)"),Especial,"_"),"_"))
  #Contando para ordenar y asignando
  cont <- filter(Base,year(`Fecha Asignaci�n`) == year(now()) & Estado != "Cancelado SER") %>% count(`Responsable Apoyo`) %>% arrange(n)
  Rep <- leer_arch(c,"gsheet",sheet = "Apoyos") %>% left_join(cont,by="Responsable Apoyo") %>% arrange(n) %>% select(-n)
  Rep2 <- filter(Base,Especial=="Apoyo") %>% mutate(`Responsable Apoyo`=rep_len(Rep$`Responsable Apoyo`,length(rownames(.))))
  Base <- filter(Base,Especial!="Apoyo") %>% rbind(Rep2) %>% select(-Especial)
}
remove(cont,Rep2,Asig)
#####----------------------------------------------------Fechas (c�lculos)----------------------------------------------------#####
Base <- mutate_all(Base,~replace_na(.,"_")) %>% mutate(`Fecha Fin Real`=if_else(Estado!="Activo" & Estado!="Activo (regresado)" & Estado!="Ayuda",#Fecha Fin Real
                                                                                if_else(`Fecha Finalizaci�n`=="_",
                                                                                        `Fecha Tarea`,if_else(FFRA=="_" | FFRA==`Fecha Finalizaci�n`,
                                                                                                              `Fecha Finalizaci�n`,FFRA)),"_"))%>%
  mutate_at(c(grep("^Fecha",names(.))),~ymd_hms(.))%>%
  mutate(Trabajado=if_else(Estado!="Activo",#Trabajado
                           if_else((`Fecha Asignaci�n` > `Fecha Fin Real`) & (!is.na(`Fecha Fin Real`) & !is.na(`Fecha Asignaci�n`)),"X",
                                   if_else(Responsable==`Analista SER`,"Si",
                                           if_else(`Responsable Apoyo`==`Analista SER`,"Si (Apoyo)","No"))),"_"),
         D�as=as.integer(difftime(now(),`Fecha Tarea`,units="days")),D�as=if_else(is.na(D�as),-1L,D�as),#D�as
         Atraso=if_else(D�as!=-1,if_else(D�as<3,"En tiempo",#Atraso
                                         if_else(D�as<5,"Atraso",
                                                 if_else(D�as<8,"Urgente","Prioridad"))),"_"),
         `Rango Cerrado`=if_else(D�as==-1,if_else(difftime(`Fecha Fin Real`,`Fecha Asignaci�n`,units="days")<3,"0 a 2 d�as",#Rango Cerrado
                                                  if_else(difftime(`Fecha Fin Real`,`Fecha Asignaci�n`,units="days")<5,"3 a 4 d�as",
                                                          if_else(difftime(`Fecha Fin Real`,`Fecha Asignaci�n`,units="days")<8,"5 a 7 d�as","8 d�as o m�s"))),"_"),
         `Atraso Cerrado`=if_else(D�as==-1,if_else(`Rango Cerrado`=="0 a 2 d�as","En tiempo",#Atraso Cerrado
                                                   if_else(`Rango Cerrado`=="3 a 4 d�as","Atraso",
                                                           if_else(`Rango Cerrado`=="5 a 7 d�as","Urgente","Prioridad"))),"_"),
         Contingencia=if_else(`Fecha Inicio`<"2020-03-23","Antes","Contingencia"),#Contingencia
         `Asignaci�n Real`="Si")#Asignaci�n real, solo para saber desde cu�ndo se tienen fechas de la asignaci�n real de los tickets.
Base <- select(Base,-EstadoA,-FFRA,-nuevo) %>% mutate_all(~as.character(.)) %>% mutate_all(~replace_na(.,"_"))#Quitando columnas que no se necesitan y convirtiendo a caracter
#####----------------------------------------------------Resumen, Hist�rico y Estatus general (Campus y Nivel)----------------------------------------------------#####
write.csv(Base,paste(c[grep("auxiliares$",c)],"Tickets Res�men.csv",sep="/"),row.names = F)#Imprimiendo res�men
Rep <- leer_arch(c,"csv","Tickets Hist�rico","Hist�ricos$")
Base <- rbind(Base,Rep)
Rep <- leer_arch(c,"csv","Estatus_General_Alumno","Escolares$",col_select=c("Matr�cula"=MATRICULA,"Correo"=CORREO,"FECHA_ESTATUS","FECHAINICIO","CAMPUS","NIVEL"))%>%
  mutate_at(c(grep("^FECHA",names(.))),~dmy(.)) %>% arrange(desc(FECHA_ESTATUS),desc(FECHAINICIO)) %>% filter(!duplicated(Matr�cula)) %>% select(-c(grep("^FECHA",names(.))))
Rep2 <- select(Rep,-Correo)
Base <- left_join(Base,Rep2,by="Matr�cula")
Rep2 <- select(Rep,-Matr�cula) %>% filter(!duplicated(Correo))
Base <- left_join(Base,Rep2,by="Correo") %>% mutate(CAMPUS.x=if_else(is.na(CAMPUS.x),CAMPUS.y,CAMPUS.x,"_"),NIVEL.x=if_else(is.na(NIVEL.x),NIVEL.y,NIVEL.x,"_"))%>%
  select(-c("CAMPUS.y","NIVEL.y")) %>% rename("Campus"=CAMPUS.x,"Nivel"=NIVEL.x)
remove(Rep2)
#####----------------------------------------------------Cruzando con Info general----------------------------------------------------#####
x <- c("Info Analistas","Info Procesos","Campus","Niveles"); y <- c("Analista SER","Proceso","Campus","Nivel")
for (i in 1:length(x)) {
  Rep <- leer_arch(c,"xlsx","Info general","Trabajo$",sheet=x[i]); if(hasName(Rep,"Fecha Campus")) Rep <- select(Rep,-`Fecha Campus`)
  if (hasName(Rep,"Proceso")) Rep <- select(Rep,c(1:grep("Descripci�n Proceso",names(Rep))))#Solo para seleccionar columnas de procesos necesarias
  if (hasName(Rep,"Analista SER")) {#Solo para cruzar el coordinador de los apoyos y el turno con el responsable
    Rep2 <- select(Rep,"Responsable Apoyo"=`Analista SER`,"Coordinador Apoyo"=Coordinador); Base <- left_join(Base,Rep2,by="Responsable Apoyo")
    Rep2 <- select(Rep,"Responsable"=`Analista SER`,Turno); Base <- left_join(Base,Rep2,by="Responsable")
    Rep <- select(Rep,-Turno)
  }
  Base <- left_join(Base,Rep,by=y[i])
}
remove(Rep2)

# Rep <- leer_arch(c,"xlsx","Info general","Trabajo$",sheet="Info Procesos")
# Rep <- reshape2::melt(Rep,id.vars="Proceso",measure.vars=5:8,value.name = "Rango",variable.name="Tiempo")
#Aqu�
#####----------------------------------------------------Re ordenando e imprimiendo----------------------------------------------------#####
Rep <- leer_arch(c,"xlsx","Orden base","Trabajo$",sheet="Tickets")
Base <- `colnames<-`(Base,Rep$Nuevo) %>% .[c(Rep$Incluir[!is.na(Rep$Incluir)])]
write.csv(Base,paste(c[grep("Principales$",c)],"Tickets.csv",sep="/"),row.names = F)



















