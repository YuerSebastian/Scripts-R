library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(lubridate)
#Iniciales
c_G <- "D:/Cosa/SIR/Carga de materias"
c_D <- paste(c_G,"Descargas",sep = "/")
c_I <- paste(c_G,"Info",sep = "/")
#Funciones
Rep_nom <- function(repet,tam){
  x <- as.integer(length(tam)/length(repet))
  x <- rep(repet,x)
  if((length(tam)-length(x))!=0) x <- append(x,x[c(1:(length(tam)-length(x)))])
  return(x)
}
#####-------------------------------------Estatus general-------------------------------------#####
Cmat <- read_csv(paste(c_D,"Estatus_General_Alumno.csv",sep = "/"),col_types=cols(DECISION="c",SALDO="c",EDAD="c",ESTATUS_EGRESO="c"))
Cmat <- mutate_all(Cmat,~replace(.,is.na(.) | .=="" | .==" ","_")) %>% mutate(CLAVE_PROG=substring(PROGRAMA,1,10), PROGRAMA=substring(PROGRAMA,12,100))%>%
  unite(MATPROG,MATRICULA,CLAVE_PROG,sep = "",remove = F) %>% mutate(MATPROG=gsub("_","",MATPROG))%>%
  mutate_at(c("FECHA_ESTATUS","FECHAINICIO","PRIMER_INSCRIPCION_SIN_ESTATUS"),~dmy(.))%>%
  arrange(desc(ESTATUS_CODE),desc(PRIMER_INSCRIPCION_SIN_ESTATUS),ESTATUS_SOLICITUD,desc(FECHA_ESTATUS),desc(FECHAINICIO)) %>% group_by(MATPROG)%>%
  mutate(cont=row_number()) %>% as.data.frame() %>% filter(cont==1) %>% select(-cont)
#####-------------------------------------Equivalencia-------------------------------------#####
Rep <- read_csv(paste(c_D,"Materias_Equivalencia.csv",sep = "/"),col_types = cols(SECUENCIA="c",CREDITOS_UTL="c"))
Rep <- unite(Rep,MATPROG,MATRICULA,PROGRAMA,sep = "") %>% select(MATPROG) %>% unique() %>% mutate(Equivalencia="Si")
Cmat <- left_join(Cmat,Rep,by="MATPROG")
#####-------------------------------------Predictamen-------------------------------------#####
Rep <- read_csv(paste(c_D,"Materias_Predictamen.csv",sep = "/"),col_types = cols(COD_ESC_PROC="c",CREDIT_PROCE="c",CALIF_PROCE="c",CREDIT_UTL="c",CALIF_UTL="c"))
Rep <- unite(Rep,MATPROG,MATRICULA,PROGRAMA,sep = "") %>% select(MATPROG) %>% unique() %>% mutate(Predictamen="Si")
Cmat <- left_join(Cmat,Rep,by="MATPROG")
Cmat <- mutate_at(Cmat,c("Equivalencia", "Predictamen"),~replace(.,is.na(.),"No"))
#####-------------------------------------Tip-------------------------------------#####
Cmat <- mutate(Cmat,Tip_Equi_Pred=if_else(Equivalencia=="Si" & Predictamen=="Si","Equi/Pred",
                                          if_else(Equivalencia=="Si" & Predictamen=="No","Equivalencia",
                                                  if_else(Equivalencia=="No" & Predictamen=="Si","Predictamen","Pendiente"))))
#####-------------------------------------PREDIC-------------------------------------#####
Rep <- read_excel(paste(c_D,"PREDIC.xlsx",sep = "/"),skip = 1) %>% select(Matricula, Identificador) %>% group_by(Matricula) %>% mutate(cont=row_number())%>%
  filter(cont==1) %>% select(-cont) %>% reshape::rename(c(Matricula="MATRICULA",Identificador="PREDIC"))
Cmat <- left_join(Cmat,Rep,by="MATRICULA")
#####-------------------------------------PREDVENT-------------------------------------#####
Rep <- read_excel(paste(c_D,"PREDVENT.xlsx",sep = "/"),skip = 1) %>% select(`Correo del prospecto`, Identificador) %>% group_by(`Correo del prospecto`)%>%
  mutate(cont=row_number()) %>% filter(cont==1) %>% select(-cont) %>% reshape::rename(c(`Correo del prospecto`="CORREO",Identificador="PREDVENT"))
Cmat <- left_join(Cmat,Rep,by="CORREO")
#Agregando columna combinada PREDIC y PREDVENT, solo si no tiene PREDIC se queda con PREDVENT.
Cmat <- mutate(Cmat, Ticket=if_else(!is.na(PREDIC),PREDIC,PREDVENT), Ticket=if_else(is.na(Ticket),"No",Ticket))
#Se agrega la tip
Cmat <- mutate(Cmat, Tip_Ticket=if_else(Ticket!="No","Con ticket","Sin ticket"))
#####-------------------------------------Ordenando por columna-------------------------------------#####
#Cmat <- arrange(Cmat, Tip_Equi_Pred,TIPO_INGRESO,ESTATUS)
Cmat <- arrange(Cmat, Tip_Equi_Pred) %>% arrange(TIPO_INGRESO) %>% arrange(ESTATUS)
#####-------------------------------------Asignación-------------------------------------#####
Rep <- read_excel(paste(c_I,"Info_general.xlsx",sep = "/"),sheet = "Responsables")
Rep <- Rep$Responsable
Cmat <- mutate(Cmat, Responsable=Rep_nom(Rep,rownames(Cmat)))
#####-------------------------------------Ordenando e imprimiendo-------------------------------------#####
Cmat <- mutate_all(Cmat,~as.character(.)) %>% mutate_all(~replace(.,is.na(.) | .=="" | .==" ","_"))
Rep <- read_excel(paste(c_I,"Info_general.xlsx",sep = "/"),sheet = "Orden") %>% filter(!is.na(Incluir))
Rep <- Rep$Incluir
Cmat <- Cmat[Rep] %>% mutate_all(~stringi::stri_trim(.))
write.csv(Cmat,paste(c_G,"Carga de materias.csv",sep = "/"),row.names = F, na = "_")











