library(dplyr);library(tidyr);library(readr);library(readxl);library(lubridate);library(googlesheets4);library(reshape2)
#Variables principales
gs4_auth("jsalinba@utel.edu.mx")
#drive_auth("jsalinba@utel.edu.mx")
c <- list.dirs("D:/Trabajo")
c <- c(c,"1HJJyzyAWIz7TxqzEOFsAxmVBmgaMZpl_Jp9Nv3JmDbc") #Se a�ade al �ltimo solo si se requiere una sheet de google
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
#####----------------------------------------Tickets----------------------------------------#####
Base <- leer_arch(c,"csv","Tickets","Principales$",
                  col_select=c("Identificador","Matr�cula","Responsable","Campus","Descripci�n Campus","Tipo Campus","Descripci�n Nivel","Coordinador","Estado","Proceso","Descripci�n Proceso",
                               "Tipo","Fecha Asignaci�n","Fecha Fin Real","Rango Cerrado")) %>% mutate_all(~replace_na(.,"_"))
impr_arch(c,Base,"csv","KPI�s-Tickets","Principales$")
#####----------------------------------------Validaci�n----------------------------------------#####
Rep <- leer_arch(c,"xlsx","Info general","Trabajo$",sheet="Campus") %>% select(-`Fecha Campus`) %>% rename("CAMPUS"=Campus)
Base <- leer_arch(c,"csv","Validaci�n","Principales$",col_select=c(MATPROG,MATRICULA,CLAVE_PROG,"Descripci�n Nivel"=Desc_Nivel,CAMPUS,DECISION,FECHA_ACTIVIDAD,FECHA_DECISION))%>%
  mutate_at(grep("^FECHA",names(.)),~ymd(.)) %>% mutate(D�as=as.integer(FECHA_DECISION-FECHA_ACTIVIDAD),Atraso=if_else(!is.na(D�as),
                                                                                                                       if_else(D�as<2,"0h-48h",
                                                                                                                               if_else(D�as<3,"48h","72h o m�s")),"SF"),
                                                        D�as=if_else(is.na(D�as),-1L,D�as)) %>% left_join(Rep,by="CAMPUS")%>%
  mutate_all(~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
impr_arch(c,Base,"csv","KPI�s-Validaci�n","Principales$")
#####----------------------------------------Documentos----------------------------------------#####
Rep <- rename(Rep,"Campus"=CAMPUS)
Base <- leer_arch(c,"gsheet",sheet="Documentos",
                  col_select=c("Matr�cula"=MATRICULA,"Programa"=PROGRAMA,Responsable,"Estatus"=ESTATUS,"Campus"=CAMPUS,Estado,"Tipo Ingreso"=Tipo_Ingreso,Equivalencia,
                               "Expediente F�sico"=`estatus expediente`,"Expediente Digital"=`expediente digital`,"Fecha Recuperaci�n"=`Fecha recu`,"Fecha Digital"=`Fecha dig`,
                               "Fecha Decisi�n"=FECHA_DECISION,"Fecha Inicio"=FECHA_INICIO,Entrega,Bono))%>%
  mutate(Matr�cula=if_else(nchar(Matr�cula)<9,paste("0",Matr�cula,sep=""),Matr�cula)) %>% mutate_at(grep("^Fecha",names(.)),~mdy(.))%>%
  unite(MATPROG,Matr�cula,Programa,sep="",remove=F) %>% arrange(desc(`Fecha Decisi�n`),desc(`Fecha Inicio`)) %>% filter(!duplicated(MATPROG)) %>% left_join(Rep,by="Campus")%>%
  mutate(`Expediente F�sico`=if_else(`Expediente F�sico`=="F�sico Completo","Completo",
                                     if_else(`Expediente F�sico`=="F�sico Incompleto","Incompleto","Sin Documentos")))
Base <- Base[c("MATPROG","Matr�cula","Programa","Responsable","Estatus","Campus","Descripci�n Campus","Tipo Campus","Estado","Tipo Ingreso","Equivalencia","Expediente F�sico",
               "Expediente Digital","Fecha Recuperaci�n","Fecha Digital","Fecha Decisi�n","Fecha Inicio","Entrega","Bono")]
Base <- mutate_all(Base,~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
impr_arch(c,Base,"csv","KPI's-Recolecci�n","auxiliares$")

#####----------------------------------------Cert y Tit (Contactaci�n,Drive)----------------------------------------#####
Base <- leer_arch(c,"gsheet",sheet="Cert y Tit",range="A2:M") %>% mutate(Fecha=parse_date_time(Fecha,c("%m/%Y")))%>%
  melt(id.vars="Fecha",measure.vars=2:length(.),value.name="Totales",variable.name="Motivo") %>% arrange(desc(Fecha))%>%
  mutate(`Tipo Contacto`=if_else(Motivo=="Remarcaciones","Remarcaci�nes","Contactadas"))
Base <- Base[c("Fecha","Motivo","Tipo Contacto","Totales")]
#Imprimiendo
impr_arch(c,Base,"csv","KPI's-Cert y Tit","auxiliares")
#####----------------------------------------Cert y Tit----------------------------------------#####
# Rep <- leer_arch(c,"csv","Cert y Tit Digitales","Escolares$",col_select=c(MATRICULA,CVE_PROGRAMA,"Estatus CT"=ESTATUS,"Fecha CT"=FECHA_CREACION,"Tipo CT"=TIPO))%>%
#   mutate(`Fecha CT`=dmy(`Fecha CT`),`Tipo CT`=if_else(`Tipo CT`=="Ti??tulo","T�tulo","Certificado")) %>% arrange(desc(`Fecha CT`))%>%
#   unite(MATPROG,MATRICULA,CVE_PROGRAMA,sep="",na.rm=T) %>% filter(!duplicated(MATPROG))
# 
# Base <- leer_arch(c,"csv","Estatus_General_Alumno","Escolares$",col_select=c(MATRICULA,ESTATUS_CODE,FECHA_ESTATUS,PRIMER_INSCRIPCION_SIN_ESTATUS,CAMPUS,NIVEL,PROGRAMA,
#                                                                              PERIODO_CATALOGO,FECHAINICIO,TIPO,TIPO_INGRESO,CLAVE_CANAL,CANAL_FINAL,DECISION,USUARIO_DECISION))%>%
#   mutate(`Clave Programa`=substring(PROGRAMA,1,10),PROGRAMA=substring(PROGRAMA,12)) %>% unite(MATPROG,MATRICULA,`Clave Programa`,sep="",remove=F,na.rm=T)%>%
#   mutate_at(grep("^FECHA|SIN_ESTATUS$",names(.)),~dmy(.)) %>% arrange(desc(FECHA_ESTATUS),desc(FECHAINICIO)) %>% filter(!duplicated(MATPROG)) %>% left_join(Rep,by="MATPROG")%>%
#   mutate_all(~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
# 
# x <- c("Estatus","Niveles","Campus"); y <- c("ESTATUS_CODE","NIVEL","CAMPUS"); z <- c("Clave Estatus","Nivel","Campus")
# for (i in 1:length(x)) {
#   Rep <- leer_arch(c,"xlsx","Info general","Trabajo$",sheet=x[1]) %>% rename(y[1]=1); if(hasName(Rep,"Fecha Campus")) Rep <- select(-`Fecha Campus`)
#   Base <- left_join(Base,Rep,by=y[1])
# }
# 
# Rep <- rename(Rep,"COSA"="MATPROG")
# 
# 
# Rep <- leer_arch(c,"xlsx","Info general","Trabajo$",sheet="Estatus") %>% rename(ESTATUS_CODE="Clave Estatus")
# Base <- left_join(Base,Rep,by="ESTATUS_CODE")
# Rep <- leer_arch(c,"xlsx","Info general","Trabajo$",sheet="Niveles") %>% rename(NIVEL="Nivel")
# Base <- left_join(Base,Rep,by="NIVEL")
# Rep <- leer_arch(c,"xlsx","Info general","Trabajo$",sheet="Campus") %>% rename(CAMPUS="Campus") %>% select(-`Fecha Campus`)
# Base <- left_join(Base,Rep,by="CAMPUS")




















