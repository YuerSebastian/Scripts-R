library(readr);library(readxl);library(lubridate);library(dplyr);library(tidyr);library(googledrive); library(googlesheets4)
#shell("C:/Users/Jsalinba/Documents/SCRIPTS/SIR_REPORTES/SIR_Documentos_QR/SIR_Documentos_QR.py")
#Variables principales
c_G <- "C:/Users/Jsalinba/Documents/SIR Reportes/Documentos QR"
c_D <- paste(c_G,"Descargas",sep="/")
c_I <- paste(c_G,"Info",sep="/")

drive_auth("jsalinba@utel.edu.mx"); gs4_auth("jsalinba@utel.edu.mx")
c <- list.dirs("C:/Users/Jsalinba/Documents/Reportes")
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
leer_drive <- function(nom,hoja=NULL,...){
  library(googledrive); library(googlesheets4); drive_auth("jsalinba@utel.edu.mx"); gs4_auth("jsalinba@utel.edu.mx")
  y <- drive_find(nom,type = "spreadsheet")
  y <- as.character(y$id)
  x <- range_speedread(y,col_types = cols(.default = "c"),sheet=hoja,...)
  return(x)
}
#QR
Base <- read_csv(paste(c_D,"Documentos QR.csv",sep="/"),locale=locale(encoding="LATIN1"),col_types=cols(.default="c"))
Base <- mutate(Base,FECHA=dmy_hms(FECHA),FECHAENVIO=dmy_hms(FECHAENVIO)) %>% unite(MATACCE,MATRICULA,ACCESORIO,remove = F) %>% arrange(desc(FECHA))%>%
  filter(!duplicated(MATACCE))
#Info
Rep <- read_excel(paste(c_I,"Info.xlsx",sep="/"),sheet="Usuarios",col_names = c("USUARIO_ENVIO","Nombre"))
Base <- left_join(Base,Rep,by="USUARIO_ENVIO")
Rep <- read_excel(paste(c_I,"Info.xlsx",sep="/"),sheet="Accesorios",col_names = c("ACCESORIO","Desc_Accesorio"))
Base <- left_join(Base,Rep,by="ACCESORIO")
#Tip envío CAPS,TIPR
Base <- mutate(Base,ENVIO=if_else(!ACCESORIO %in% c("CAPS","TIPR") & is.na(ENVIO),
                                  if_else(!is.na(USUARIO_ENVIO),"Nuevo envío","No enviado"),
                                  if_else(is.na(USUARIO_ENVIO),"No enviado","Enviado")))
#Base anterior
Rep <- read_csv(paste(c_G,"Documentos QR.csv",sep="/"),locale=locale(encoding="LATIN1"),col_select=c("MATACCE","Responsable"),lazy=F)
T_a <- length(rownames(Rep))#Tamaño anterior de la base
Base <- left_join(Base,Rep,by="MATACCE") %>% mutate(Responsable=if_else(is.na(FECHAENVIO) & (is.na(Responsable) | Responsable=="_"),
                                                                        if_else(ENVIO=="Enviado","Inmediato","nuevo"),Responsable))
#Asignación
Rep <- read_excel(paste(c_I,"Info.xlsx",sep="/"),sheet="Responsables")
Rep2 <- count(Base,Responsable)
Rep <- left_join(Rep,Rep2,by="Responsable") %>% arrange(n) %>% select(-n)
Rep2 <- filter(Base,Responsable=="nuevo") %>% mutate(Responsable=rep_len(Rep$Responsable,length(rownames(.))))
Base <- filter(Base,Responsable!="nuevo") %>% rbind(Rep2)
#Días
Base <- mutate(Base,Días=if_else(is.na(FECHAENVIO),difftime(now(),FECHA,units = "days"),-1),Días=as.integer(Días))
#Moy
Rep <- read_excel(paste(c_I,"Moy.xlsx",sep = "/"), col_types="text") %>%
  mutate(MATRICULA=ifelse(nchar(MATRICULA)==8,paste("0",MATRICULA,sep = ""),MATRICULA)) %>% unite(MATPROG, MATRICULA, PROGRAMA, remove = F, sep = "") %>% unique()
names(Rep)[5] <- "Docs Digitales"
Rep <- Rep[!duplicated(Rep$MATRICULA), ]
Rep <- select(Rep, MATRICULA,Titulación,`Docs Digitales`)
Base <- left_join(Base,Rep,by="MATRICULA")
#Adeudo
Rep <- leer_arch(c,"csv","Academico_Financiero","Escolares$",col_select=
                   c(MATRICULA,"Saldo Vencido"=SALDO_VENCIDO,"Fecha Límite"=PROX_FECHA_LIMITE_PAG,"Primer Fecha Límite"=PRIMER_FECHA_LIMITE_DE_PAGO,
                     "Última Fecha Límite"=ULTIMA_FECHA_LIMITE_DE_PAGO,"Estado"=ESTADO_ALUMNO))%>%
  mutate(`Saldo Vencido`=as.numeric(`Saldo Vencido`),Adeudo=if_else(`Saldo Vencido`>500,"Si","No")) %>% mutate_at(grep("Fecha",names(.)),~dmy(.))%>%
  arrange(desc(`Fecha Límite`)) %>% filter(!duplicated(MATRICULA)) %>% select(MATRICULA,`Saldo Vencido`,Adeudo)
Base <- left_join(Base,Rep,by="MATRICULA")
#Re ordenando e imprimiendo
Base <- mutate(Base,Ult_Act = as.character(now()))
Rep <- Rep <- read_excel(paste(c_I,"Info.xlsx",sep="/"),sheet="Orden")
Base <- `colnames<-`(Base,Rep$Nuevo) %>% .[Rep$Incluir[!is.na(Rep$Incluir)]] %>% mutate_all(~as.character(.)) %>% mutate_all(~replace_na(.,"_"))

if (length(rownames(Base))>=T_a) {
  file.rename(paste(c_G,"Documentos QR.csv",sep="/"), paste(c_G,"Documentos QR_Ant.csv",sep="/"))
  write.csv(Base,paste(c_G,"Documentos QR.csv",sep="/"),row.names = F)
}else{print("El tamaño de la base es menor a la anterior")}
















