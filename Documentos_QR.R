library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(readxl)
shell("D:/Scripts/Python/REPORTES_SIR/SIR_Docs_QR/SIR_Docs_QR.py")
#Funciones
ord <- function(t1,t2){
  x <- rep(1:t2,t1/t2)
  if (length(x)<t1) {
    x <- append(x,1:(t1-length(x)))
  }
  return(x)
}
#Variables
c_G <- "D:/Cosa/Documentos QR"
c_D <- paste(c_G,"Descargas",sep="/")
c_I <- paste(c_G,"Info",sep="/")
#####-----------------------------------------Documentos QR-----------------------------------------#####
#Actual
QR <- read_csv(paste(c_D,"UTEL.csv",sep = "/"),col_types=cols(ENVIO=col_character())) %>% reshape::rename(c("#...8"="No.1","#...9"="No.2"))
QR <- separate(QR,FECHA,into = c("Día","Mes_N","Año","Hora"),remove = F,sep = "-") %>% separate(FECHAENVIO,into = c("Día_E","Mes_N_E","Año_E","Hora_E"),remove = F,sep = "-")%>%
  unite(MATACCE,MATRICULA,ACCESORIO,remove = F)
#cruzando con los meses (número)
Info <- read_excel(paste(c_I,"Info.xlsx",sep = "/"),sheet = "Meses")
QR <- left_join(QR,Info,by="Mes_N")
Info <- reshape::rename(Info, c(Mes_N="Mes_N_E",Mes="Mes_E"))
QR <- left_join(QR,Info,by="Mes_N_E")
QR <- unite(QR,FECHA,Año,Mes,Día,sep="/",remove=F) %>% unite(FECHAENVIO,Año_E,Mes_E,Día_E,sep="/",remove=F) %>% 
  unite(FECHA,FECHA,Hora,sep = " ",remove = F) %>% unite(FECHAENVIO,FECHAENVIO,Hora_E,sep = " ",remove = F) %>% 
  mutate_at(c("FECHA","FECHAENVIO"),~ymd_hms(.)) %>% arrange(desc(FECHA)) %>% group_by(MATACCE) %>% mutate(cont=row_number()) %>% as.data.frame()%>%
  filter(cont==1) %>% select(-cont)
#Cruce con base anterior
Rep <- read.csv(paste(c_G,"Documentos QR.csv",sep = "/")) %>% select(MATACCE,Responsable)
QR <- left_join(QR,Rep,by="MATACCE")
#Conteo y ordenamiento
Info <- read_excel(paste(c_I,"Info.xlsx",sep = "/"),sheet = "Responsables")
Rep <- count(QR, Responsable)
Info <- left_join(Info, Rep, by="Responsable") %>% arrange(n) %>% mutate(n=row_number()) %>% select(n,Responsable) %>% reshape::rename(c(n="Responsable",Responsable="Asig"))
#Asignando
QR$Responsable[is.na(QR$Responsable)] <- "nuevo"
Rep <- filter(QR, Responsable=="nuevo")
x <- ord(length(rownames(Rep)),length(rownames(Info)))
Rep <- mutate(Rep, Responsable=x) %>% left_join(Info, by="Responsable") %>% mutate(Responsable=Asig) %>% select(-Asig)
#Uniendo lo nuevo
QR <- filter(QR, Responsable!="nuevo") %>% rbind(Rep)
#Moy
Rep <- read_excel(paste(c_I,"Moy.xlsx",sep = "/"), col_types = c("text", "text", "text", "text")) %>% 
  mutate(MATRICULA=ifelse(nchar(MATRICULA)==8,paste("0",MATRICULA,sep = ""),MATRICULA)) %>% unite(MATPROG, MATRICULA, PROGRAMA, remove = F, sep = "") %>% unique()
names(Rep)[5] <- "Docs Digitales"
Rep <- Rep[!duplicated(Rep$MATRICULA), ]
Rep <- select(Rep, MATRICULA,Titulación,`Docs Digitales`)
QR <- left_join(QR,Rep,by="MATRICULA")
#Utlima actualización
QR$Ult_Act <- as.character(now())
#Reordenando
Info <- read_excel(paste(c_I,"Info.xlsx",sep = "/"),sheet = "Orden")
Info <- Info$Incluir
QR <- QR[Info]
#Imprimiendo
QR <- mutate_all(QR,~as.character(.)) %>% mutate_all(~replace(.,is.na(.),"_"))
#Anterior
file.rename(paste(c_G,"Documentos QR.csv",sep = "/"),paste(c_G,"Documentos QR_Ant.csv",sep = "/"))
#Actual
write.csv(QR,paste(c_G,"Documentos QR.csv",sep = "/"),row.names = F,na = "_")
















