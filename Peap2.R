library(lubridate)
library(dplyr)
library(tidyr)

drive <-googlesheets4::read_sheet(ss="1-7A3TKCV3tfeOjOVaup1wgXot722Ei7GDXkmHQCosfI",range="A1:P5000",col_types = "Tcdddddddddddddd")
names(drive)[1]="Fecha"
names(drive)[3]="Matricula"

peap <- select(drive,Fecha,Matricula)
peap <- peap[!is.na(peap$Matricula),]
peap <- peap[!is.na(peap$Fecha),]
peap <- peap %>% group_by(Matricula) %>% mutate(Solicitud = row_number(),Anio = year(Fecha),Mes = month(Fecha))

retencion <- read.csv("D:/Users/jvelazhe/Desktop/Licenciaturas/Retencion/Retencion.csv")
retencion  <- select(retencion,Matricula,Alianza,Modalidad,Nombre,Bloque_Seg,Nivel_Riesgo,ESTATUS_BANNER)


peap <- left_join(peap,retencion,by="Matricula") 
peap$Alianza <- as.character(peap$Alianza)
peap$Alianza[is.na(peap$Alianza)]<-"Sin_Registro"
peaplisto <- subset(peap,Alianza != "Sin_Registro")
peappendiente <- subset(peap,Alianza == "Sin_Registro")


EDOGRAL <-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/ESTADOGENERAL.csv", header = TRUE,sep = ",",encoding="UTF-7",stringsAsFactors = FALSE)
EDOGRAL <- select(EDOGRAL,MATRICULA,ESTATUS,NIVEL,CAMPUS)
names(EDOGRAL)[1]="Matricula"
peappendiente <- left_join(peappendiente,EDOGRAL,by="Matricula")
peappendiente <- peappendiente %>% mutate(Modalidad = if_else(NIVEL == "LI","Licenciatura",if_else(NIVEL == "MA","Maestria",
                                                                                                   if_else(NIVEL == "MS","Master",
                                                                                                           if_else(NIVEL == "BA","Bachillerato",
                                                                                                                   if_else(NIVEL == "DO","Doctorado","-"))))))

CAMP <-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/CAMPS.csv", header = TRUE,sep = ",",encoding="UTF-7",stringsAsFactors = FALSE)
peappendiente <- left_join(peappendiente,CAMP,by="CAMPUS")
peappendiente$Alianza <- peappendiente$Alianzass
peappendiente$ESTATUS_BANNER <- peappendiente$ESTATUS
peappendiente$Nivel <- "Sin_Materias"
peappendiente$Nivel_Riesgo <- peappendiente$Nivel



peappendiente <- mutate_at(peappendiente,c("Alianza","Modalidad","Nombre","Bloque_Seg","Nivel_Riesgo","ESTATUS_BANNER"),~replace(.,is.na(.),"Sin_Registro"))
peappendiente$ESTATUS <- NULL
peappendiente$NIVEL<- NULL
peappendiente$CAMPUS <- NULL
peappendiente$Alianzass <- NULL
peappendiente$Nivel <- NULL



peap <- rbind(peaplisto,peappendiente)


write.csv(peap,"D://Users/eescobev/Documents/REPORTES/ESTADOS/PEAP.csv")



