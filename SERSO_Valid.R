library(readxl)
library(dplyr)
library(lubridate)

#########################################--Validación--#########################################
file.rename("D:/Cosa/SIR/Validación SS.csv", "D:/Cosa/SIR/Validación SS_Anterior.csv")#Se renombra para que quede en el lugar del anterior
SERSO <- read.csv("D:/Cosa/SIR/Descargas/SS/Doc.csv")
names(SERSO) <- gsub(".$","",names(SERSO))
SERSO$Estatus_Val <- if_else(SERSO$ESTATUS=="ENVALIDACION" & SERSO$ESTATUS4=="ENVALIDACION" & SERSO$ESTATUS8=="ENVALIDACION", "Por validar",".")
SERSO <- filter(SERSO, SERSO$Estatus_Val=="Por validar")
SERSO <- SERSO[!duplicated(SERSO$MATRICULA),]
#Se crea una secuencia de números de a cuerdo al número de filas, es necesario hacerlo de ésta manera ya que "rep()" requiere que la secuencia sea
#igual al tamaño de las filas, si son más o menos da error
x <- rep(1:3,length(SERSO$Estatus_Val)/3)
if (((length(SERSO$Estatus_Val) - length(x))!=0)) {
  y <- rep(1:(length(SERSO$Estatus_Val) - length(x)))
  x <- c(x,y)
}else x <- x


SERSO$Responsable <- x
remove(x,y) #---
SERSO$Responsable <- ifelse(SERSO$Responsable==1,"Viridiana Craviotto Peña",
                          if_else(SERSO$Responsable==2,"Stephanie Loraine Gómez Rosado ","Rebeca Raquel Carrasco Benitez"))

write.csv(SERSO, "D:/Cosa/SIR//Validación SS.csv", row.names = F)
#########################################--Asignación SERSO--#########################################
SERSO <- read_excel("D:/Cosa/SS/REPORTES/SERSO_FLOK.xlsx", skip = 1)
names(SERSO)[10] <- "Fecha Asignación"

SERSO <- select(SERSO, Identificador,`Fecha Inicio`,Finalizado,`Fecha Finalización`,`Tarea Actual`,`Fecha Asignación`,Estudiante,Matricula,Licenciatura,
               Correo,CorreoAlternativo,`Opcion de Servicio Social`,Tipo,`Actividades a realizar`,`Area de la empresa/institucion`,`Nombre de la empresa/institucion`,
               `Evidencia de trabajador`)
names(SERSO)[11] <- "Correo alternativo"
names(SERSO)[12] <- "Opción SS"
names(SERSO)[14] <- "Actividades"
names(SERSO)[15] <- "Area"
names(SERSO)[16] <- "Nombre Empresa"

Aux <- select(SERSO, `Fecha Inicio`, `Fecha Asignación`)
Aux$`Fecha Inicio` <- parse_date_time(Aux$`Fecha Inicio`, c("ymd HMS"))

Aux$`Fecha Asignación`[is.na(Aux$`Fecha Asignación`)] <- "1900-01-01"
Aux$`Fecha Asignación` <- as.Date(Aux$`Fecha Asignación`)

Aux$Día<-day(Aux$`Fecha Inicio`)
Aux$Mes<-format(Aux$`Fecha Inicio`, "%B")
Aux$Año<-year(Aux$`Fecha Inicio`)

SERSO <- mutate(SERSO, Día=Aux$Día, Mes=Aux$Mes, Año=Aux$Año)
#Días
Aux$Días <- Sys.Date()-Aux$`Fecha Asignación`
Aux$Días <- gsub(" days","",Aux$Días)
Aux$Días <- as.numeric(Aux$Días)

SERSO$Días <- Aux$Días
#Cruzando con base anterior
Aux <- read.csv("D:/Cosa/SS/REPORTES/SERSO_Anterior.csv")
Aux <- select(Aux, Identificador, Responsable)
SERSO <- left_join(SERSO, Aux, by = "Identificador")
#Si hay nuevos
SERSO$Responsable[is.na(SERSO$Responsable)] <- "nuevo"
Aux <- filter(SERSO, Responsable=="nuevo")

if (length(Aux$Responsable)!=0) {
  x <- rep(1:3,length(Aux$Responsable)/3)
  if ((length(Aux$Responsable)-length(x))!=0) {
    y <- c(1:(length(Aux$Responsable)-length(x)))
    x <- c(x,y)
  }
  
  Aux$Responsable <- x
  
  Aux$Responsable <- if_else(Aux$Responsable==1, "Viridiana Craviotto Peña",
                             ifelse(Aux$Responsable==2, "Rebeca Raquel Carrasco Benitez","Stephanie Loraine Gómez Rosado"))
  SERSO <- filter(SERSO, Responsable!="nuevo")
  SERSO <- rbind(Aux, SERSO)
  
  Aux <- mutate_all(Aux, ~replace(.,is.na(.),"."))
  Aux <- Aux[c(1,2,18,19,20,21,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]
  openxlsx::write.xlsx(Aux, file = "D:/Cosa/SS/REPORTES/SERSO_nuevos.xlsx",sheetName="nuevos",row.names = F)
}


SERSO <- mutate_all(SERSO, ~replace(.,is.na(.),"."))
SERSO <- SERSO[c(1,2,18,19,20,21,22,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]
#Imprimiendo
write.csv(SERSO,file="D:/Cosa/SS/REPORTES/SERSO.csv", row.names = F)
write.csv(SERSO,file="D:/Cosa/SS/REPORTES/SERSO_Anterior.csv", row.names = F)





