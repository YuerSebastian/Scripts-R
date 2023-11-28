library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)


source("D://Users/jvelazhe/Desktop/R/paquetes_dinamicos.R")

fechainicio = "NI 04 JUL"
datest = "2022-07-04"
futuroinicio = "NI 01 AGO"
datestfuturo = "2022-08-01"
#fILTROS niri#####
NIRI <-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/NIRISIR.csv", header = TRUE,sep = ",",encoding="UTF-8",stringsAsFactors = FALSE)
NIRI <- NIRI %>% mutate(MATRICULA=as.numeric(MATRICULA),DECISION=as.numeric(DECISION)) %>%
  filter(grepl('MATRICULADO|ADMITIDO',ESTATUS),!grepl('INA|IND|FIL|VIE', CAMPUS),!grepl('ueba|Uru',ESTUDIANTE),!is.na(MATRICULA))%>%
  mutate(DECISION = if_else(is.na(DECISION),0,DECISION),FECHAINICIO=parse_date_time(FECHAINICIO,"dmy")) 

NIRIGRAL <- NIRI %>% 
  select(-DIRECCION,-COLONIA,-LOCALIDAD,-ESTATUS_SOLICITUD,-ETIQUETA,-OBSERVACIONES,-CURSO,-FECHA_ENROLAMIENTO) %>% 
  mutate(descendiente = xtfrm(FECHA_DECISION)) %>% arrange(desc(descendiente))%>%
  group_by(MATRICULA)%>% mutate(conteos = row_number()) %>% filter(conteos == 1) %>% select(-descendiente,-conteos,-PARTE,-ESTATUS_EGRESO,-INGRESO_MENSUAL) %>% 
  rename(FECHA_INICIO=FECHAINICIO) %>% mutate(ano = year(FECHA_INICIO))


POSGRA <- NIRIGRAL %>% filter(grepl('MA|MS|DO', NIVEL),DECISION == 35)
x <- names(POSGRA)
Inicio1 <- POSGRA %>%
  select(x) %>%
  filter(FECHA_INICIO == as.Date(datest))

Futuros <- POSGRA %>%
  select(x) %>%
  filter(FECHA_INICIO == as.Date(datestfuturo))


IniciosP <- rbind(Inicio1,Futuros)

seniorh<-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/Etiquetas_Historico.csv", header = TRUE,sep = ",",encoding="UTF-8",stringsAsFactors = FALSE)
seniorh2<-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/Etiquetas_Historico2.csv", header = TRUE,sep = ",",encoding="UTF-8",stringsAsFactors = FALSE)
seniormarzo<-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/EtiquetasMarzo21.csv", header = TRUE,sep = ",",encoding="UTF-8",stringsAsFactors = FALSE)
seniormayo<-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/EtiquetasMayo21.csv", header = TRUE,sep = ",",encoding="UTF-8",stringsAsFactors = FALSE)
seniorjulio<-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/EtiquetasJulio21.csv", header = TRUE,sep = ",",encoding="UTF-8",stringsAsFactors = FALSE)
seniorseptiembre<-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/EtiquetasSeptiembre21.csv", header = TRUE,sep = ",",encoding="UTF-8",stringsAsFactors = FALSE)
seniornoviembre<-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/EtiquetasNoviembre21.csv", header = TRUE,sep = ",",encoding="UTF-8",stringsAsFactors = FALSE)
seniorene<-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/EtiquetasMarzo22.csv", header = TRUE,sep = ",",encoding="UTF-8",stringsAsFactors = FALSE)
seniormay<-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/EtiquetasMayo22.csv", header = TRUE,sep = ",",encoding="UTF-8",stringsAsFactors = FALSE)




senior <- rbind(seniorh,seniorh2,seniormarzo,seniormayo,seniorjulio,seniorseptiembre,seniornoviembre,seniorene,seniormay)
write.csv(senior,file="G:Mi unidad/Contencion/ventas_paquetes_etiquetas_historico.csv",row.names = F)

per <- grep("seni",senior$PAQUETE_INSCRITO, ignore.case = TRUE)
senior <- senior[per,]
senior <- data.frame(senior)
senior$Tipo <- "Senior"
senior <- dplyr::select(senior,MATRICULA,Tipo)
semi <- grep("eje",IniciosP$PROGRAMA, ignore.case = TRUE)
Ejec <- IniciosP[semi,]
Ejec <- data.frame(Ejec)
Ejec$Tipo <- "Ejecutivas"
Ejec <- dplyr::select(Ejec,MATRICULA,Tipo)
tipos <- rbind(Ejec,senior)
IniciosP <- dplyr::left_join(IniciosP,tipos,by="MATRICULA")
IniciosP <- mutate_at(IniciosP,c("Tipo"),~replace(.,is.na(.),"Online"))
Filter1 <- grep("UNICEF",IniciosP$PROGRAMA,ignore.case = TRUE)
F1 <- IniciosP[Filter1,]




Online <- subset(IniciosP,Tipo == "Online")
senior_ejec <- subset(IniciosP,Tipo != "Online")


sesionesejecutivas <-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/sesionejecutiva.csv", header = TRUE,sep = ",",encoding="UTF-8",stringsAsFactors = FALSE)
Online <- Online %>% filter(MATRICULA%in%sesionesejecutivas$MATRICULA==F)

#Online <- subset(Online, Online$MATRICULA%in%F1$MATRICULA==F)

#Online <- IniciosP  
#Cargamos la asignación de ETCEL  
  
Etcel <- read_excel("D:/Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/AsignacionPosgrados.xlsm", 
                    sheet = "Asignacion", skip = 1)






EtcelListado <- dplyr::select(Etcel,Matricula,`Clave Programa`)
EtcelListado$Check <- "Ok"
EtcelListado <- tidyr::unite(EtcelListado,"Matr",c(Matricula,`Clave Programa`),sep="")
Online$mat <- Online$MATRICULA
Online$PRO  <- Online$PROGRAMA
Online <- tidyr::separate(Online,PRO,c("CODE_PROG","b","c","d","e","f"),sep=" ")
Online$clave_Prog <- Online$CODE_PROG
Online$b <- NULL
Online$c <- NULL
Online$d <- NULL
Online$e <- NULL
Online$f <- NULL
Online <- tidyr::unite(Online,"Matr",c(mat,CODE_PROG),sep="")
Online <- dplyr::left_join(Online,EtcelListado, by="Matr")
Online <- mutate_at(Online,c("Check"),~replace(.,is.na(.),"Nosta"))
Online <- subset(Online,Check != "Ok" & ESTATUS == "MATRICULADO" |
                      Check != "Ok" & ESTATUS == "ADMITIDO")



prueba <-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/prueba.csv", header = TRUE,sep = ",",encoding="UTF-8",stringsAsFactors = FALSE)
Online <- rbind(data.frame(Online), data.frame(prueba))




Online$Area <- "POSGRADOS"
CAMPUS<-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/campus.csv", header = TRUE,sep = ",",encoding="UTF-7",stringsAsFactors = FALSE)
Online <- dplyr::left_join(Online,CAMPUS,by="CAMPUS")
opm <- grep("opm",Online$Campus, ignore.case = TRUE)
opm <- Online[opm,]
Online <- subset(Online, Online$MATRICULA%in%opm$MATRICULA==F)
Online$`Tutor Anterior` <- ""
Online$Tutor <- ""
Online$Supervisor <-""
Online$Estado <- ifelse(Online$FECHA_INICIO == as.Date(datest),"Activo","FUTUROS")
Online$`NI-Q` <- ifelse(Online$FECHA_INICIO == as.Date(datest),fechainicio,futuroinicio)
Online$Clasificación <- "sin Asignaturas"
Online$Incidencias <- "-"
Online$`Nivel de Riesgo` <- "Sin Asignaturas"
Online$Semana <- "S0"
Online$Fecha_ingreso <- Online$FECHA_ACTIVIDAD
Online$Inicio_Curso <- Online$FECHA_INICIO
Online$Modalidad <- ifelse(Online$NIVEL == "MA","Maestría",
                             ifelse(Online$NIVEL == "MS","Master","Doctorado"))
Online$Duracion <- ifelse(Online$Modalidad == "Doctorado","Cuatrimestral","Bimestral")
Online$Etiqueta <- ifelse(Online$Modalidad == "Doctorado",120,60)
Online$programas <- Online$PROGRAMA
Online <- tidyr::separate(Online,PROGRAMA,c("Clave Programa"),sep=" ")
Online$Programa <- substring(Online$programas,11)
Online <- tidyr::separate(Online,CORREO,c("Email"),sep="@")



Online$Mat <-"-"
Online$`Tutor Anterior` <-"-" 
Online$Turno <-"-"
Online$Rango_Adeudo <-"-"
Online$Mora <-"-"
Online$Docs <-"-"
Online$Acc_Materia <-"-"
Online$Acc_Auvi_ <-"-"
Online$Acc_Mat <-"-"
Online$Avance <-0
Online$Promedio <-0
Online$Aprobadas <-0
Online$Reprobadas <-0
Online$`En Curso` <-0
Online$`Clasificación por Materia` <-"-"
Online$`# Contacto Efectivo` <-0
Online$`# Intentos Contacto` <-0
Online$Rango_Cont_Efec <-"-"
Online$Rango_Int_Cont <-"-"
Online$Contactación <-"-"
Online$Whatsapp <-"-"
Online$`Fecha Ultimo Contacto` <-"-"
Online$`Medio de Contacto` <-"-"
Online$`Tutor que contacto` <-"-"
Online$`Contacto Telefono` <-"-"
Online$Colaboradores <-"-"
Online$Entidad <-"-"
Online$Retención <-"-"
Online$`CICLO JULIO` <-"-"
Online$`TUTOR NIVEL` <-"-"
Online$OBSERVACIONES <-"-"
#Online$TELEFONOS <- IniciosP$CELULAR
Online$`Clasificación anterior` <-"-"
Online$Programas <-"-"
Online$F.Decisón <- Online$FECHA_DECISION
Online$`F. ASIGNACIÓN` <- Sys.Date()
Online$`LINK AULA` <-"-"
Online$`LINK CRM` <-"-"

Online <- dplyr::select(Online,Area,Campus,MATRICULA,Mat,
                        ESTUDIANTE,`Tutor Anterior`,Tutor,
                        Supervisor,Etiqueta,Turno,Estado,`NI-Q`,
                        Clasificación,Incidencias,`Nivel de Riesgo`,
                        Semana,Fecha_ingreso,Inicio_Curso,Modalidad,
                        Duracion,clave_Prog,Programa,Email,FECHA_ACTIVIDAD,
                        CANAL_FINAL,ESTATUS,FECHA_INSCRIPCION,SALDO,
                        Rango_Adeudo,Mora,Docs,Acc_Materia,Acc_Auvi_,
                        Acc_Mat,Avance,Promedio,Aprobadas,Reprobadas,
                        `En Curso`,`Clasificación por Materia`,`# Contacto Efectivo`,
                        `# Intentos Contacto`,Rango_Cont_Efec,Rango_Int_Cont,
                        Contactación,Whatsapp,`Fecha Ultimo Contacto`,`Medio de Contacto`,
                        `Tutor que contacto`,`Contacto Telefono`,Tipo,Colaboradores,Entidad,
                        Retención,`CICLO JULIO`,`TUTOR NIVEL`,OBSERVACIONES,CELULAR,
                        `Clasificación anterior`,Programas,F.Decisón,`F. ASIGNACIÓN`,`LINK AULA`,`LINK CRM`)


x <- names(Etcel)
colnames(Online)<- x

Online$Fecha_ingreso <- lubridate::parse_date_time(Online$Fecha_ingreso,c("dmy"))
Online$Inicio_Curso <- lubridate::date(Online$Inicio_Curso)
Online$`Fecha cambio de Edo` <- lubridate::parse_date_time(Online$`Fecha cambio de Edo`,c("dmy"))
Etcel$`Fecha Inscripción` <- lubridate::date(Etcel$`Fecha Inscripción`)
Online$`Fecha Inscripción` <- lubridate::parse_date_time(Online$`Fecha Inscripción`,c("dmy"))
Online$F.Decisón <- lubridate::parse_date_time(Online$F.Decisón,c("dmy"))
Online[,58] <- as.numeric(as.character(Online[,58]))
Online <- mutate_at(Online,c("TELEFONOS"),~replace(.,is.na(.),0))


write.csv(Online,file ="D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/in.csv",row.names=F)

names(Online)


asignacion <- dplyr::bind_rows(list(Etcel,Online))


EDOGRAL <-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/ESTADOGENERAL.csv", header = TRUE,sep = ",",encoding="UTF-7",stringsAsFactors = FALSE)

asignacion$Matri <- asignacion$Matricula
asignacion$clav <- asignacion$`Clave Programa`
asignacion <- tidyr::unite(asignacion,"Matr",c(Matri,clav),sep="")

EDOGRAL <- dplyr::select(EDOGRAL,Matr,ESTATUS,FECHA_ESTATUS,PAIS)
EDOGRAL$FECHA_ESTATUS <- lubridate::parse_date_time(EDOGRAL$FECHA_ESTATUS,c("dmy"))
EDOGRAL <- EDOGRAL[!duplicated(EDOGRAL),]
asignacion <- dplyr::left_join(asignacion,EDOGRAL,by="Matr")
asignacion$`Estado Argos` <-  asignacion$ESTATUS
asignacion$ESTATUS <- NULL
asignacion$`Fecha cambio de Edo` <- asignacion$FECHA_ESTATUS
asignacion$FECHA_ESTATUS <- NULL
asignacion$Entidad <- asignacion$PAIS
asignacion$PAIS <- NULL


edos <-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/ESTADOS.csv", header = TRUE,sep = ",",encoding="UTF-7",stringsAsFactors = FALSE)
names(edos)[1]="Estado Argos"
asignacion <- dplyr::left_join(asignacion,edos,by="Estado Argos")
asignacion$edos <- ifelse(asignacion$Estado == "FUTUROS","FUTUROS",
                            asignacion$Edo)
asignacion$Estado <- ifelse(asignacion$Estado == "CC","CC",asignacion$edos)
asignacion$Edo <- NULL
asignacion$edos <- NULL




activos1 <- subset(asignacion, Estado == "Activo" & `NI-Q` == "NI 04 JUL")
activos2 <- subset(asignacion, Estado == "Activo" & `NI-Q` == "NI 02 MAY")
activos3 <- subset(asignacion, Estado == "Activo" & `NI-Q` == "NI 30 MAY")

noactivos1 <- subset(asignacion,`NI-Q` != "NI 04 JUL")
noactivos2 <- subset(asignacion,`NI-Q` != "NI 02 MAY")
noactivos3 <- subset(asignacion,`NI-Q` != "NI 30 MAY")


activos <- rbind(activos1,activos2,activos3)
noactivos <- rbind(noactivos1,noactivos2,noactivos3)

NIRI$PRO <- NIRI$PROGRAMA
NIRI <- tidyr::separate(NIRI,PRO,c("CODE_PROG","b","c","d","e","f"),sep=" ")
NIRI$mat <- NIRI$MATRICULA
NIRI$b <- NULL
NIRI$c <- NULL
NIRI$d <- NULL
NIRI$e <- NULL
NIRI$f <- NULL
NIRI <- tidyr::unite(NIRI,"Matr",c(mat,CODE_PROG),sep="")
NIRI <- dplyr::select(NIRI,Matr,FECHAINICIO)
NIRI <- NIRI[!duplicated(NIRI),]
NIRI$descendiente <- xtfrm(NIRI$FECHAINICIO)
NIRI <- NIRI[with(NIRI,order(-NIRI$descendiente)),]
NIRI <- NIRI %>% group_by(Matr)%>% dplyr::mutate(conteos = row_number())
NIRI <- subset(NIRI,conteos == 1)
NIRI$descendiente <- NULL
NIRI$conteos <- NULL
activos <- dplyr::left_join(activos,NIRI,by="Matr")
activos <- activos %>%
  mutate(FECHAINICIO = if_else(is.na(FECHAINICIO), Inicio_Curso, FECHAINICIO))
activos$FECHAINICIO[is.na(activos$FECHAINICIO)]<- activos$Inicio_Curso
#asignacion <- mutate_at(asignacion,c("FECHAINICIO"),~replace(.,is.na(.),asignacion$Inicio_Curso))
activos$CC <- ifelse(activos$FECHAINICIO > activos$Inicio_Curso,"CC" ,"Act")
activos$Estado <- ifelse(activos$CC == "CC" ,activos$CC,activos$Estado)
activos <- activos %>%
  mutate(Inicio_C = if_else(activos$CC == "CC", FECHAINICIO, Inicio_Curso))


activos$Inicio_Curso <- activos$Inicio_C
activos$Inicio_C <- NULL
activos$CC <- NULL
activos$FECHAINICIO <- NULL
asignacion <- rbind(activos,noactivos)

class(activos$FECHAINICIO)
class(activos$Inicio_Curso)

retencion <-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/retencion.csv", header = TRUE,sep = ",",encoding="UTF-7",stringsAsFactors = FALSE)
retencion$X <- NULL
names(retencion)[1]="Matricula"
asignacion <- dplyr::left_join(asignacion,retencion,by="Matricula")
asignacion <- mutate_at(asignacion,c("SOLICITUD"),~replace(.,is.na(.),"-"))
asignacion$Retención <- asignacion$SOLICITUD
asignacion$SOLICITUD <- NULL

SabanaMae <-read.csv("D://Users/eescobev/Documents/REPORTES/FRONT/sabanamae.csv", header = TRUE,sep = ",",encoding="UTF-7",stringsAsFactors = FALSE)
SabanaMae <- subset(SabanaMae,Status.Materia == "Activo")
SabanaMae <- dplyr::select(SabanaMae,Matricula,Semana,Acceso.Materia,Acceso.Aula)
SabanaMae$Acceso.Materia <- lubridate::parse_date_time(SabanaMae$Acceso.Materia,"ymd HM")
SabanaMae$Acceso.Aula <- lubridate::parse_date_time(SabanaMae$Acceso.Aula,"ymd HM")
SabanaMae <- mutate_at(SabanaMae,c("Acceso.Materia","Acceso.Aula"),~replace(.,is.na(.),"2021-01-01 00:00:00"))
SabanaMae$desciende <- xtfrm(SabanaMae$Acceso.Materia)
SabanaMae <- SabanaMae[with(SabanaMae,order(-SabanaMae$desciende)),]
SabanaMae <- SabanaMae %>% group_by(Matricula)%>% dplyr::mutate(conteos = row_number())
SabanaMae <- subset(SabanaMae,conteos == 1)
SabanaMae$desciende <- NULL
SabanaMae$conteos <- NULL
SabanaMae$Acceso.Materias <- as.character(SabanaMae$Acceso.Materia)
SabanaMae$Acceso.Materias <- ifelse(SabanaMae$Acceso.Materias == "2021-01-01 06:00:00","Nunca",SabanaMae$Acceso.Materias) 
SabanaMae$Acceso.Aulas <- as.character(SabanaMae$Acceso.Aula)
SabanaMae$Acceso.Aulas <- ifelse(SabanaMae$Acceso.Aulas == "2021-01-01 06:00:00","Nunca",SabanaMae$Acceso.Aulas) 
SabanaMae$S <- "Semana"
SabanaMae <- tidyr::unite(SabanaMae,"Semanas",c(S,Semana),sep="")
SabanaMae$Acceso.Materia <- SabanaMae$Acceso.Materias
SabanaMae$Acceso.Materias <- NULL
SabanaMae$Acceso.Aula <- SabanaMae$Acceso.Aulas
SabanaMae$Acceso.Aulas <- NULL
asignacion <- dplyr::left_join(asignacion,SabanaMae,by="Matricula")
asignacion <- mutate_at(asignacion,c("Acceso.Materia","Acceso.Aula"),~replace(.,is.na(.),"Sin Asignaturas"))
asignacion <- mutate_at(asignacion,c("Semanas"),~replace(.,is.na(.),"-"))
asignacion$Acc_Materia <- asignacion$Acceso.Materia
asignacion$Acc_Materia <- asignacion$Acceso.Aula
asignacion$Semana <- asignacion$Semanas
asignacion$Acc_Mat <- ifelse(asignacion$Acc_Materia == "Nunca","Nunca",
                            ifelse(asignacion$Acc_Materia == "Sin Asignaturas",
                                   "Sin Asignaturas","Activo"))
asignacion$Semanas<-NULL
asignacion$Acceso.Materia <- NULL
asignacion$Acceso.Aula <- NULL


NRMAE <-read.csv("D://Users/eescobev/Documents/REPORTES/FRONT/niveles_de_riesgoMAE.csv", header = TRUE,sep = ",",encoding="UTF-7",stringsAsFactors = FALSE)
NRMAE$X <- NULL
NRMAE <- subset(NRMAE,edo.moodle == "Activo")
NRS <- dplyr::select(NRMAE,Matricula,Semaforo)
asignaturas <- dplyr::select(NRMAE,Matricula,Aprobado,Np,Nuncas,Por.aprobar,Reprobó,Total.general)
materias <- asignaturas
asignaturas <- tidyr::gather(asignaturas, key="nivel",value="num",2:6)
asignaturas <- subset(asignaturas,num != 0)
asignaturas <- tidyr::unite(asignaturas,"Mat",c(num,nivel),sep=" ")
#asignaturas <- rbind(Bienvenida,asignaturas)
asignaturas <- asignaturas %>% group_by(Matricula) %>% dplyr::mutate(cuenta = row_number())
uno <- subset(asignaturas,cuenta == 1)
dos <- subset(asignaturas,cuenta == 2)
tres <- subset(asignaturas,cuenta == 3)
cuatro <- subset(asignaturas,cuenta == 4)
cinco <- subset(asignaturas,cuenta == 5)
names(dos)[3]="Mat2"
names(tres)[3]="Mat3"
names(cuatro)[3]="Mat4"
dos <- dplyr::select(dos,Matricula,Mat2)
tres <- dplyr::select(tres,Matricula,Mat3)
cuatro <- dplyr::select(cuatro,Matricula,Mat4)
asignaturas <- dplyr::left_join(uno,dos,by="Matricula")
asignaturas <- dplyr::left_join(asignaturas,tres,by="Matricula")
asignaturas <- dplyr::left_join(asignaturas,cuatro,by="Matricula")
asignaturas <- dplyr::select(asignaturas,Matricula,Mat,Mat2,Mat3,Mat4)
asignaturas <- mutate_at(asignaturas,c("Mat","Mat2","Mat3","Mat4"),~replace(.,is.na(.),""))
asignaturas <- tidyr::unite(asignaturas,"asigna",c(Mat,Mat2,Mat3,Mat4),sep="-")
asignaturas$asigna<-gsub("--","",asignaturas$asigna)
asignaturas$asigna<-gsub("---","",asignaturas$asigna)
asignaturas$asigna<-gsub("- ","",asignaturas$asigna)
asignaturas$asigna<-gsub("- ","",asignaturas$asigna)
asignaturas$asigna<-gsub("-"," ",asignaturas$asigna)

materias$clasificacion <- ifelse(materias$Nuncas >= 1,"Nuncas",
                                 ifelse(materias$Np >=1,"Np",
                                        ifelse(materias$Reprobó >=1,"Reprobó",
                                               "Aprobado")))
materias <- dplyr::select(materias,Matricula,clasificacion)
asignaturas <- dplyr::left_join(asignaturas,materias,by="Matricula")
asignaturas <- dplyr::left_join(asignaturas,NRS,by="Matricula")
asignaturas <- asignaturas[!duplicated(asignaturas),]
#write.csv(asignaturas,file ="D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/asignaturas.csv",row.names=F)
asignacion <- dplyr::left_join(asignacion,asignaturas,by="Matricula")
asignacion <- mutate_at(asignacion,c("asigna","clasificacion","Semaforo"),~replace(.,is.na(.),"Sin Asignaturas"))
sesiones <-read.csv("D://Users/eescobev/Documents/REPORTES/FRONT/sesiones.csv", header = TRUE,sep = ",",encoding="UTF-7",stringsAsFactors = FALSE)
sesiones <- subset(sesiones,status == "Activo")
sesiones <- dplyr::count_(sesiones,c("matricula"))
sesiones$Ta <- ifelse(sesiones$n == 1,"Taller","Talleres")
sesiones <- tidyr::unite(sesiones,"sTipo",c(n,Ta),sep=" ")
names(sesiones)[1]="Matricula"
asignacion <- left_join(asignacion,sesiones,by="Matricula")
asignacion <- mutate_at(asignacion,c("sTipo"),~replace(.,is.na(.),""))
asignacion <- tidyr::unite(asignacion,"asigna",c(asigna,sTipo),sep = " ")
asignacion$Mat <- asignacion$asigna
asignacion$`Clasificación por Materia` <- asignacion$asigna
asignacion$`Nivel de Riesgo` <- asignacion$Semaforo
asignacion$Clasificación <- asignacion$clasificacion
asignacion$asigna <- NULL
asignacion$clasificacion <- NULL
asignacion$Semaforo <- NULL

Contactacion <-read.csv("G:Mi unidad/Contactacion/Conteos_Interacciones.csv", header = TRUE,sep = ",",encoding="UTF-7",stringsAsFactors = FALSE)
Contactacion <- Contactacion[!duplicated(Contactacion),]
Contactacion <- dplyr::select(Contactacion,matricula,Contacto.efectivo,Intento,Fecha_ultimo_contacto,Tutor.y,Medio)
names(Contactacion)[1]="Matricula"
Contactacion <- Contactacion[!duplicated(Contactacion),]
Contactacion <- Contactacion[with(Contactacion,order(-Contactacion$Contacto.efectivo)),]

Contactacion <- Contactacion %>% group_by(Matricula)%>% dplyr::mutate(conteos = row_number())
Contactacion <- subset(Contactacion,conteos == 1)
asignacion <- dplyr::left_join(asignacion,Contactacion,by="Matricula")
asignacion <- mutate_at(asignacion,c("Contacto.efectivo","Intento"),~replace(.,is.na(.),0))
asignacion <- mutate_at(asignacion,c("Fecha_ultimo_contacto","Tutor.y","Medio"),~replace(.,is.na(.),"-"))
asignacion$`# Contacto Efectivo` <- asignacion$Contacto.efectivo
asignacion$`# Intentos Contacto` <- asignacion$Intento
asignacion$Contactación <- ifelse(asignacion$`# Contacto Efectivo` > 0,"Contactado",
                               ifelse(asignacion$`# Intentos Contacto` > 0, "No Contactado",
                                      "Sin Intento"))

asignacions <- asignacion
repro <- read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/listado.csv", header = TRUE, sep = ",")
levlab <- read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/levelslabels.csv", header = TRUE, sep = ",")
v <- repro$Interacciones
asignacions <- cbind(asignacions, findInterval(asignacions$`# Contacto Efectivo`, v))
asignacions <- data.frame(asignacions)


asignacions$Rango_Cont_Efec <- factor(asignacions$findInterval.asignacions....Contacto.Efectivo...v., 
                                 levels = levlab$levels,
                                 labels = levlab$labels)

asignacions <- cbind(asignacions, findInterval(asignacions$X..Intentos.Contacto, v))
asignacions <- data.frame(asignacions)

asignacions$Rango_Int_Cont <- factor(asignacions$findInterval.asignacions.X..Intentos.Contacto..v., 
                                      levels = levlab$levels,
                                      labels = levlab$labels)

asignacion <- asignacions
asignacion$Fecha.Ultimo.Contacto <- asignacion$Fecha_ultimo_contacto
asignacion$Tutor.que.contacto <- asignacion$Tutor.y
asignacion$Medio.de.Contacto <- asignacion$Medio
asignacion$Contacto.efectivo <- NULL
asignacion$Intento <- NULL
asignacion$Fecha_ultimo_contacto <- NULL
asignacion$Tutor.y <- NULL
asignacion$Medio <- NULL
asignacion$conteos <- NULL
asignacion$findInterval.asignacions....Contacto.Efectivo...v.<- NULL
asignacion$findInterval.asignacions.X..Intentos.Contacto..v. <- NULL

Financierosir <-read.csv("//Tut-095/REPORTES/ESTADOS/Asignacion Posgrados/financiero.csv", header = TRUE,sep = ",",encoding="UTF-8")
Financiero <- Financierosir
Financiero <- select(Financiero,MATRICULA,MAIL_PRIN)
Financiero[,1] <- as.numeric(as.character(Financiero[,1]))
Financierosir <- select(Financierosir,MATRICULA,SALDO_TOTAL)
Financierosir[,1] <- as.numeric(as.character(Financierosir[,1]))
Financierosir <- Financierosir[!duplicated(Financierosir),]
names(Financierosir)[1]="Matricula"
asignacion <- dplyr::left_join(asignacion,Financierosir,by="Matricula")
asignacion$Adeudo <- asignacion$SALDO_TOTAL
asignacion$SALDO_TOTAL <- NULL


asignacions <- asignacion
repro <- read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/listado2.csv", header = TRUE, sep = ",")
levlab <- read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/levelslabels2.csv", header = TRUE, sep = ",")
v <- repro$Rango_Adeudo
asignacions <- cbind(asignacions, findInterval(asignacions$Adeudo, v))
asignacions <- data.frame(asignacions)


asignacions$Rango_Adeudo <- factor(asignacions$findInterval.asignacions.Adeudo..v., 
                                      levels = levlab$levels,
                                      labels = levlab$labels)

asignacion <- asignacions
asignacion$findInterval.asignacions.Adeudo..v. <- NULL

accstatus <-read.csv("G:Mi unidad/reports/acc_status.csv", header = TRUE,sep = ",",encoding="UTF-7",stringsAsFactors = FALSE)
AULA12 <- subset(accstatus,aula == "Aula12")
AULA12$Num <- 12
AULA13 <- subset(accstatus,aula == "Aula13")
AULA13$Num <- 13
accstatus <- rbind(AULA12,AULA13)
accstatus <- accstatus[with(accstatus,order(-accstatus$Num)),]
accstatus <- dplyr::select(accstatus,Matricula,id,aula)
accstatus <- accstatus[!duplicated(accstatus),]
accstatus <- accstatus %>% group_by(Matricula)%>% dplyr::mutate(conteos = row_number())
accstatus <- subset(accstatus,conteos == 1)
accstatus$conteos <- NULL 
AULA12 <- subset(accstatus,aula == "Aula12")
AULA13 <- subset(accstatus,aula == "Aula13")
AULA12$link12 <- "https://aula12.utel.edu.mx/user/profile.php?id="
AULA13$link13 <- "https://aula13.utel.edu.mx/user/profile.php?id="
AULA12 <- tidyr::unite(AULA12,"linkaula",c(link12,id),sep="")
AULA13 <- tidyr::unite(AULA13,"linkaula",c(link13,id),sep="")
accstatus <- rbind(AULA12,AULA13)
accstatus$aula <- NULL
asignacion <- dplyr::left_join(asignacion,accstatus,by="Matricula")
asignacion$LINK.AULA <- asignacion$linkaula
asignacion$linkaula <- NULL
asignacion <- mutate_at(asignacion,c("LINK.AULA"),~replace(.,is.na(.),"-"))



library(RMySQL)
cons <- dbListConnections(MySQL())
for(con in cons)dbDisconnect(con)


library(RMySQL)
CRM<- dbConnect(MySQL(),user="jvelazhe",host="10.1.46.182",password="Velmar085",dbname="crm")
alumnos<- dbGetQuery(CRM,statement = "select * from rep_alumnos")
alumnos <- dplyr::select(alumnos,matricula_c,email_address,id,fecha_creacion)
names(alumnos)[1]="Matricula"
names(alumnos)[2]="Correo"
names(alumnos)[3]="ID_CRM"
alumnos[,1] <- as.numeric(as.character(alumnos[,1]))
alumnos <- alumnos[!is.na(alumnos$Matricula),]
write.csv(alumnos, file = "G:Mi unidad/Contactacion/IDCRM.csv",row.names = F)
library(RMySQL)
cons <- dbListConnections(MySQL())
for(con in cons)dbDisconnect(con)

# alumnos <- read.csv( file = "//Tut-095/Users/jvelazhe/Google Drive/Contactacion/IDCRM.csv")


alumnos$descendiente <- xtfrm(alumnos$fecha_creacion)
alumnos <- alumnos[with(alumnos,order(-alumnos$descendiente)),]
alumnos <- alumnos %>% group_by(Matricula)%>% dplyr::mutate(conteos = row_number())
alumnos <- subset(alumnos,conteos == 1)
alumnos$descendiente <- NULL
alumnos$conteos <- NULL
alumnos <- dplyr::select(alumnos,Matricula,ID_CRM)
alumnos$link <- "http://crm.utel.edu.mx/index.php?module=Contacts&action=DetailView&record="
alumnos <- tidyr::unite(alumnos,"linkcrm",c(link,ID_CRM),sep="")
write.csv(alumnos, file = "G:Mi unidad/Contactacion/linkCRM.csv",row.names = F)
asignacion <- dplyr::left_join(asignacion,alumnos,by="Matricula")
asignacion <- mutate_at(asignacion,c("linkcrm"),~replace(.,is.na(.),"-"))
asignacion$LINK.CRM <- asignacion$linkcrm
asignacion$linkcrm <- NULL

asignacion <- asignacion[!duplicated(asignacion),]
prueb <- grep("prueb",asignacion$Nombre, ignore.case = TRUE)
eba <- asignacion[prueb,]

asignacion <- subset(asignacion, asignacion$Matricula%in%eba$Matricula==F)

SM <- grep("SM",asignacion$Programa, ignore.case = TRUE)
SM17 <- asignacion[SM,]

asignacion <- subset(asignacion, asignacion$Matricula%in%SM17$Matricula==F)


asignacion$Programa <- trimws(asignacion$Programa, "l")

asignacionpend <- subset(asignacion, Programas == '-')
asignacionind <- subset(asignacion, Programas != '-')

asignacionpend$CEUPE <- ifelse(asignacionpend$Canal == 'OPM UTEL ESPANA',"CEUPE",
                              ifelse(asignacionpend$Canal == 'OPM','CEUPE','-'))

asignacionpend$Izzi <- ifelse(asignacionpend$Canal == 'IZZI','IZZI','-')

asignacionpend$region <- ifelse(asignacionpend$Campus == 'UTEL','UTEL','LATAM')

asignacionpend$modal <- ifelse(asignacionpend$Modalidad == 'Doctorado','DOCTORADO','-')

segmentos <- read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/segmentados.csv", header = TRUE, sep = ",")

asignacionpend <- dplyr::left_join(asignacionpend,segmentos,by="Programa")

asignacionpend[,70] <- as.character(as.character(asignacionpend[,70]))

asignacionpend <- mutate_at(asignacionpend,c("Segmentacion"),~replace(.,is.na(.),"-"))

asignacionpend$regions <- asignacionpend$region
asignacionpend$regionss <- asignacionpend$region
asignacionpend$modals <- asignacionpend$modal
asignacionpend$segms <- asignacionpend$Segmentacion


asignacionpend <- tidyr::unite(asignacionpend,"Modals",c(modals,regions),sep=" ")
asignacionpend <- tidyr::unite(asignacionpend,"Regions",c(segms,regionss),sep=" ")


asignacionpend$Programas2 <- ifelse(asignacionpend$CEUPE != '-',asignacionpend$CEUPE,
                                    ifelse(asignacionpend$Izzi != '-',asignacionpend$Izzi,
                                           ifelse(asignacionpend$modal != '-',asignacionpend$Modals,
                                                  ifelse(asignacionpend$Segmentacion != '-',asignacionpend$Regions,
                                                         asignacionpend$region))))

asignacionpend$Programas <- asignacionpend$Programas2
asignacionpend$CEUPE <- NULL
asignacionpend$Izzi <- NULL
asignacionpend$region <- NULL
asignacionpend$modal <- NULL
asignacionpend$Segmentacion <- NULL
asignacionpend$Modals <- NULL
asignacionpend$Regions <- NULL
asignacionpend$Programas2 <- NULL

asignacion <- rbind(asignacionind,asignacionpend)

futuros1 <- subset(asignacion,Estado == "FUTUROS")
futuros2 <- subset(asignacion,NI.Q == "NI 28 MAR")
futuros <- rbind(futuros1,futuros2)
Financiero <- Financiero[!duplicated(Financiero),]
names(Financiero)[1]="Matricula"
futuros <- left_join(futuros,Financiero,by="Matricula")

library(readxl)
CCAMP1 <- read_excel("D:/Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/CCAMP2020_2021_6.xlsx", 
                               skip = 1)
CCAMP2 <- read_excel("D:/Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/CCAMP2020_2021_7.xlsx", 
                    skip = 1)

CCAMP3 <- read_excel("D:/Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/CCAMP2020_2021_8.xlsx", 
                     skip = 1)

CCAMP1 <- select(CCAMP1,`Correo del estudiante`,Identificador)
CCAMP2 <- select(CCAMP2,`Correo del estudiante`,Identificador)
CCAMP3 <- select(CCAMP3,`Correo del estudiante`,Identificador)

CCAMP <- rbind(CCAMP1,CCAMP2,CCAMP3)
#CCAMP <- select(CCAMP,`Correo del estudiante`,Identificador)
names(CCAMP)[1]="MAIL_PRIN"

futuros <- left_join(futuros,CCAMP,by="MAIL_PRIN")

conteos <-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/historicodematerias.csv", header = TRUE,sep = ",",encoding="UTF-8",stringsAsFactors = FALSE)

futuros <- mutate(futuros,Matr=Matricula,cla = Clave.Programa) %>% unite("MATR",c(Matr,cla),sep="") %>%
  left_join(conteos,by="MATR")

futuros <- mutate_at(futuros,c("Identificador"),~replace(.,is.na(.),"-")) %>% mutate_at(c("Materias_Cursadas"),~replace(.,is.na(.),0))

futuros$tip <- ifelse(futuros$Identificador != "-","Master_Maestria",ifelse(futuros$Materias_Cursadas >= 2 ,"Avanzado","Onboarding"))
futuros <- select(futuros,Matricula,tip)

asignacion <- left_join(asignacion,futuros,by="Matricula")
asignacion <- mutate_at(asignacion,c("tip"),~replace(.,is.na(.),"-"))
asignacion$Tipo <- ifelse(asignacion$tip != "-",asignacion$tip,asignacion$Tipo)

asignacion$tip<- NULL
asignacion$Matr <- NULL

write.csv(asignacion,file ="D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/asignacion.csv",row.names=F)


source("D://Users/jvelazhe/Desktop/R/AsignacionEjecSir.R")

