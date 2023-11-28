library(readxl) #Libreria # 
library(dplyr)

# fechainicio = "Bimestre 01_B" # Variables
# datest = "2022-03-14" # Variables
# futuroinicio = "Bimestre 01_C" # Variables
# datestfuturo = "2022-03-28" # Variables
#fILTROS niri#####
library(dplyr)
NIRI <-read.csv("//Tut-095/reportes/ESTADOS/NIRISIR.csv" , header = TRUE,sep = ",",encoding="latin",stringsAsFactors = FALSE) #cargar archivo CSV#
NIRI[,1] <- as.numeric(as.character(NIRI[,1])) #Esta funcion solo afecta a la columna 1, indica que es numerico o caracter#
NIRI[,18] <- as.numeric(as.character(NIRI[,18]))
NIRI <- NIRI[!is.na(NIRI$MATRICULA),] #Filtros - quitar todo lo que sea diferente a lo indicado en la columna indicada# 
NIRI <- NIRI %>% filter((grepl('MATRICULADO', .$ESTATUS)
                     | grepl('ADMITIDO', .$ESTATUS))) #Filtramos matriculados y admitidos#

# NIRI1 <- subset(NIRI,ESTATUS == "MATRICULADO") #Filtro todo lo que sea igual a #
# NIRI12 <- subset(NIRI,ESTATUS == "ADMITIDO")
# NIRI <- rbind(NIRI1,NIRI12) # Unir las filas #

NIRI <- NIRI %>% # LLamar paquete #
  dplyr::mutate(DECISION = if_else(is.na(DECISION),0,DECISION)) # reescribe la columna y todo lo que es NA lo ponga como 0#
NIRI$FECHAINICIO <- lubridate::parse_date_time(NIRI$FECHAINICIO,"dmy") # LLama libreria lubridate para cambiar el formato de la columna indicada #
#NIRI$FECHAINICIO <- as.Date(NIRI$FECHAINICIO,format="%Y-%m-%d")
# NIRI$FECHA_DECISION <- lubridate::parse_date_time(NIRI$FECHA_DECISION,"dmy") # LLama libreria lubridate para cambiar el formato de la columna indicada #
# NIRI$FECHA_DECISION <- as.Date(NIRI$FECHA_DECISION,format="%Y-%m-%d")

# NIRI$HOY <- Sys.Date()
# NIRI$RESTA <- NIRI$HOY - NIRI$FECHAINICIOS
NIRIGRAL <- NIRI #Se crea un nuevo dataframe#
names(NIRIGRAL) #nombre de las columnas #
NIRIGRAL$DIRECCION <- NULL #Se elimina la columna indicada#
NIRIGRAL$COLONIA <- NULL
NIRIGRAL$LOCALIDAD <- NULL
NIRIGRAL$ESTATUS_SOLICITUD <- NULL
NIRIGRAL$ETIQUETA <- NULL
NIRIGRAL$OBSERVACIONES <-NULL
NIRIGRAL$CURSO <- NULL
NIRIGRAL$FECHA_ENROLAMIENTO <- NULL
NIRIGRAL <- NIRIGRAL[!duplicated(NIRIGRAL),]#Quita duplicados#
NIRIGRAL$descendiente <- xtfrm(NIRIGRAL$FECHA_DECISION)#Convierte fecha a valor numerico#
NIRIGRAL <- NIRIGRAL[with(NIRIGRAL,order(-NIRIGRAL$descendiente)),]# Ordena de mas reciente a mas antiguos#
NIRIGRAL <- NIRIGRAL %>% group_by(MATRICULA)%>% dplyr::mutate(conteos = row_number()) #agrupa y cuenta con row_number#
NIRIGRAL <- subset(NIRIGRAL,conteos == 1) #Filtra todo lo que es igual a 1#
NIRIGRAL$descendiente <- NULL
NIRIGRAL$conteos <-NULL
NIRIGRAL$PARTE <- NULL
NIRIGRAL$ESTATUS_EGRESO <- NULL
names(NIRIGRAL)[9]="FECHA_INICIO"
NIRIGRAL$INGRESO_MENSUAL <- NULL
NIRIGRAL <- NIRIGRAL %>% filter((grepl('FUTURO', .$TIPO)
                         | grepl('NUEVO INGRESO', .$TIPO)))


# # NIRIGRAL1 <- subset(NIRIGRAL,TIPO == "FUTURO")
# # NIRIGRAL2 <- subset(NIRIGRAL,TIPO == "NUEVO INGRESO")  
# # NIRIGRAL <- rbind(NIRIGRAL1,NIRIGRAL2)
# NIRIGRAL$ano <- lubridate::year(NIRIGRAL$FECHA_INICIO) #extrae solo el año"
Li <- subset(NIRIGRAL,NIVEL == "LI")

LIC<- subset(Li,DECISION==35)
LIC <- LIC %>% filter((grepl('UVE', .$CAMPUS)
                                 | grepl('UMM', .$CAMPUS) #Filtramos matriculados y admitidos#
                                    | grepl('UNI', .$CAMPUS) #Filtramos matriculados y admitidos#
                                      | grepl('UNA', .$CAMPUS) #Filtramos matriculados y admitidos#
                                        | grepl('UIN', .$CAMPUS) #Filtramos matriculados y admitidos#
                                            | grepl('INC', .$CAMPUS))) #Filtramos matriculados y admitidos#

x <- names(LIC)
Inicio1 <- LIC %>%
  select(x) %>%
  filter(FECHA_INICIO == as.Date(datest))

Futuros <- LIC %>%
  select(x) %>%
  filter(FECHA_INICIO == as.Date(datestfuturo))


IniciosP <- rbind(Inicio1,Futuros)

Prueba <- grep("Prueb",IniciosP$ESTUDIANTE, ignore.case = FALSE) #Filtra lo que esté en las comillas dentro de la columna indicada#
IniciosP2 <- IniciosP[Prueba,]#Quita todo lo que diga Prueb#

IniciosP <-subset(IniciosP,IniciosP$MATRICULA%in%IniciosP2$MATRICULA==FALSE)

# 
# seniorh<-read.csv("//Tut-095/reportes/ESTADOS/Asignacion Posgrados/Etiquetas_Historico.csv", header = TRUE,sep = ",",encoding="UTF-8",stringsAsFactors = FALSE)
# seniorh2<-read.csv("//Tut-095/reportes/ESTADOS/Asignacion Posgrados/Etiquetas_Historico2.csv", header = TRUE,sep = ",",encoding="UTF-8",stringsAsFactors = FALSE)
# seniormarzo<-read.csv("//Tut-095/reportes/ESTADOS/Asignacion Posgrados/EtiquetasMarzo21.csv", header = TRUE,sep = ",",encoding="UTF-8",stringsAsFactors = FALSE)
# seniormayo<-read.csv("//Tut-095/reportes/ESTADOS/Asignacion Posgrados/EtiquetasMayo21.csv", header = TRUE,sep = ",",encoding="UTF-8",stringsAsFactors = FALSE)
# seniorjulio<-read.csv("//Tut-095/reportes/ESTADOS/Asignacion Posgrados/EtiquetasJulio21.csv", header = TRUE,sep = ",",encoding="UTF-8",stringsAsFactors = FALSE)
# seniorseptiembre<-read.csv("//Tut-095/reportes/ESTADOS/Asignacion Posgrados/EtiquetasSeptiembre21.csv", header = TRUE,sep = ",",encoding="UTF-8",stringsAsFactors = FALSE)
# seniornoviembre<-read.csv("//Tut-095/reportes/ESTADOS/Asignacion Posgrados/EtiquetasNoviembre21.csv", header = TRUE,sep = ",",encoding="UTF-8",stringsAsFactors = FALSE)




# senior <- rbind(seniorh,seniorh2,seniormarzo,seniormayo,seniorjulio,seniorseptiembre,seniornoviembre)
# ##write.csv(senior,file="G:Mi unidad/Contencion/ventas_paquetes_etiquetas_historico.csv",row.names = F)##

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




#Cargamos la asignación de ETCEL  
  
Etcel <- read_excel("C:/Users/egarcigo/Desktop/Reportes/Licenciatura/Asignacion/Asignacion_Lic.xlsm", 
                    sheet = "Alumnos", range = "A2:BA99000")






EtcelListado <- dplyr::select(Etcel,matricula,`Clave Programa`)
EtcelListado$Check <- "Ok"
EtcelListado <- tidyr::unite(EtcelListado,"Matr",c(matricula,`Clave Programa`),sep="")
IniciosP$mat <- IniciosP$MATRICULA
IniciosP$PRO  <- IniciosP$PROGRAMA
IniciosP <- tidyr::separate(IniciosP,PRO,c("CODE_PROG","b","c","d","e","f"),sep=" ")
IniciosP$clave_Prog <- IniciosP$CODE_PROG
IniciosP$b <- NULL
IniciosP$c <- NULL
IniciosP$d <- NULL
IniciosP$e <- NULL
IniciosP$f <- NULL
IniciosP <- tidyr::unite(IniciosP,"Matr",c(mat,CODE_PROG),sep="")
IniciosP <- dplyr::left_join(IniciosP,EtcelListado, by="Matr")
IniciosP <- mutate_at(IniciosP,c("Check"),~replace(.,is.na(.),"Nosta"))
IniciosP <- subset(IniciosP,Check != "Ok" & ESTATUS == "MATRICULADO" |
                      Check != "Ok" & ESTATUS == "ADMITIDO")


Online<- IniciosP
Plantel<-read.csv("C:/Users/egarcigo/Desktop/Reportes/Licenciatura/Asignacion/Plantel.csv", header = TRUE,sep = ",",encoding="UTF-8",stringsAsFactors = FALSE)


Online <- left_join(Online,Plantel,by="CANAL_FINAL")
Online <- mutate_at(Online,c("Plantel"),~replace(.,is.na(.),"Mèxico"))
Online$CRM <-"-"
Online$Modalidad <- "Licenciatura"
Online$matricula <- Online$MATRICULA
Online$Nombre <- Online$ESTUDIANTE
Online$`Tutor Anterior` <- ""
Online$Tutor <- ""
Online$Supervisor <-""
Online$Etiqueta_V2 <- ifelse(Online$FECHA_INICIO == as.Date(datest),"Bimestre 01_B","Bimestre 01_C")
Online$Turno<- ""
Online$Estado <- ifelse(Online$FECHA_INICIO == as.Date(datest),"Activo","Activo")
Online$Equivalencia<- Online$TIPO_INGRESO
Online$Q_V2 <- ifelse(Online$FECHA_INICIO == as.Date(datest),"NI (14/03/2022) B","NI (28/03/2022) C")
Online$Fecha_ingreso <- Online$FECHA_REGISTRO
Online$`Fecha inicio` <- Online$FECHA_INICIO
Online$`Fecha de Asignación` <- Sys.Date()
Online$`Asignación nuevo tutor` <- Sys.Date()
Online$Modalidad1 <- "Licenciatura"
Online$Duracion <- "Cuatrimestral"
Online$Programa <- Online$PROGRAMA
Online <- tidyr::separate(Online,PROGRAMA,c("Clave Programa"),sep=" ")
Online$Programa <- substring(Online$Programa,11)
Online$`Fecha cambio de Edo` <- Online$FECHA_ACTIVIDAD
Online$Canal <- Online$CANAL_FINAL
Online$Jornada <- "Sin Registro"
Online$`%_Avance` <- "Sin Registro"
Online$Rango_Avance <- "Sin Registro"
Online$ Adeudo <- "Sin Registro"
Online$ Rango_Adeudo <- "Sin Registro"
Online$Mora <- "Sin Regstro"
Online$Docs <- "Sin Registro"
Online$`# Contacto Efectivo` <- "Sin Registro"
Online$`# Intentos Contacto` <- "Sin Registro"
Online$Rango_Cont_Efec <- "Sin Registro"
Online$Rango_Int_Cont <- "Sin Registro"
Online$Contactación <- "Sin Registro"
Online$Acc_Materia <- "Sin Registro"
Online$`Acc_Auvi_` <- "Sin Registro"
Online$Indicadores <- "Sin Registro"
Online$`SEMAFORO MARZO-ABRIL` <- "Sin Registro"
Online$`SEMAFORO NOVIEMBRE-DICIEMBRE`  <- "Sin Registro"
Online$`SEMAFORO SEPTIEMBRE-OCTRUBRE` <- "Sin Registro"
Online$Last_date <- "Sin Registro"
Online$Status_medio <- "Sin Registro"
Online$MEDIO <- "Sin Registro"
Online$`Tutor que contacto`<- "Sin Registro"
Online$Retención <- "Sin Registro"
Online$Entidad <- "Sin Registro"
Online$`Correo Tutor` <- "Sin Registro"
Online$Semana <- "Sin Registro"
Online$Etiqueta <- "Sin Registro"
Online$Fecha <- "Sin Registro"
Online$`No Empleado` <- "Sin Registro"
Online$`No materias` <- "Sin Registro"

Online <- ungroup(Online)
Online <- dplyr::select(Online,Plantel,CRM,Modalidad,matricula,Nombre,`Tutor Anterior`,Tutor,Supervisor,Etiqueta_V2,Turno,Estado,Equivalencia,
                        Q_V2,Fecha_ingreso,`Fecha inicio`,`Fecha de Asignación`,`Asignación nuevo tutor`,Modalidad1,Duracion,`Clave Programa`,
                        Programa,`Fecha cambio de Edo`,Canal,Jornada,`%_Avance`,Rango_Avance,Adeudo,Rango_Adeudo,Mora,Docs,
                        `# Contacto Efectivo`,`# Intentos Contacto`,Rango_Cont_Efec,Rango_Int_Cont,Contactación,Acc_Materia,`Acc_Auvi_`,
                        Indicadores,`SEMAFORO MARZO-ABRIL`,`SEMAFORO NOVIEMBRE-DICIEMBRE`,`SEMAFORO SEPTIEMBRE-OCTRUBRE`,Last_date,Status_medio,
                        MEDIO,`Tutor que contacto`,Retención,Entidad,`Correo Tutor`,Semana,Etiqueta,Fecha,`No Empleado`,`No materias`)



x <- names(Etcel)
colnames(Online)<- x
# 
# Online$Fecha_ingreso <- lubridate::parse_date_time(Online$Fecha_ingreso,c("dmy"))
# Online$`Fecha de Asignación` <- lubridate::parse_date_time(Online$`Fecha de Asignación`,c("dmy"))
# Online$`Asignación nuevo tutor` <- lubridate::parse_date_time(Online$`Asignación nuevo tutor`,c("dmy"))
# Online$Fecha inicio <- lubridate::date(Online$Fecha inicio)
# Online$`Fecha cambio de Edo` <- lubridate::parse_date_time(Online$`Fecha cambio de Edo`,c("dmy"))
# Etcel$`Fecha Inscripción` <- lubridate::date(Etcel$`Fecha Inscripción`)
# Online$`Fecha Inscripción` <- lubridate::parse_date_time(Online$`Fecha Inscripción`,c("dmy"))
# Online$F.Decisón <- lubridate::parse_date_time(Online$F.Decisón,c("dmy"))
# Online[,58] <- as.numeric(as.character(Online[,58]))
# Online <- mutate_at(Online,c("TELEFONOS"),~replace(.,is.na(.),0))
# 
# 
#write.csv(Online,file ="D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/in.csv",row.names=F)

names(Online)

asignacion <- rbind(Etcel,Online)
asignacion <- asignacion[!is.na(asignacion$matricula),]
write.csv(asignacion,file = "C:/Users/egarcigo/Desktop/Reportes/Licenciatura/Asignacion/Asig.csv",row.names = FALSE)

#asignacion <- dplyr::bind_rows(list(Etcel,Online))


EDOGRAL <-read.csv("//Tut-095/reportes/ESTADOS/ESTADOGENERAL.csv", header = TRUE,sep = ",",encoding="UTF-7",stringsAsFactors = FALSE)

asignacion$Matri <- asignacion$matricula
asignacion$clav <- asignacion$`Clave Programa`
asignacion <- tidyr::unite(asignacion,"Matr",c(Matri,clav),sep="")

EDOGRAL <- dplyr::select(EDOGRAL,Matr,ESTATUS,FECHA_ESTATUS)
EDOGRAL$FECHA_ESTATUS <- lubridate::parse_date_time(EDOGRAL$FECHA_ESTATUS,c("dmy"))
EDOGRAL <- EDOGRAL[!duplicated(EDOGRAL),]
asignacion <- dplyr::left_join(asignacion,EDOGRAL,by="Matr")
asignacion$Estado <-  ifelse(asignacion$ESTATUS == "MATRICULADO", "Activo",ifelse(asignacion$ESTATUS == "BAJA TEMPORAL", "Baja"
                                                                                  ,ifelse(asignacion$ESTATUS == "BAJA DEFINITIVA", "Baja",
                                                                                          ifelse(asignacion$ESTATUS == "EGRESADO", "Egresado",
                                                                                                 ifelse(asignacion$ESTATUS == "ADMITIDO", "Activo",
                                                                                                        ifelse(asignacion$ESTATUS == "CANCELACION DE VENTA", "CV",
                                                                                                               ifelse(asignacion$ESTATUS == "BAJA INACTIVO", "Baja","-")))))))
asignacion$ESTATUS <- NULL
asignacion$`Fecha cambio de Edo` <- asignacion$FECHA_ESTATUS
asignacion$FECHA_ESTATUS <- NULL
asignacion$PAIS <- NULL





activos1 <- subset(asignacion, Estado == "Activo" & `Etiqueta_V2` == "Bimestre 01_D")
activos2 <- subset(asignacion, Estado == "Activo" & `Etiqueta_V2` == "Bimestre 01_A")
activos3 <- subset(asignacion, Estado == "Activo" & `Etiqueta_V2` == "Bimestre 01_B")
activos4 <- subset(asignacion, Estado == "Activo" & `Etiqueta_V2` == "Bimestre 01_C")

noactivos1 <- subset(asignacion, `Etiqueta_V2` != "Bimestre 01_D")
noactivos2 <- subset(asignacion, `Etiqueta_V2` != "Bimestre 01_A")
noactivos3 <- subset(asignacion, `Etiqueta_V2` != "Bimestre 01_B")
noactivos4 <- subset(asignacion, `Etiqueta_V2` != "Bimestre 01_C")



activos <- rbind(activos1,activos2,activos3,activos4)
noactivos <- rbind(noactivos1,noactivos2,noactivos3,noactivos4)

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
  mutate(FECHAINICIO = if_else(is.na(FECHAINICIO), `Fecha inicio`, FECHAINICIO))
activos$FECHAINICIO[is.na(activos$FECHAINICIO)]<- activos$`Fecha inicio`
#asignacion <- mutate_at(asignacion,c("FECHAINICIO"),~replace(.,is.na(.),asignacion$Fecha inicio))
activos$CC <- ifelse(activos$FECHAINICIO > activos$`Fecha inicio`,"CC" ,"Act")
activos$Estado <- ifelse(activos$CC == "CC" ,activos$CC,activos$Estado)
activos <- activos %>%
  mutate(Inicio_C = if_else(activos$CC == "CC", FECHAINICIO, `Fecha inicio`))


activos$`Fecha inicio` <- activos$Inicio_C
activos$Inicio_C <- NULL
activos$CC <- NULL
activos$FECHAINICIO <- NULL
asignacion <- rbind(activos,noactivos)

write.csv( asignacion, file = "C:/Users/egarcigo/Desktop/Reportes/Licenciatura/Asignacion/asig1.0.csv", row.names = FALSE)


#asignacion <- dplyr::left_join(asignacion,retencion,by="Matricula")
#signacion <- mutate_at(asignacion,c("SOLICITUD"),~replace(.,is.na(.),"-"))
#asignacion$Retención <- asignacion$SOLICITUD
#asignacion$SOLICITUD <- NULL

# Sabana <-read.csv("//Tut-095/reportes/FRONT/sabana.csv", header = TRUE,sep = ",",encoding="UTF-7",stringsAsFactors = FALSE)
# Sabana <- subset(Sabana,edo_moodle == "Activo")
# Sabana <- dplyr::select(Sabana,Matricula,Semana,Acceso.Materia,Acceso.Aula)
# Sabana$Acceso.Materia <- lubridate::parse_date_time(Sabana$Acceso.Materia,"ymd HM")
# Sabana$Acceso.Aula <- lubridate::parse_date_time(Sabana$Acceso.Aula,"ymd HM")
# Sabana <- mutate_at(Sabana,c("Acceso.Materia","Acceso.Aula"),~replace(.,is.na(.),"2021-01-01 00:00:00"))
# Sabana$desciende <- xtfrm(Sabana$Acceso.Materia)
# Sabana <- Sabana[with(Sabana,order(-Sabana$desciende)),]
# Sabana <- Sabana %>% group_by(Matricula)%>% dplyr::mutate(conteos = row_number())
# Sabana <- subset(Sabana,conteos == 1)
# Sabana$desciende <- NULL
# Sabana$conteos <- NULL
# Sabana$Acceso.Materias <- as.character(Sabana$Acceso.Materia)
# Sabana$Acceso.Materias <- ifelse(Sabana$Acceso.Materias == "2021-01-01 06:00:00","Nunca",Sabana$Acceso.Materias) 
# Sabana$Acceso.Aulas <- as.character(Sabana$Acceso.Aula)
# Sabana$Acceso.Aulas <- ifelse(Sabana$Acceso.Aulas == "2021-01-01 06:00:00","Nunca",Sabana$Acceso.Aulas) 
# Sabana$S <- "Semana"
# Sabana <- tidyr::unite(Sabana,"Semanas",c(S,Semana),sep="")
# Sabana$Acceso.Materia <- Sabana$Acceso.Materias
# Sabana$Acceso.Materias <- NULL
# Sabana$Acceso.Aula <- Sabana$Acceso.Aulas
# Sabana$Acceso.Aulas <- NULL
# asignacion <- dplyr::left_join(asignacion,SabanaMae,by="Matricula")
# asignacion <- mutate_at(asignacion,c("Acceso.Materia","Acceso.Aula"),~replace(.,is.na(.),"Sin Asignaturas"))
# asignacion <- mutate_at(asignacion,c("Semanas"),~replace(.,is.na(.),"-"))
# asignacion$Acc_Materia <- asignacion$Acceso.Materia
# asignacion$Acc_Materia <- asignacion$Acceso.Aula
# asignacion$Semana <- asignacion$Semanas
# asignacion$Acc_Mat <- ifelse(asignacion$Acc_Materia == "Nunca","Nunca",
#                             ifelse(asignacion$Acc_Materia == "Sin Asignaturas",
#                                    "Sin Asignaturas","Activo"))
# asignacion$Semanas<-NULL
# signacion$Acceso.Materia <- NULL
# asignacion$Acceso.Aula <- NULL


NR <-read.csv("//Tut-095/reportes/FRONT/niveles_de_riesgo.csv", header = TRUE,sep = ",",encoding="UTF-7",stringsAsFactors = FALSE)
NR$X <- NULL
NR <- subset(NR,edomoddle == "Activo")
NRS <- dplyr::select(NR,matricula,Semaforo,Total.general)
# asignaturas <- dplyr::select(NR,Matricula,Aprobado,Np,Nuncas,Por.aprobar,Reprobó,Total.general)
# materias <- asignaturas
# asignaturas <- tidyr::gather(asignaturas, key="nivel",value="num",2:6)
# asignaturas <- subset(asignaturas,num != 0)
# asignaturas <- tidyr::unite(asignaturas,"Mat",c(num,nivel),sep=" ")
# #asignaturas <- rbind(Bienvenida,asignaturas)
# asignaturas <- asignaturas %>% group_by(Matricula) %>% dplyr::mutate(cuenta = row_number())
# uno <- subset(asignaturas,cuenta == 1)
# dos <- subset(asignaturas,cuenta == 2)
# tres <- subset(asignaturas,cuenta == 3)
# cuatro <- subset(asignaturas,cuenta == 4)
# cinco <- subset(asignaturas,cuenta == 5)
# names(dos)[3]="Mat2"
# names(tres)[3]="Mat3"
# names(cuatro)[3]="Mat4"
# dos <- dplyr::select(dos,Matricula,Mat2)
# tres <- dplyr::select(tres,Matricula,Mat3)
# cuatro <- dplyr::select(cuatro,Matricula,Mat4)
# asignaturas <- dplyr::left_join(uno,dos,by="Matricula")
# asignaturas <- dplyr::left_join(asignaturas,tres,by="Matricula")
# asignaturas <- dplyr::left_join(asignaturas,cuatro,by="Matricula")
# asignaturas <- dplyr::select(asignaturas,Matricula,Mat,Mat2,Mat3,Mat4)
# asignaturas <- mutate_at(asignaturas,c("Mat","Mat2","Mat3","Mat4"),~replace(.,is.na(.),""))
# asignaturas <- tidyr::unite(asignaturas,"asigna",c(Mat,Mat2,Mat3,Mat4),sep="-")
# asignaturas$asigna<-gsub("--","",asignaturas$asigna)
# asignaturas$asigna<-gsub("---","",asignaturas$asigna)
# asignaturas$asigna<-gsub("- ","",asignaturas$asigna)
# asignaturas$asigna<-gsub("- ","",asignaturas$asigna)
# asignaturas$asigna<-gsub("-"," ",asignaturas$asigna)
# 
# materias$clasificacion <- ifelse(materias$Nuncas >= 1,"Nuncas",
#                                  ifelse(materias$Np >=1,"Np",
#                                         ifelse(materias$Reprobó >=1,"Reprobó",
#                                                "Aprobado")))
# materias <- dplyr::select(materias,Matricula,clasificacion)
# asignaturas <- dplyr::left_join(asignaturas,materias,by="Matricula")
# asignaturas <- dplyr::left_join(asignaturas,NRS,by="Matricula")
# asignaturas <- asignaturas[!duplicated(asignaturas),]
#write.csv(asignaturas,file ="D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/asignaturas.csv",row.names=F)
asignacion <- dplyr::left_join(asignacion,NRS,by="matricula")
asignacion <- mutate_at(asignacion,c("Semaforo"),~replace(.,is.na(.),"Sin Asignaturas"))
asignacion <- mutate_at(asignacion,c("Total.general"),~replace(.,is.na(.),0))
asignacion$Indicadores <-asignacion$Semaforo
asignacion$`No materias` <- asignacion$Total.general
asignacion$Semaforo <-NULL
asignacion$Total.general <-NULL

# Contactacion <-read.csv("G:Mi unidad/Contactacion/Conteos_Interacciones.csv", header = TRUE,sep = ",",encoding="UTF-7",stringsAsFactors = FALSE)
# Contactacion <- Contactacion[!duplicated(Contactacion),]
# Contactacion <- dplyr::select(Contactacion,matricula,Contacto.efectivo,Intento,Fecha_ultimo_contacto,Tutor.y,Medio)
# names(Contactacion)[1]="Matricula"
# Contactacion <- Contactacion[!duplicated(Contactacion),]
# Contactacion <- Contactacion[with(Contactacion,order(-Contactacion$Contacto.efectivo)),]
# 
# Contactacion <- Contactacion %>% group_by(Matricula)%>% dplyr::mutate(conteos = row_number())
# Contactacion <- subset(Contactacion,conteos == 1)
# asignacion <- dplyr::left_join(asignacion,Contactacion,by="Matricula")
# asignacion <- mutate_at(asignacion,c("Contacto.efectivo","Intento"),~replace(.,is.na(.),0))
# asignacion <- mutate_at(asignacion,c("Fecha_ultimo_contacto","Tutor.y","Medio"),~replace(.,is.na(.),"-"))
# asignacion$`# Contacto Efectivo` <- asignacion$Contacto.efectivo
# asignacion$`# Intentos Contacto` <- asignacion$Intento
# asignacion$Contactación <- ifelse(asignacion$`# Contacto Efectivo` > 0,"Contactado",
#                                ifelse(asignacion$`# Intentos Contacto` > 0, "No Contactado",
#                                       "Sin Intento"))

# asignacions <- asignacion
# repro <- read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/listado.csv", header = TRUE, sep = ",")
# levlab <- read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/levelslabels.csv", header = TRUE, sep = ",")
# v <- repro$Interacciones
# asignacions <- cbind(asignacions, findInterval(asignacions$`# Contacto Efectivo`, v))
# asignacions <- data.frame(asignacions)


# asignacions$Rango_Cont_Efec <- factor(asignacions$findInterval.asignacions....Contacto.Efectivo...v., 
#                                  levels = levlab$levels,
#                                  labels = levlab$labels)
# 
# asignacions <- cbind(asignacions, findInterval(asignacions$X..Intentos.Contacto, v))
# asignacions <- data.frame(asignacions)
# 
# asignacions$Rango_Int_Cont <- factor(asignacions$findInterval.asignacions.X..Intentos.Contacto..v., 
#                                       levels = levlab$levels,
#                                       labels = levlab$labels)
# 
# asignacion <- asignacions
# asignacion$Fecha.Ultimo.Contacto <- asignacion$Fecha_ultimo_contacto
# asignacion$Tutor.que.contacto <- asignacion$Tutor.y
# asignacion$Medio.de.Contacto <- asignacion$Medio
# asignacion$Contacto.efectivo <- NULL
# asignacion$Intento <- NULL
# asignacion$Fecha_ultimo_contacto <- NULL
# asignacion$Tutor.y <- NULL
# asignacion$Medio <- NULL
# asignacion$conteos <- NULL
# asignacion$findInterval.asignacions....Contacto.Efectivo...v.<- NULL
# asignacion$findInterval.asignacions.X..Intentos.Contacto..v. <- NULL

# Financierosir <-read.csv("//Tut-095/REPORTES/ESTADOS/FinancieroGeneral.csv", header = TRUE,sep = ",",encoding="UTF-8")
# Financiero <- Financierosir
# Financiero <- select(Financiero,MATRICULA,MAIL_PRIN)
# Financiero[,1] <- as.numeric(as.character(Financiero[,1]))
# Financierosir <- select(Financierosir,MATRICULA,SALDO_TOTAL)
# Financierosir[,1] <- as.numeric(as.character(Financierosir[,1]))
# Financierosir <- Financierosir[!duplicated(Financierosir),]
# names(Financierosir)[1]="Matricula"
# asignacion <- dplyr::left_join(asignacion,Financierosir,by="Matricula")
# asignacion$Adeudo <- asignacion$SALDO_TOTAL
# asignacion$SALDO_TOTAL <- NULL


# asignacions <- asignacion
# repro <- read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/listado2.csv", header = TRUE, sep = ",")
# levlab <- read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Asignacion Posgrados/levelslabels2.csv", header = TRUE, sep = ",")
# v <- repro$Rango_Adeudo
# asignacions <- cbind(asignacions, findInterval(asignacions$Adeudo, v))
# asignacions <- data.frame(asignacions)
# 
# 
# asignacions$Rango_Adeudo <- factor(asignacions$findInterval.asignacions.Adeudo..v., 
#                                       levels = levlab$levels,
#                                       labels = levlab$labels)
# 
# asignacion <- asignacions
# asignacion$findInterval.asignacions.Adeudo..v. <- NULL

# accstatus <-read.csv("G:Mi unidad/reports/acc_status.csv", header = TRUE,sep = ",",encoding="UTF-7",stringsAsFactors = FALSE)
# AULA12 <- subset(accstatus,aula == "Aula12")
# AULA12$Num <- 12
# AULA13 <- subset(accstatus,aula == "Aula13")
# AULA13$Num <- 13
# accstatus <- rbind(AULA12,AULA13)
# accstatus <- accstatus[with(accstatus,order(-accstatus$Num)),]
# accstatus <- dplyr::select(accstatus,Matricula,id,aula)
# accstatus <- accstatus[!duplicated(accstatus),]
# accstatus <- accstatus %>% group_by(Matricula)%>% dplyr::mutate(conteos = row_number())
# accstatus <- subset(accstatus,conteos == 1)
# accstatus$conteos <- NULL 
# AULA12 <- subset(accstatus,aula == "Aula12")
# AULA13 <- subset(accstatus,aula == "Aula13")
# AULA12$link12 <- "https://aula12.utel.edu.mx/user/profile.php?id="
# AULA13$link13 <- "https://aula13.utel.edu.mx/user/profile.php?id="
# AULA12 <- tidyr::unite(AULA12,"linkaula",c(link12,id),sep="")
# AULA13 <- tidyr::unite(AULA13,"linkaula",c(link13,id),sep="")
# accstatus <- rbind(AULA12,AULA13)
# accstatus$aula <- NULL
# asignacion <- dplyr::left_join(asignacion,accstatus,by="Matricula")
# asignacion$LINK.AULA <- asignacion$linkaula
# asignacion$linkaula <- NULL
# asignacion <- mutate_at(asignacion,c("LINK.AULA"),~replace(.,is.na(.),"-"))




alumnos <- read.csv( file = "C:/Users/egarcigo/Desktop/Reportes/Licenciatura/Asignacion/IDCRM.csv")
names(alumnos)[1]="matricula"

alumnos$descendiente <- xtfrm(alumnos$fecha_creacion)
alumnos <- alumnos[with(alumnos,order(-alumnos$descendiente)),]
alumnos <- alumnos %>% group_by(matricula)%>% dplyr::mutate(conteos = row_number())
alumnos <- subset(alumnos,conteos == 1)
alumnos$descendiente <- NULL
alumnos$conteos <- NULL
alumnos <- dplyr::select(alumnos,matricula,ID_CRM)
alumnos$link <- "http://crm.utel.edu.mx/index.php?module=Contacts&action=DetailView&record="
alumnos <- tidyr::unite(alumnos,"linkcrm",c(link,ID_CRM),sep="")
asignacion <- dplyr::left_join(asignacion,alumnos,by="matricula")
asignacion <- mutate_at(asignacion,c("linkcrm"),~replace(.,is.na(.),"-"))
asignacion$CRM <- asignacion$linkcrm
asignacion$linkcrm <- NULL

asignacion <- asignacion[!duplicated(asignacion),]
prueb <- grep("prueb",asignacion$Nombre, ignore.case = TRUE)
eba <- asignacion[prueb,]

asignacion <- subset(asignacion, asignacion$matricula%in%eba$matricula==F)



asignacion$Matr <- NULL

write.csv(asignacion,file ="C:/Users/egarcigo/Desktop/Reportes/Licenciatura/Asignacion/Asig_lic.csv",row.names = FALSE)



