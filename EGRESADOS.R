library(dplyr)
library(tidyr)
library(lubridate)
library(stringi)

#t <- proc.time()
#Funciones
#Variables generales
c_Egre <- "D:/Cosa/SIR/Egresados/"
c_Desc <- paste(c_Egre,"Descargas/",sep = "")
c_info <- paste(c_Egre,"Info/",sep = "")
file.rename(paste(c_Egre,"Egresados.csv",sep = ""), paste(c_Egre,"Egresados_Anterior.csv",sep = ""))
#####--------------------------------------------------Estatus General Alumnos--------------------------------------------------#####
Est_Gen <- read.csv(paste(c_Desc,"Estatus_General_Alumno.csv",sep = "")) %>% mutate_all(~stri_trim(.)) %>% mutate_all(~as.character(.))%>% 
  mutate(MATRICULA=if_else(nchar(MATRICULA)==8,paste("0",MATRICULA,sep = ""),MATRICULA),CLAVE_PROG=substring(PROGRAMA,1,10),PROGRAMA=substring(PROGRAMA,11,50))%>%
  unite(MATPROG,MATRICULA,CLAVE_PROG,sep = "",remove = F)
#####--------------------------------------------------Avance Curricular--------------------------------------------------#####
Aux <- read.csv(paste(c_Desc,"Avance_Curricular.csv",sep = "")) %>% mutate(MATRICULA=as.character(MATRICULA)) %>% select(MATRICULA, PROGRAMA, AVANCE_CURRICULAR, PROMEDIO)%>%
  mutate(MATRICULA=if_else(nchar(MATRICULA)==8,paste("0",MATRICULA,sep = ""),MATRICULA)) %>% unite(MATPROG,MATRICULA,PROGRAMA, sep = "") %>% 
  reshape::rename(c(AVANCE_CURRICULAR="Avance",PROMEDIO="Promedio"))

Aux$TIP_Promedio <- if_else((Aux$Promedio>=6 & Aux$Promedio<=6.99)|(Aux$Promedio>=60 & Aux$Promedio<=69.99),"6 a 6.99",
                            if_else((Aux$Promedio>=7 & Aux$Promedio<=7.99)|(Aux$Promedio>=70 & Aux$Promedio<=79.99),"7 a 7.99",
                                    if_else((Aux$Promedio>=8 & Aux$Promedio<=8.99)|(Aux$Promedio>=80 & Aux$Promedio<=89.99),"8 a 8.99",
                                            if_else((Aux$Promedio>=9 & Aux$Promedio<=9.99)|(Aux$Promedio>=90 & Aux$Promedio<=99.99),"9 a 9.99",
                                                    ifelse(Aux$Promedio==10 | Aux$Promedio==100,"10","_")))))
Est_Gen <- left_join(Est_Gen, Aux, by ='MATPROG') %>% mutate_at(c("Avance", "Promedio", "TIP_Promedio"),~replace(.,is.na(.),"_"))
#####--------------------------------------------------TIP_Estatus--------------------------------------------------#####
Aux <- readxl::read_excel(paste(c_info,"Info_general.xlsx",sep = ""),sheet = "Tip_Estatus")
Est_Gen <- left_join(Est_Gen,Aux,by="ESTATUS") %>% mutate(TIP_Estatus=replace(TIP_Estatus,is.na(TIP_Estatus),"Prospecto"))
#####------------------------------------------Fecha de Estatus y primer inscripción (separación)------------------------------------------#####
Aux <- select(Est_Gen,FECHA_ESTATUS,PRIMER_INSCRIPCION_SIN_ESTATUS) %>% reshape::rename(c(FECHA_ESTATUS="Fecha_e",PRIMER_INSCRIPCION_SIN_ESTATUS="Fecha_i"))%>%
  mutate_at(c("Fecha_e","Fecha_i"),~dmy(.)) %>% mutate(DIA=day(Fecha_e),MES=format(Fecha_e,"%B"),AÑO=year(Fecha_e),
                                                       DIA_INS=day(Fecha_i),MES_INS=format(Fecha_i,"%B"),AÑO_INS=year(Fecha_i))%>%
  mutate_at(c("DIA","AÑO","DIA_INS","AÑO_INS"),~replace(.,is.na(.),0)) %>% mutate_at(c("MES","MES_INS"),~if_else(is.na(.),"_",.))
Est_Gen <- cbind(Est_Gen,Aux) %>% select(-Fecha_e,-Fecha_i)
Aux <- mutate(Aux, MES_INS=month(Fecha_i))
#####------------------------------------------Cálculos con fechas------------------------------------------#####
Aux <- select(Aux, MES_INS, AÑO_INS, Fecha_e, Fecha_i) %>% mutate(dif_tiempo=interval(Fecha_e,Fecha_i),dif_tiempo=as.period(dif_tiempo),
                                                                  dif_tiempo=gsub("-","",dif_tiempo),dif_tiempo=as.period(dif_tiempo))
#Tipificación de eficiencia.
Tip_efi <- c("1m","1y","4y","7y","10y","13y","16y")
Tip_efi <- as.period(Tip_efi)
Aux$TIP_EFICIENCIA <- if_else(Aux$dif_tiempo<Tip_efi[1], "Días",
                              if_else(Aux$dif_tiempo<Tip_efi[2], "Meses",
                                      if_else(Aux$dif_tiempo<Tip_efi[3], "1 año a 3 años 11 meses",
                                              if_else(Aux$dif_tiempo<Tip_efi[4], "4 años a 6 años 11 meses",
                                                      if_else(Aux$dif_tiempo<Tip_efi[5], "7 años a 9 años 11 meses",
                                                              if_else(Aux$dif_tiempo<Tip_efi[6], "10 años a 12 años 11 meses",
                                                                      if_else(Aux$dif_tiempo<Tip_efi[7], "13 años a 15 años 11 meses","Indefinido")))))))
#EFICIENCIA...Separando
Aux <- separate(Aux, dif_tiempo,into = c("y","m","d"), sep = " ")
#Arreglando días transcurridos
Aux$d <- if_else(Aux$d=="0M",Aux$y,
                 if_else(Aux$d=="0H",Aux$m,Aux$d))
Aux$d <- gsub("d"," Días", Aux$d)
Aux$d <- if_else(substring(Aux$d,1,2)=="1 ",gsub(" Días", " Día", Aux$d),Aux$d)
#Arreglando meses transcurridos
Aux$m <- if_else(Aux$m=="0H","0",
                 if_else(substring(Aux$m,3,3)=="d"|substring(Aux$m,2,2)=="d",Aux$y,Aux$m))
Aux$m <- gsub("m"," Meses", Aux$m)
Aux$m <- if_else(substring(Aux$m,1,2)=="1 ",gsub(" Meses", " Mes", Aux$m),Aux$m)
#Arreglando años transcurridos y uniendo con meses
Aux$y <- ifelse(substring(Aux$y,2,2)=="y"|substring(Aux$y,3,3)=="y"|substring(Aux$y,4,4)=="y",Aux$y,"0")
Aux$y <- gsub("y"," Años", Aux$y)
Aux$y <- if_else(substring(Aux$y,1,2)=="1 ",gsub(" Años", " Año", Aux$y),Aux$y)
Aux <- unite(Aux, EFICIENCIA, y, m, sep = " ")
#Si los años y meses son 0, se reemplaza por días, se sustituyen valores innecesarios y se reemplazan los que no tienen fecha.
Aux$EFICIENCIA <- if_else(Aux$EFICIENCIA=="0 0",Aux$d,
                          if_else(substring(Aux$EFICIENCIA,1,2)=="0 ", gsub("0 ","",Aux$EFICIENCIA),
                                  if_else(Aux$EFICIENCIA=="NA NA","n",Aux$EFICIENCIA)))
Aux <- mutate_at(Aux, c("EFICIENCIA","TIP_EFICIENCIA"),~replace(.,is.na(.)|.=="n","_"))
#Se reemplaza texto innecesario por nada y se agrega la columna completa a la base general
Aux$EFICIENCIA <- if_else(substring(Aux$EFICIENCIA,6,13)==" 0 Meses"|
                            substring(Aux$EFICIENCIA,7,14)==" 0 Meses"|
                            substring(Aux$EFICIENCIA,8,15)==" 0 Meses"|
                            substring(Aux$EFICIENCIA,9,16)==" 0 Meses", gsub(" 0 Meses","",Aux$EFICIENCIA),Aux$EFICIENCIA)
Aux$EFICIENCIA <- if_else(Aux$EFICIENCIA=="1Meses","1 Mes",if_else(Aux$EFICIENCIA=="NA","Mismo día",Aux$EFICIENCIA))
#Generación
Aux$Generación <- if_else(Aux$MES_INS>=6 & Aux$AÑO_INS>=2010, paste("Generación",Aux$AÑO_INS+1, sep=" "),
                          if_else(Aux$MES_INS<=5 & Aux$AÑO_INS>=2010, paste("Generación",Aux$AÑO_INS, sep=" "), "_"))
#Perdiodo SEP
Aux$`Periodo SEP` <- ifelse(Aux$MES_INS>=2 & Aux$MES_INS<=5 & Aux$Generación!="_", paste("Mayo", Aux$AÑO_INS, sep=" "),
                            if_else(Aux$MES_INS>=6 & Aux$MES_INS<=9 & Aux$Generación!="_", paste("Septiembre", Aux$AÑO_INS, sep=" "),
                                    if_else(Aux$MES_INS>=10 & Aux$MES_INS<=12 & Aux$Generación!="_", paste("Enero", Aux$AÑO_INS+1, sep=" "),
                                            if_else(Aux$MES_INS==1 & Aux$Generación!="_",paste("Enero", Aux$AÑO_INS, sep=" "),"_"))))
#Agregando columnas a la base.
Aux <- select(Aux, EFICIENCIA, TIP_EFICIENCIA, Generación, `Periodo SEP`)
Est_Gen <- cbind(Est_Gen, Aux)
#####------------------------------------------Campus------------------------------------------#####
Aux <- readxl::read_excel(paste(c_info,"Info_general.xlsx",sep = ""),sheet = "Campus")
Est_Gen <- left_join(Est_Gen, Aux, by="CAMPUS")
#####------------------------------------------Académico Financiero (Descuento)------------------------------------------#####
Aux <- read.csv(paste(c_Desc,"Academico_Financiero.csv",sep = "")) %>% select(MATRICULA,PROGRAMA_CODE,DESCUENTO) %>% unite(MATPROG,MATRICULA,PROGRAMA_CODE,sep = "") %>% unique()
Est_Gen <- left_join(Est_Gen, Aux, by = "MATPROG") %>% mutate(DESCUENTO=as.character(DESCUENTO),DESCUENTO=if_else(is.na(DESCUENTO),"_",DESCUENTO))
#####------------------------------------------Solicitudes SSB (Pago Tit)------------------------------------------#####
Aux <- read.csv(paste(c_Desc,"Solicitudes_Generadas_por_SSB.csv",sep = "")) %>% filter((SERVICIO=="TITULACION"|SERVICIO=="COLEGIATURA FINAL"|
                                                                                          SERVICIO=="COLEGIATURA FINAL"|SERVICIO=="OBTENCION DE GRADO")&
                                                                                         (ESTATUS_SOLC=="PAGADO"))%>%
  select(MATRICULA,PROGRAMA) %>% separate(PROGRAMA,into=c("CLAVE","PROGRAMA"),sep = "-") %>% mutate(MATRICULA=as.character(MATRICULA),
                                                                                                    MATRICULA=if_else(nchar(MATRICULA)==8,paste("0",MATRICULA,sep = ""),MATRICULA))%>%
  unite(MATPROG,MATRICULA,CLAVE,sep = "") %>% reshape::rename(c(PROGRAMA="Pago_Tit")) %>% mutate(Pago_Tit="Pagado") %>% unique()
Est_Gen <- left_join(Est_Gen, Aux, by ="MATPROG") %>% mutate(Pago_Tit=if_else(is.na(Pago_Tit),"Sin pago",Pago_Tit))
#####------------------------------------------Servicio social------------------------------------------#####
Aux <- read.csv(paste(c_info,"Avance SS.csv",sep = "")) %>% select(MATRICULA,PROGRAMA,GLOBAL)%>%
  mutate(MATRICULA=if_else(nchar(MATRICULA)==8,paste("0",MATRICULA,sep = ""),MATRICULA)) %>% unite(MATPROG,MATRICULA,PROGRAMA,sep = "") %>% unique() %>% 
  reshape::rename(c(GLOBAL="Servicio Social")) %>% mutate(`Servicio Social`=if_else(`Servicio Social`=="Alinear/Liberado","Liberado",`Servicio Social`))
Est_Gen <- left_join(Est_Gen,Aux, by = "MATPROG") %>% mutate(`Servicio Social`=if_else(is.na(`Servicio Social`),"Pendiente",`Servicio Social`))
#####------------------------------------------Moy, Vic y Van------------------------------------------#####
#Documentos
Aux <- readxl::read_excel(paste(c_info,"Moy.xlsx",sep = ""), col_types = c("text", "text", "text"))%>%
  mutate(MATRICULA=if_else(nchar(MATRICULA)==8,paste("0",MATRICULA,sep = ""),MATRICULA)) %>% unite(MATPROG,MATRICULA,PROGRAMA,sep = "") %>% unique()
names(Aux)[2] <- "Documentos"

Est_Gen <- left_join(Est_Gen, Aux, by="MATPROG") %>% mutate(Documentos=if_else(is.na(Documentos),"Sin Expediente",Documentos))
#Cert y Tit
Aux <- readxl::read_excel(paste(c_info,"Vic.xlsx",sep = "")) %>% select(MATRICULA,CVE_PROGRAMA, c(17)) %>% mutate_all(~as.character(.))%>%
  mutate(MATRICULA=if_else(nchar(MATRICULA)==8,paste("0",MATRICULA,sep = ""),MATRICULA)) %>% unite(MATPROG, MATRICULA, CVE_PROGRAMA, sep = "") %>% unique()
cont <- count(Aux, MATPROG)
Aux <- left_join(Aux, cont, by="MATPROG")
Aux$`tip victoria` <- if_else(Aux$n==3 | Aux$n==2,"Liberado",
                              if_else(Aux$`tip victoria`=="completo/proceso de nvio","Completo/Proceso de envío",
                                      ifelse(Aux$`tip victoria`=="liberado","Liberado",Aux$`tip victoria`)))
names(Aux)[2] <- "Cert y Tit"
Aux <- select(Aux, -n) %>% unique()
Est_Gen <- left_join(Est_Gen,Aux,by="MATPROG") %>% mutate(`Cert y Tit`=if_else(is.na(`Cert y Tit`),"En proceso de certificado",`Cert y Tit`))
remove(cont)#----x
#Entregas
Aux <- readxl::read_excel(paste(c_info,"Van.xlsx",sep = "")) %>% select(MATRICULA,ESTATUS) %>% unique() %>% group_by(MATRICULA) %>% mutate(cont=row_number()) %>% filter(cont==1)%>%
  select(-cont)
names(Aux)[2] <- "Entregas"
Est_Gen <- left_join(Est_Gen, Aux, by="MATRICULA")
Est_Gen$Entregas <- if_else(Est_Gen$Entregas=="Devolución"|is.na(Est_Gen$Entregas), "Pendiente","Enviado")
#####------------------------------------------Limpieza final------------------------------------------#####
Est_Gen <- select(Est_Gen, -COMENTARIO)
Est_Gen <- mutate_all(Est_Gen,~replace(.,is.na(.) | .=="" | .==" ","_")) %>% mutate_all(~stri_trim(.)) %>% mutate_all(~gsub(",","_",.))
#Quitando duplicados, revisar si son correctos después.
Est_Gen <- mutate(Est_Gen, Fecha_e=dmy(FECHA_ESTATUS),Fecha_i=dmy(FECHAINICIO),Fecha_ie=dmy(PRIMER_INSCRIPCION_SIN_ESTATUS))%>%
  arrange(desc(ESTATUS_CODE),desc(Fecha_ie),ESTATUS_SOLICITUD,desc(Fecha_e),desc(Fecha_i)) %>% group_by(MATPROG) %>% mutate(cont=row_number())
Est_Gen <- filter(Est_Gen, cont==1)
#Ordenando, se quitan columnas innecesarias
Aux <- readxl::read_excel(paste(c_info,"Info_general.xlsx",sep = ""),sheet = "Orden")
Aux <- Aux$Orden
Est_Gen <- Est_Gen[Aux]
#Arreglando nombres
Est_Gen$ESTUDIANTE <- gsub("/"," ",Est_Gen$ESTUDIANTE)
#Imprimiendo final.
write.csv(Est_Gen, paste(c_Egre,"Egresados.csv",sep = ""), row.names = F)
#proc.time()-t


















