library(MultiLEDS); library(dplyr); library(lubridate); library(tidyr)
#Variables iniciales
F1 <- c("NI 20 NOV"="2023-11-20"); F2 <- c("NI 08 ENE"="2024-01-08"); c1 <- "//Tut-095/reportes"; c2 <- "C:/Users/jsalinba/Documents/Reportes"
diremail(c2,"jsalinba@utel.edu.mx")
# drive_sd("des",c("Contactacion/Conteos_Interacciones.csv","CRM$")); drive_sd("des",c("Contactacion/IDCRM.csv","CRM$")); drive_sd("des",c("Reports/acc_status.csv","Otros$"))
cols <- c("Tutor Anterior","Tutor","Supervisor","Turno","NR Autogestionable","Fecha Inscripción","Mora","Docs","Avance","Promedio","Aprobadas","Reprobadas","En Curso","Whatsapp",
          "Contacto Telefono","Colaboradores","CICLO JULIO","TUTOR NIVEL","OBSERVACIONES","TELEFONOS","Clasificación anterior","Programas")
#####----------------NIRI---------------#####
Base <- leer(c("Inscritos NIRI","SIR$"),col_select=c("Matricula"=MATRICULA,"Nombre"=ESTUDIANTE,CAMPUS,"Canal"=CANAL_FINAL,NIVEL,"Programa"=PROGRAMA,ESTATUS,DECISION,
                                                     "F.Decisón"=FECHA_DECISION,"Inicio_Curso"=FECHAINICIO,"Fecha_ingreso"=FECHA_ACTIVIDAD,CORREO))%>%
  mutate_at(grep("^f|^f\\.|inicio",names(.),ignore.case = T),~dmy(.)) %>% arrange(desc(F.Decisón)) %>% mutate(`Clave Programa`=substring(Programa,1,10),Programa=substring(Programa,12))%>%
  filter(DECISION=="35",ESTATUS %in% c("MATRICULADO","ADMITIDO"),!(CAMPUS %in% c("INA","IND","FIL","VIE","CAP")),(Inicio_Curso==F2 | Inicio_Curso==F1),
         !grepl("prueba|^test|aaron gallo",Nombre,ignore.case = T),NIVEL %in% c("DO","MS","MA")) %>% filter(!duplicated(Matricula))%>%
  mutate(Tipo=if_else(grepl("eje",Programa,ignore.case = T),"Ejecutiva","Online"),
         Estado=if_else(Inicio_Curso==F1,"Activo","FUTUROS"),
         `NI-Q`=if_else(Inicio_Curso==F1,names(F1),names(F2)),
         Modalidad=if_else(NIVEL=="MS","Máster",if_else(NIVEL=="MA","Maestría","Doctorado")),
         Duracion=if_else(NIVEL=="DO","Cuatrimestral","Bimestral"),
         Etiqueta=if_else(NIVEL=="DO",120,60),
         Area="POSGRADOS",
         `F. ASIGNACIÓN`=Sys.Date()) %>% rename("Email"=CORREO) %>%#separate(CORREO,c("Email"),sep="@") %>% 
  unite(MATPROG,Matricula,`Clave Programa`,remove = F,na.rm = T,sep="")
#####----------------Paquetes Ventas, Campus----------------#####
diremail(c1)
Rep <- unificar(c("Posgrados$","Etiquetas")) %>% rename("Matricula"=MATRICULA)
x <- filter(Rep,Matricula=="1038284"); Rep <- filter(Rep,Matricula!="1038284") #Se eliminan NA también, se filtra única matrícula diferente para unirla después
for (i in 1:8) {
  Rep <- mutate(Rep,Matricula=if_else(nchar(Matricula)<9,
                                      if_else(nchar(Matricula)==7,paste("1",Matricula,sep=""),paste("0",Matricula,sep="")),Matricula))
}; Rep <- rbind(Rep,x); remove(x)
diremail(c2); escribir(Rep,c("Histórico Ventas Paquetes Etiquetas.csv","auxiliares$")); diremail(c1)
Rep <- mutate(Rep,Sen=if_else(grepl("seni",PAQUETE_INSCRITO,ignore.case = T),"Senior","_")) %>% filter(Sen=="Senior") %>% select(Matricula,Sen) %>% filter(!duplicated(Matricula))
Base <- left_join(Base,Rep,by="Matricula") %>% mutate(Tipo=if_else(!is.na(Sen),Sen,Tipo)) %>% select(-Sen)
#Filtrando solo Ejecutivo y Senior, agregando sesiones ejecutivas
diremail(c2)
Base <- left_join(Base,mutate(leer(c("Sesiones Ejecutivas","auxiliares$"),col_select=c("Matricula"=MATRICULA)),tip="Sesión Ejecutiva")%>%
                    filter(!duplicated(Matricula)),by="Matricula") %>% mutate(Tipo=if_else(is.na(tip),Tipo,tip)) %>% filter(Tipo!="Online")%>%
  left_join(leer(c("Campus","auxiliares$")),by="CAMPUS") %>% select(-tip)
#####----------------Base anterior,Nuevos----------------#####
diremail(c2)
Rep <- leer(c("Asignación Posgrados","Bases$"),sheet="Base") %>% filter(Base=="Ejecutiva") %>% select(-Base) %>%
  mutate(Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula),Nuevo="No")%>%
  mutate_at(grep("Acc|^Fecha|Inicio_Curso|F\\.",names(.)),~if_else(!grepl("[A-Z\\-]",.),as.character(as.Date(as.numeric(.),"1899-12-30")),.))%>%
  unite(MATPROG,Matricula,`Clave Programa`,remove = F,na.rm = T,sep="")
Base <- left_join(Base,Rep[c("MATPROG",cols,"Nuevo")],by="MATPROG") %>% mutate(Nuevo=if_else(is.na(Nuevo),"Si","No")) %>% filter(Nuevo=="Si",!grepl("OPM",Campus))
Rep <- select(Rep,names(Rep)[names(Rep) %in% names(Base)]); Base <- select(Base,names(Rep)) %>% mutate_all(~as.character(.)) #Columnas Principales
Base <- rbind(Rep,Base) #Se imprime en esta parte
Base <- mutate_at(Base,grep("^f|^f\\.|inicio",names(Base),ignore.case = T),~ymd(.))
#####----------------Estatus General----------------#####
diremail(c2); x <- c("MATPROG","Estado Argos","Fecha cambio de Edo","Entidad")
Rep <- leer(c("Estatus General Alumnos","SIR$"),col_select=c(MATRICULA,PROGRAMA,"Estado Argos"=ESTATUS,"Fecha cambio de Edo"=FECHA_ESTATUS,PRIMER_INSCRIPCION,FECHAINICIO,
                                                             "Entidad"=PAIS)) %>% separate(PRIMER_INSCRIPCION,c("PI"),sep=" ") %>%
  mutate(`Clave Programa`=substring(PROGRAMA,1,10)) %>% unite(MATPROG,MATRICULA,`Clave Programa`,sep = "") %>% mutate_at(grep("fecha|pi",names(.),ignore.case = T),~dmy(.))%>%
  arrange(desc(`Fecha cambio de Edo`),desc(FECHAINICIO)) %>% filter(!duplicated(MATPROG)) %>% select(identity(x))
Base <- left_join(Base,Rep,by="MATPROG")
#####----------------Estados----------------#####
diremail(c1)
Rep <- leer(c("ESTADOS","Posgrados$"))
Base <- left_join(Base,Rep,by="Estado Argos") %>% mutate(Estado=if_else(Estado=="CC","CC",
                                                                        if_else(Estado=="FUTUROS","FUTUROS",Edo))) %>% select(-Edo)
#####----------------Cambio de ciclo----------------#####
diremail(c2)
Rep <- leer(c("Inscritos NIRI","SIR$"),col_select=c(MATRICULA,PROGRAMA,FECHA_DECISION,FECHAINICIO)) %>% mutate_at(c("FECHA_DECISION","FECHAINICIO"),~dmy(.))%>%
  arrange(desc(FECHA_DECISION),desc(FECHAINICIO)) %>% mutate(PROGRAMA=substring(PROGRAMA,1,10)) %>% unite(MATPROG,MATRICULA,PROGRAMA,sep="") %>% filter(!duplicated(MATPROG))%>%
  select(-FECHA_DECISION)
Base <- left_join(Base,Rep,by="MATPROG")
Base <- mutate(Base,FECHAINICIO=if_else(is.na(FECHAINICIO),Inicio_Curso,FECHAINICIO),
               CC=if_else(Estado %in% c("FUTUROS","Activo","Pendiente") & `NI-Q` %in% c("NI 23 OCT","NI 20 NOV","NI 08 ENE","Pendiente"),
                          if_else(FECHAINICIO > Inicio_Curso,"CC","_"),"_"),
               Estado=if_else(CC=="CC","CC",Estado),
               Inicio_Curso=if_else(CC=="CC",FECHAINICIO,Inicio_Curso)) %>% select(-CC,-FECHAINICIO)
#####----------------Retención----------------#####
diremail(c1)
Rep <- leer(c("retencion","ESTADOS$"),col_select=c("Matricula"=matricula,"Retención"=SOLICITUD))%>%
  mutate(Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula))
Base <- left_join(Base,Rep,by="Matricula")
#####----------------Sabana----------------#####
Rep <- leer(c("sabanamae","FRONT$"),col_select=c(Matricula,Semana,"Acc_Materia"=Acceso.Materia,"Acc_Auvi_"=Acceso.Aula,Status.Materia)) %>% filter(Status.Materia=="Activo")%>%
  mutate(Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula)) %>% mutate_at(c("Acc_Materia","Acc_Auvi_"),~ymd_hm(.)) %>% arrange(desc(Acc_Materia))%>%
  filter(!duplicated(Matricula)) %>% mutate_at(c("Acc_Materia","Acc_Auvi_"),~if_else(is.na(.),"Nunca",as.character(.))) %>% mutate(Semana=paste("Semana",Semana,sep=""))%>%
  select(-Status.Materia)
Base <- left_join(Base,Rep,by="Matricula") %>% mutate_at(c("Acc_Materia","Acc_Auvi_"),~replace_na(.,"Sin Asignaturas"))%>%
  mutate(Acc_Mat=if_else(Acc_Materia=="Sin Asignaturas","Sin Asignaturas",if_else(Acc_Materia=="Nunca","Nunca","Activo")))
#####----------------Nivel de Riesgo y Clasificación por materia----------------#####
Rep <- leer(c("niveles_de_riesgoMAE","FRONT$"),col_select=c(Matricula,Aprobado,Np,Nuncas,Por.aprobar,Reprobó,"Nivel de Riesgo"=Semaforo,edo.moodle)) %>%
  filter(edo.moodle=="Activo") %>% select(-edo.moodle) %>% mutate(x="")
for (i in 2:6) {
  Rep <- mutate(Rep,x=if_else(Rep[i]!="0",paste(x,Rep[[i]],names(Rep[i]),sep=" "),x))
}
Rep <- mutate(Rep,x=if_else(x=="","Sin Asignaturas",x),Clasificación=if_else(Nuncas >= 1,"Nuncas",if_else(Np >=1,"Np",if_else(Reprobó >=1,"Reprobó","Aprobado"))),
              x=stringi::stri_trim(x),Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula)) %>% rename("Clasificación por Materia"=x) %>% select(-(2:6))
Base <- left_join(Base,Rep,by="Matricula") %>% mutate_at(c("Clasificación por Materia","Clasificación","Nivel de Riesgo"),~replace_na(.,"Sin Asignaturas"))
#Sesiones
Rep <- leer(c("sesiones","FRONT"),col_select=c("Matricula"=matricula,status)) %>% filter(status=="Activo") %>% count(Matricula)%>%
  mutate(Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula),TipoS=if_else(n > 1,"Talleres","Taller")) %>% unite(TipoS,n,TipoS,sep=" ")
Base <- left_join(Base,Rep,by="Matricula") %>% unite(`Clasificación por Materia`,`Clasificación por Materia`,TipoS,sep=" ",na.rm=T) %>% mutate(Mat=`Clasificación por Materia`)
#####----------------Interacciones----------------#####
diremail(c2)
Rep <- leer(c("Conteos_Interacciones","CRM$"),col_select=c("Matricula"=matricula,"# Contacto Efectivo"=`Contacto efectivo`,"# Intentos Contacto"=Intento,
                                                           "Fecha Ultimo Contacto"=Fecha_ultimo_contacto,"Tutor que contacto"=Tutor.y,"Medio de Contacto"=Medio))%>%
  filter(!duplicated(Matricula)) %>% arrange(`# Contacto Efectivo`) %>% mutate(Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula))
Base <- left_join(Base,Rep,by="Matricula") %>% mutate_at(c("# Contacto Efectivo","# Intentos Contacto"),~replace_na(.,"0"))%>%
  mutate_at(c("# Contacto Efectivo","# Intentos Contacto"),~as.integer(.)) %>% mutate_at(c("Fecha Ultimo Contacto","Tutor que contacto","Medio de Contacto"),~replace_na(.,"_"))%>%
  mutate(Contactación=if_else(`# Contacto Efectivo` > 0,"Contactado",if_else(`# Intentos Contacto` > 0,"No Contactado","Sin Intento")),Rango_Cont_Efec="_",Rango_Int_Cont="_")
#Rango interacciones
Rep <- leer(c("Info Interna","Reportes$"),secc="Interacciones") %>% mutate(Interacciones=as.integer(Interacciones))
for (i in length(rownames(Rep)):1) {
  Base <- mutate(Base,Rango_Cont_Efec=if_else(Rango_Cont_Efec=="_",
                                              if_else(`# Contacto Efectivo`>=Rep[[i,1]],Rep[[i,2]],Rango_Cont_Efec),Rango_Cont_Efec),
                 Rango_Int_Cont=if_else(Rango_Int_Cont=="_",
                                        if_else(`# Intentos Contacto`>=Rep[[i,1]],Rep[[i,2]],Rango_Int_Cont),Rango_Int_Cont))
}
#####----------------Adeudo (Financiero)----------------#####
Rep <- leer(c("Académico Financiero","SIR$"),col_select=c("Matricula"=MATRICULA,"Adeudo"=SALDO_TOTAL)) %>% filter(!duplicated(Matricula))
Base <- left_join(Base,Rep,by="Matricula") %>% mutate(Adeudo=replace_na(Adeudo,"0"),Adeudo=as.numeric(Adeudo),Rango_Adeudo="_")
#Rango adeudo
Rep <- leer(c("Info Interna","Reportes$"),secc="Adeudo") %>% mutate(Adeudo=as.integer(Adeudo))
for (i in length(rownames(Rep)):1) {
  Base <- mutate(Base,Rango_Adeudo=if_else(Rango_Adeudo=="_",
                                           if_else(Adeudo>=Rep[[i,1]],Rep[[i,2]],Rango_Adeudo),Rango_Adeudo))
}
#####----------------LINKS----------------#####
#Aula
Rep <- leer(c("acc_status","Otros$"),col_select=c(Matricula,id,aula)) %>% filter(aula %in% c("Aula12","Aula13")) %>% arrange(as.integer(substring(aula,5,6)))%>%
  filter(!duplicated(Matricula)) %>% mutate(`LINK AULA`=if_else(aula=="Aula12","https://aula12.utel.edu.mx/user/profile.php?id=","https://aula13.utel.edu.mx/user/profile.php?id="),
                                            Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula)) %>% unite(`LINK AULA`,`LINK AULA`,id,sep = "") %>% select(-aula)
Base <- left_join(Base,Rep,by="Matricula")
#CRM
Rep <- leer(c("IDCRM","CRM$")) %>% mutate(fecha_creacion=ymd_hms(fecha_creacion),`LINK CRM`="http://crm.utel.edu.mx/index.php?module=Contacts&action=DetailView&record=")%>%
  unite(`LINK CRM`,`LINK CRM`,ID_CRM,sep="") %>% arrange(desc(fecha_creacion)) %>% select(Matricula,`LINK CRM`)
for (i in 1:8) {
  Rep <- mutate(Rep,Matricula=if_else(nchar(Matricula)<9,
                                      if_else(nchar(Matricula)==7,paste("1",Matricula,sep=""),paste("0",Matricula,sep="")),Matricula))
}
Rep <- filter(Rep,!duplicated(Matricula)); Base <- left_join(Base,Rep,by="Matricula")
# #####----------------Segmentos----------------#####
# Rep <- leer(c("Info Interna","Reportes$"),secc="Segmentos")
# Base <- left_join(Base,Rep,by="Programa") %>% mutate(Programas=if_else(Canal %in% c("OPM","OPM UTEL ESPANA"),"CEUPE",if_else(Canal=="IZZI","IZZI",Programas)),
#                                                      Reg=if_else(Campus=="UTEL","UTEL","LATAM"),Mod=if_else(Modalidad=="Doctorado","DOCTORADO",as.character(NA)))%>%
#   unite(Programas,Programas,Mod,na.rm = T,sep = " ") %>% unite(Programas,Programas,Reg,na.rm = T,sep = " ") %>% mutate(Programas=stringi::stri_trim(Programas))
# #####----------------Avanzados----------------#####
# Rep <- left_join(rbind(filter(Base,Estado=="FUTUROS"),filter(Base,`NI-Q`=="NI 28 MAR")) %>% select(MATPROG,Matricula),
#                  leer(c("Académico Financiero","SIR$"),col_select=c("Matricula"=MATRICULA,MAIL_PRIN)) %>% filter(!duplicated(Matricula)),by="Matricula")
# diremail(c1)
# Rep <- left_join(Rep,unificar(c("Posgrados$","CCAMP2020"),c("Correo del estudiante"="MAIL_PRIN"),c("MAIL_PRIN","Identificador"),skip = 1) %>% filter(!duplicated(MAIL_PRIN)),
#                  by="MAIL_PRIN") %>% left_join(leer(c("historicodematerias","ESTADOS$")) %>% mutate(MATR=if_else(nchar(MATR)<19,paste("0",MATR,sep=""),MATR)) %>% rename("MATPROG"=MATR),
#                                                by="MATPROG")%>%
#   mutate(Materias_Cursadas=as.numeric(replace_na(Materias_Cursadas,"0")),
#          tip=if_else(!is.na(Identificador),"Master_Maestria",
#                      if_else(Materias_Cursadas >= 2,"Avanzado","Onboarding"))) %>%  select(Matricula,tip)
# Base <- left_join(Base,Rep,by="Matricula") %>% mutate(Tipo=if_else(!is.na(tip),tip,Tipo)) %>% select(-tip)
Base <- filter(Base,!duplicated(MATPROG))
Base <- filter(Base,!is.na(`Estado Argos`))
#####----------------Re ordenando e imprimiendo----------------#####
diremail(c2)
Rep <- leer(c("Info Interna","Reportes$"),sheet="Orden") %>% .[["Orden"]]
Base <- Base[Rep] %>% mutate_all(~as.character(.)) %>% mutate_all(~replace_na(.,"_")) %>% mutate(Base="Ejecutiva")
#Filtrando nuevos Matriculados
# Rep <- subset(Base,(Nuevo=="Si" & `Estado Argos` %in% c("ADMITIDO","MATRICULADO","PREMATRICULADO")))
# Base <- subset(Base,Nuevo=="No")
# Base <- rbind(Base,Rep)
data.table::fwrite(Base,"C:/Users/jsalinba/Documents/Reportes/Bases/Principales/Asignación Posgrados Eje-Sen.csv",bom = T,na = "_")
#escribir(Base,c("Asignación Posgrados Eje-Sen.csv","Principales$"))


























