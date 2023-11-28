sapply(c("MultiLEDS","dplyr","tidyr","lubridate","stringr","stringi"), library,character.only=T)
diremail("C:/Users/jsalinba/Documents/Reportes","jsalinba@utel.edu.mx")
#####-----------Eliminando archivos-----------#####
#carpeta FTP
for (arch in list.files("C:/Users/jsalinba/Documents/Reportes/Bases/FTP","*csv|*xlsx|*xlsxm|*txt",full.names = T)) {
  file.remove(arch)
}
#Carpeta Subir
for (arch in list.files("C:/Users/jsalinba/Documents/Reportes/Bases/FTP/Subir",full.names = T)) {
  file.remove(arch)
}
#####-------------------------------------------------------FTP Lic-------------------------------------------------------#####
#####-----------Retencion-----------#####
Rep <- leer("//Tut-095/reportes/ESTADOS/retencion.csv",col_select=c(matricula,"Ret"=SOLICITUD))
Base <- leer("//Tut-095/reportes/FRONT/niveles_de_riesgo.csv") %>%
  mutate_all(~gsub("\\|,","",.)) %>% unite(MATPROG,matricula,`Clave programa`,remove=F,na.rm=T,sep="") %>%
  left_join(Rep,by="matricula") %>% mutate(edomoddle=if_else(is.na(Ret),edomoddle,"Retencion"),
                                           Grado="Licenciatura",
                                           Modalidad=if_else(grepl("eje",`Tipo de Alumno`,ignore.case = T),"Ejecutiva",
                                                             if_else(grepl("sen",`Tipo de Alumno`,ignore.case = T),"Senior","Online")),
                                           FORMA_ADQUISICION="",Peap_S="",`Correo alternativo`="")
#####-----------Tipo SFTP-----------#####
Rep <- leer("//Tut-095/reportes/ESTADOS/tipi_ftp.xlsx") %>% filter(!duplicated(matricula))
Base <- left_join(Base,Rep,by="matricula") %>%
  mutate(edomoddle=if_else(!is.na(tipo),
                           if_else(tipo%in%c("Preventivo","Contención","Contencion") & Semaforo!="Alto riesgo académico","Activo",tipo),edomoddle))
#####-----------Indicadores Bi Mae-----------#####
Rep <- leer("//Tut-095/reportes/FRONT/indicadoresbi.csv",col_select=c(matricula,inicio_plus,`Fecha Inicio`)) %>% unite(Clave,matricula,inicio_plus,remove=F) %>%
  filter(!duplicated(Clave)) %>% select(-Clave) %>% filter(!duplicated(matricula))
Base <- left_join(Base,Rep,by="matricula")
#####-----------Financiero-----------#####
Rep <- leer("//Tut-095/reportes/ESTADOS/FinancieroGeneral.csv",col_select=c(MATRICULA,SALDO_VENCIDO,MORA,AVANCE_CURRICULAR,PROGRAMA_CODE)) %>%
  mutate(MATRICULA=as.integer(MATRICULA)) %>% unite(MATPROG,MATRICULA,PROGRAMA_CODE,sep="") %>% filter(!duplicated(MATPROG))
Base <- left_join(Base,Rep,by="MATPROG")
#####-----------Alianza y Regla negocio-----------#####
Rep <- leer("//Tut-095/reportes/ESTADOS/alianzamodalida.csv",col_select=c("matricula"=Matr,Alianza)) %>% mutate(matricula=gsub("[A-z]","",matricula)) %>%
  filter(!duplicated(matricula))
Base <- left_join(Base,Rep,by="matricula")
Rep <- leer("//Tut-095/reportes/ESTADOS/reglas_negocios.csv") %>% rename("matricula"=MATRICULA) %>% filter(!duplicated(matricula))
Base <- left_join(Base,Rep,by="matricula")
#####-----------Ordenando-----------#####
diremail("C:/Users/jsalinba/Documents/Reportes")
Rep <- leer(c("Info FTP","Bases$"),sheet="ColsL")
Base <- `colnames<-`(Base,Rep$Nuevo)
Rep <- filter(Rep,!is.na(Seleccionar))
Base <- Base[Rep$Seleccionar] %>% mutate_all(~gsub("\\|,","",.)) %>% mutate_all(~replace_na(.,"")) %>%
  mutate(Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula))
#####-----------Matrículas piloto-----------#####
Rep <- filter(Base,`Estado en Moodle` == "Piloto")
Base <- mutate(Base,Alianzas = if_else(`Estado en Moodle`=="Piloto","UTEL",Alianzas))#,
               # Bl=if_else(!grepl("D2",`Bloque seguimiento`),as.integer(substring(`Bloque seguimiento`,10,11))+1,1),
               # Bl=if_else(Bl>25,25,Bl),
               # `Bloque seguimiento`=if_else(Bl<25,gsub(" y más","",`Bloque seguimiento`),`Bloque seguimiento`),
               # Bl=if_else(nchar(Bl)==1,paste0("0",Bl),as.character(Bl)),
               # Bl=paste0(substring(`Bloque seguimiento`,1,9),Bl,substring(`Bloque seguimiento`,12)),
               # Bl=if_else(grepl("02",Bl),gsub("_[ABCD]","",Bl),Bl),
               # Bl=replace(Bl,Bl=="NANANA",""),
               # `Bloque seguimiento`=Bl) %>% select(-Bl)
Base <- rbind(Base,Rep)
#####-----------Automation y Nivelación costo cero-----------#####
#Automation
Rep <- leer(c("Automation","auxiliares$")) %>% select(Matricula) %>% mutate(SP = "Materia Foco") %>%
  mutate(Matricula = if_else(nchar(Matricula)<9,paste0("0",Matricula),Matricula),Matricula = stri_trim(Matricula)) %>% filter(!duplicated(Matricula))
Base <- left_join(Base,Rep,"Matricula") %>% mutate(`Servicios PEAP`=if_else(!is.na(SP),SP,`Servicios PEAP`)) %>% select(-SP)
#Nivelación
Rep <- leer(c("Nivelación Costo Cero","auxiliares$")) %>% select("Matricula"=matricula) %>% mutate(SP = "Nivelación costo cero") %>%
  mutate(Matricula = if_else(nchar(Matricula)<9,paste0("0",Matricula),Matricula),Matricula = stri_trim(Matricula)) %>% filter(!duplicated(Matricula))
Base <- left_join(Base,Rep,"Matricula") %>% mutate(`Servicios PEAP`=if_else(!is.na(SP),SP,`Servicios PEAP`)) %>% select(-SP)
#####-----------Imprimiendo por alianza-----------#####
Rep2 <- leer(c("Info FTP","Bases$"),sheet="AlianzasL")
diremail("C:/Users/jsalinba/Documents/Reportes")
#diremail("//TUT-095/Licenciaturas/Inconcert")
for (i in 1:length(Rep2[[1]])) {
  Rep <- filter(Base,Alianzas==Rep2[[i,"Alianza"]])
  escribir(Rep,c(paste(Rep2[[i,"Nombre"]],Sys.Date(),".csv",sep=""),"FTP$"),quote=F)
}
#####-------------------------------------------------------FTP Mae-------------------------------------------------------#####
#####-----------Retencion-----------#####
try(drive_sd("des",c(paste("Niveles/NR_POSGRADOS_",Sys.Date(),".csv",sep=""),"NR/Sep-Oct 23$")))
#drive_sd("des",c(paste0("NR_UTEL_GLOBAL/NR_",Sys.Date(),".csv"),"NR Global$"))
if (!file.exists(paste0("C:/Users/jsalinba/Documents/Reportes/Bases/Originales/NR/Sep-Oct 23/NR_POSGRADOS_",Sys.Date(),".csv"))) {
  file.copy(paste0("C:/Users/jsalinba/Documents/Reportes/Bases/Originales/NR/Sep-Oct 23/NR_POSGRADOS_",Sys.Date()-1,".csv"),
            paste0("C:/Users/jsalinba/Documents/Reportes/Bases/Originales/NR/Sep-Oct 23/NR_POSGRADOS_",Sys.Date(),".csv"))
}
Rep <- leer("//Tut-095/reportes/ESTADOS/retencion.csv",col_select=c("Matricula"=matricula,"Ret"=SOLICITUD))
Base <- leer(c(paste0("NR_POSGRADOS_",Sys.Date()),"Sep-Oct 23$")) %>%
  mutate_all(~gsub("\\|,","",.)) %>% unite(MATPROG,Matricula,Clave.Programa,remove=F,na.rm=T,sep="") %>%
  left_join(Rep,by="Matricula") %>% mutate(edo.moodle=if_else(is.na(Ret),edo.moodle,"Retencion"),
                                           Grado=Modalidad,Grado=if_else(Grado=="Máster","Master",Grado),
                                           Modalidad=if_else(grepl("eje",Ingreso,ignore.case = T),"Ejecutiva",
                                                             if_else(grepl("sen",Ingreso,ignore.case = T),"Senior","Online")),
                                           FORMA_ADQUISICION="",Peap_S="",`Correo alternativo`="")
#####-----------Tipo SFTP-----------#####
Rep <- leer("//Tut-095/reportes/ESTADOS/tipi_ftp.xlsx") %>% rename("Matricula"=matricula) %>% filter(!duplicated(Matricula))
Base <- left_join(Base,Rep,by="Matricula") %>%
  mutate(edo.moodle=if_else(!is.na(tipo),
                           if_else(tipo%in%c("Preventivo","Contención","Contencion") & Semaforo!="Alto riesgo académico","Activo",tipo),edo.moodle))
#####-----------Indicadores Bi Mae-----------#####
Rep <- leer("//Tut-095/reportes/FRONT/indicadoresbiMAE.csv",col_select=c("Matricula"=matricula,Fecha.Inicio)) %>%
  mutate(inicio_plus=paste(substring(Fecha.Inicio,9,10),substring(Fecha.Inicio,6,7),sep="")) %>% unite(Clave,Matricula,inicio_plus,remove=F) %>%
  filter(!duplicated(Clave)) %>% select(-Clave)
Base <- left_join(Base,Rep,by="Matricula")
#####-----------Financiero-----------#####
Rep <- leer("//Tut-095/reportes/ESTADOS/FinancieroGeneral.csv",col_select=c(MATRICULA,SALDO_VENCIDO,MORA,AVANCE_CURRICULAR,PROGRAMA_CODE)) %>%
  mutate(MATRICULA=as.integer(MATRICULA)) %>% unite(MATPROG,MATRICULA,PROGRAMA_CODE,sep="") %>% filter(!duplicated(MATPROG))
Base <- left_join(Base,Rep,by="MATPROG")
#####-----------Alianza y Regla negocio-----------#####
Rep <- leer("//Tut-095/reportes/ESTADOS/alianzamodalida.csv",col_select=c("MATPROG"=Matr,Alianza)) %>% filter(!duplicated(MATPROG))
Base <- left_join(Base,Rep,by="MATPROG")
Rep <- leer("//Tut-095/reportes/ESTADOS/reglas_negocios.csv") %>% rename("Matricula"=MATRICULA) %>% filter(!duplicated(Matricula))
Base <- left_join(Base,Rep,by="Matricula")
#####-----------Ordenando-----------#####
diremail("C:/Users/jsalinba/Documents/Reportes")
Rep <- leer(c("Info FTP","Bases$"),sheet="ColsM")
Base <- `colnames<-`(Base,Rep$Nuevo)
Rep <- filter(Rep,!is.na(Seleccionar))
Base <- Base[Rep$Seleccionar] %>% mutate_all(~gsub("\\|,|NA","",.)) %>% mutate_all(~replace_na(.,"")) %>%
  mutate(Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula))
#####-----------Matrículas piloto-----------#####
Base <- mutate(Base,Alianzas = if_else(`Estado en Moodle`=="Piloto","UTEL",Alianzas))#,
               # Bl=if_else(nchar(`Bloque seguimiento`)<15,substring(`Bloque seguimiento`,10,11),substring(`Bloque seguimiento`,14,15)),
               # Bl=if_else(nchar(`Bloque seguimiento`)<15,
               #            if_else(Bl=="09",paste0("0",as.integer(Bl)),paste0("0",as.integer(Bl)+1)),
               #            if_else(Bl=="06",paste0("0",as.integer(Bl)),paste0("0",as.integer(Bl)+1))),
               # Bl=if_else(nchar(`Bloque seguimiento`)<15,paste0("Bimestre ",Bl),paste0("Cuatrimestre ",Bl)),
               # Bl=if_else(grepl("A2",`Bloque seguimiento`),`Bloque seguimiento`,Bl),
               # Bl=replace(Bl,Bl=="Bimestre 0NA",""),
               # `Bloque seguimiento`=Bl) %>% select(-Bl)
#####-----------Automation y Nivelación costo cero-----------#####
#Automation
Rep <- leer(c("Automation","auxiliares$")) %>% select(Matricula) %>% mutate(SP = "Materia Foco") %>%
  mutate(Matricula = if_else(nchar(Matricula)<9,paste0("0",Matricula),Matricula),Matricula = stri_trim(Matricula)) %>% filter(!duplicated(Matricula))
Base <- left_join(Base,Rep,"Matricula") %>% mutate(`Servicios PEAP`=if_else(!is.na(SP),SP,`Servicios PEAP`)) %>% select(-SP)
#Nivelación
Rep <- leer(c("Nivelación Costo Cero","auxiliares$")) %>% select("Matricula"=matricula) %>% mutate(SP = "Nivelación costo cero") %>%
  mutate(Matricula = if_else(nchar(Matricula)<9,paste0("0",Matricula),Matricula),Matricula = stri_trim(Matricula)) %>% filter(!duplicated(Matricula))
Base <- left_join(Base,Rep,"Matricula") %>% mutate(`Servicios PEAP`=if_else(!is.na(SP),SP,`Servicios PEAP`)) %>% select(-SP)
#####-----------Imprimiendo por alianza-----------#####
Rep2 <- leer(c("Info FTP","Bases$"),sheet="AlianzasM")
diremail("C:/Users/jsalinba/Documents/Reportes")
for (i in 1:length(Rep2[[1]])) {
  Rep <- filter(Base,Alianzas==Rep2[[i,"Alianza"]])
  escribir(Rep,c(paste(Rep2[[i,"Nombre"]],Sys.Date(),".csv",sep=""),"FTP$"),quote=F)
}
#####-------------------------------------------------------FTP Global-------------------------------------------------------#####
#####-----------Retencion-----------#####
try(drive_sd("des",c(paste0("NR_UTEL_GLOBAL/NR_",Sys.Date(),".csv"),"NR Global$")))
#drive_sd("des",c(paste0("NR_UTEL_GLOBAL/NR_",Sys.Date(),".csv"),"NR Global$"))
if (!file.exists(paste0("C:/Users/jsalinba/Documents/Reportes/Bases/Originales/NR Global/NR_",Sys.Date(),".csv"))) {
  file.copy(paste0("C:/Users/jsalinba/Documents/Reportes/Bases/Originales/NR Global/NR_",Sys.Date()-1,".csv"),
            paste0("C:/Users/jsalinba/Documents/Reportes/Bases/Originales/NR Global/NR_",Sys.Date(),".csv"))
}
Rep <- leer("//Tut-095/reportes/ESTADOS/retencion.csv",col_select=c("Matricula"=matricula,"Ret"=SOLICITUD))
Base <- leer(c(paste0("NR_",Sys.Date()),"NR Global$")) %>% mutate_all(~gsub("\\|,","",.)) %>% unite(MATPROG,matricula,`Clave programa`,remove=F,na.rm=T,sep="") %>%
  rename("Matricula"=matricula) %>% left_join(Rep,by="Matricula") %>%
  mutate(edomoddle=if_else(is.na(Ret),edomoddle,"Retencion"),
         Grado=Nivel,Grado=if_else(grepl("Bache",Grado),"Licenciatura","Maestría"),
         Modalidad="Online",
         `Fecha inicio bimestre`="2023-10-23",
         Semaforo = if_else(grepl("Without",Semaforo),"Sin riesgo académico",
                            if_else(grepl("High",Semaforo),"Alto riesgo académico","Riesgo académico")),
         NR = gsub("To Pass","Por aprobar",NR),
         NR = gsub("Pass","Aprobado",NR),
         NR = gsub("Never","Nuncas",NR),
         NR = gsub("Fail","Reprobó",NR),
         NR = replace(NR,NR=="Without risk","Sin riesgo"),
         documentos = "",Tutor="No carterizado",Supervisor="No carterizado",Facultad="",`Servicios PEAP`="")
#####-----------Financiero-----------#####
Rep <- leer("//Tut-095/reportes/ESTADOS/FinancieroGeneral.csv",col_select=c(MATRICULA,SALDO_VENCIDO,MORA,AVANCE_CURRICULAR,PROGRAMA_CODE)) %>%
  mutate(MATRICULA=as.integer(MATRICULA)) %>% unite(MATPROG,MATRICULA,PROGRAMA_CODE,sep="") %>% filter(!duplicated(MATPROG))
Base <- left_join(Base,Rep,by="MATPROG")
#####-----------Alianza y Regla negocio-----------#####
Base <- mutate(Base,Alianzas = paste0("UTEL ",sapply(strsplit(`Tipo de Alumno`," "), function(x) x[2])),
               `Línea de negocio`="Global")
#####-----------Ordenando-----------#####
Rep <- leer(c("Info FTP","Bases$"),sheet="ColsG")
Base <- `colnames<-`(Base,Rep$Nuevo)
Rep <- filter(Rep,!is.na(Seleccionar))
Base <- Base[Rep$Seleccionar] %>% mutate_all(~gsub("\\|,|NA","",.)) %>% mutate_all(~replace_na(.,"")) %>%
  mutate(Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula))
#####-----------Matrículas piloto-----------#####
Base <- mutate(Base,Alianzas = if_else(`Estado en Moodle`=="Piloto","UTEL",Alianzas))
# Base <- mutate(Base,`Fecha inicio bimestre`=if_else(`Fecha inicio bimestre`!="2023-06-19","2023-07-03",`Fecha inicio bimestre`),
#                Bl=as.integer(substring(`Bloque seguimiento`,10,11))+1,
#                Bl=replace(Bl,Bl==10,9),
#                Bl=paste0("Bimestre 0",Bl),
#                `Bloque seguimiento`=Bl) %>% select(-Bl)

# Rep2 <- leer(c("Info FTP","Bases$"),sheet="AlianzasG")
# for (i in 1:length(Rep2[[1]])) {
#   Rep <- filter(Base,Alianzas==Rep2[[i,"Alianza"]])
#   escribir(Rep,c(paste(Rep2[[i,"Nombre"]],Sys.Date(),".csv",sep=""),"FTP$"),quote=F)
#   escribir(Rep,c(paste(Rep2[[i,"Nombre"]],Sys.Date(),".csv",sep=""),"Subir$"),quote=F)
# }
#####-----------Automation y Nivelación costo cero-----------#####
#Automation
Rep <- leer(c("Automation","auxiliares$")) %>% select(Matricula) %>% mutate(SP = "Materia Foco") %>%
  mutate(Matricula = if_else(nchar(Matricula)<9,paste0("0",Matricula),Matricula),Matricula = stri_trim(Matricula)) %>% filter(!duplicated(Matricula))
Base <- left_join(Base,Rep,"Matricula") %>% mutate(`Servicios PEAP`=if_else(!is.na(SP),SP,`Servicios PEAP`)) %>% select(-SP)
#Nivelación
Rep <- leer(c("Nivelación Costo Cero","auxiliares$")) %>% select("Matricula"=matricula) %>% mutate(SP = "Nivelación costo cero") %>%
  mutate(Matricula = if_else(nchar(Matricula)<9,paste0("0",Matricula),Matricula),Matricula = stri_trim(Matricula)) %>% filter(!duplicated(Matricula))
Base <- left_join(Base,Rep,"Matricula") %>% mutate(`Servicios PEAP`=if_else(!is.na(SP),SP,`Servicios PEAP`)) %>% select(-SP)
#####-----------Imprimiendo global-----------#####
escribir(Base,c(paste0("Glo_",Sys.Date(),".csv"),"FTP$"),quote=F)
escribir(Base,c(paste0("Glo_",Sys.Date(),".csv"),"Subir$"),quote=F)
#####-----------Unificando-----------#####
Rep2 <- leer(c("Info FTP","Bases$"),sheet="Subir")
for (i in 1:length(Rep2[[1]])) {
  nom <- Rep2[[i,"Nombre2"]]
  BG <- unificar(c("FTP$",nom),na=c(" "))
  #No. registros
  if(i==1){
    Tams <- data.frame("Nombre"=c(paste(Rep2[[i,"Nombre2"]],Sys.Date(),".csv",sep="")),"Registros"=c(length(BG[[1]])))
  } else{
    Tams[nrow(Tams)+1,] = c(paste(Rep2[[i,"Nombre2"]],Sys.Date(),".csv",sep=""),length(BG[[1]]))
  }
  # if(Rep2[[i,"Nombre2"]]=="Bol_"){
  #   BG <- mutate_at(BG,c("Nombre","Apellidos","Correo","Correo alternativo","Telefono","Estatus Escolares","Adeudo","Mora","Grado","Línea de negocio",
  #                     "Avance curricular","Fecha inicio bimestre"),~(.=""))
  # }
  BG <- mutate(BG,Modalidad=if_else(Modalidad=="Ejecutiva","Ejecutivo",Modalidad))
  #Escribiendo
  escribir(BG,c(paste(Rep2[[i,"Nombre2"]],Sys.Date(),".csv",sep=""),"Subir$"),quote=F)
}
#####--------Unificado general--------#####
Base <- unificar("Subir$",na=c(" "))
escribir(Base,c("General FTP.csv","Principales$"))
#####--------Subiendo--------#####
for (nom in list_archs("Subir$")) {
  googledrive::drive_put(paste0("C:/Users/jsalinba/Documents/Reportes/Bases/FTP/Subir/",nom),"levels_of_risk/")
}
googledrive::drive_put("C:/Users/jsalinba/Documents/Reportes/Bases/Principales/General FTP.csv","levels_of_risk/")
Tams <- arrange(Tams,Nombre)
escribir(Tams,c("Num Registros FTP.csv","Reportes$"))





























