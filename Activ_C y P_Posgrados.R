sapply(c("MultiLEDS","dplyr","tidyr","lubridate","RMySQL"),library,character.only=T)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#####--------Descarga de archivos--------#####
drive_sd("des",c("Reports/Activities_Mae.csv","Otros$"))
drive_sd("des",c("Reports/reporte_completo_Mae.csv","Otros$"))
drive_sd("des",c("Reports/Acc_Prof_Mae.csv","Otros$"))
drive_sd("des",c("Reports/links_actividades_posgra.csv","Otros$"))
#####--------Reporte completo--------#####
Rep <- leer(c("reporte_completo_Mae","Otros"),col_select=c(matricula,Nombre,Apellidos,clave,"GRC"=grupo)) %>% codificacion() %>% unite(Nombre.Alumno,Nombre,Apellidos,sep=" ") %>%
  unite(ID,matricula,clave) %>% filter(!duplicated(ID))
Base <- leer(c("Activities_Mae","Otros$")) %>% codificacion() %>% unite(ID,matricula,clave,remove=F) %>% left_join(Rep,"ID") %>% filter(!is.na(grupo)) %>%
  unite(clave_g,clave,grupo,remove=F) %>% select(clave_g,matricula,Nombre.Alumno,clave,"grupo"=GRC,asignatura,actividad,intentos,fechalimiteentrega,fechadeenvio,modificado,
                                                 calificacion,statusentrega,retro,num_archivos,aula) %>%
  filter(statusentrega=="Enviado para calificar") %>% mutate(`Tipo actividad`=if_else(calificacion=="-1","Pendiente","Calificada"))#,retro=="Sin Retro",calificacion  == "-1"
#####--------Datos profesores--------#####
Rep <- leer(c("Acc_Prof_Mae","Otros$")) %>% codificacion() %>% filter(rol!="cobranza",status=="Activo",substring(matricula,1,4)=="0198") %>%
  mutate(matricula=as.numeric(matricula)) %>% unite(clave_g,clave,grupo) %>% select(clave_g,"Matricula_Docente"=matricula,"Docente"=profesor) %>% filter(!duplicated(clave_g))
Base <- left_join(Base,Rep,"clave_g")
#####--------Tiempos--------#####
Base <- mutate(Base,modificado=as.Date(modificado),`Días Sin Calificar`=Sys.Date()-modificado,`Días Sin Calificar`=gsub(" days","",`Días Sin Calificar`),
               `Días Sin Calificar`=if_else(calificacion=="-1","-1",`Días Sin Calificar`))
#####--------Links--------#####
Rep <- leer(c("links_actividades_posgra","Otros$"),col_select=c(IDENT,calificador)) %>% codificacion() %>% filter(!duplicated(IDENT))
Base <- unite(Base,IDENT,clave,actividad,aula,remove=F) %>% left_join(Rep,"IDENT")
#####--------Semana entrega--------#####
Base <- mutate(Base,Semana="_"); fi <- as.Date("2023-02-27")
for (i in 1:15) {
  Base <- mutate(Base,Semana=if_else(modificado>=fi & modificado<=(fi + days(6)),paste("S",i,sep=""),Semana))
  fi <- fi+days(7)
}
#####--------Campus y regla negocio--------#####
Rep <- leer(c("Estatus General Alumnos","SIR$"),col_select=c("matricula"=MATRICULA,"Clave Campus"=CAMPUS)) %>% filter(!duplicated(matricula)) %>%
  mutate(matricula=as.character(as.numeric(matricula)))
Base <- left_join(Base,Rep,"matricula")
Rep <- leer(c("Info General","gsheet"),secc="Campus") %>% select(1:3)
Base <- left_join(Base,Rep,"Clave Campus")
#####--------Reordenando e imprimiendo--------#####
Base <- select(Base,matricula,`Clave Campus`,`Tipo Campus`,Nombre.Alumno,clave,"Grupo"=grupo,asignatura,actividad,`Tipo actividad`,intentos,"Entregado"=modificado,Semana,
               `Días Sin Calificar`,calificacion,statusentrega,retro,num_archivos,aula,Matricula_Docente,Docente,calificador) %>% filter(num_archivos!="0")
Base <- mutate_all(Base,~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
escribir(Base,c("Actividades Cal y Pen Posgrados.csv","Principales$"))



















