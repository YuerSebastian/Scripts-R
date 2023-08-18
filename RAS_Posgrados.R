#####-----Resultados acaémicos semanales posgrados-----#####
sapply(c("MultiLEDS","dplyr","tidyr","lubridate","RMySQL"),library,character.only=T)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#####----------------Descarga de archivos----------------#####
#Se toma el domingo como cierre de cada semana.
fecha <- ymd("2023-03-05"); fi <- fecha
drive_sd("des",c("Reports/correos_docentes_posgra.csv","CRM$"))#; drive_sd("des",c("Contactacion/Interacciones_gral.csv","CRM$"))
while (fi <= Sys.Date()){
  drive_sd("des",c(paste("Niveles/NR_POSGRADOS_",fi,".csv",sep=""),"Originales/NR$"))
  fi <- fi + days(7)
}
#####----------------Inicio----------------#####
fi <- fecha; Base <- data.frame(); s <- 1
while (fi <= Sys.Date()) {
  if (s!=1) {
    Rep2 <- leer(c(paste("NR_POSGRADOS_",fi - days(7),sep=""),"Originales/NR$"),
                 col_select=c(Matricula,"SA"=Semaforo,"EMA"=edo.moodle,"EBA"=Estado.Banner)) %>% filter(!duplicated(Matricula))
    Rep <- leer(c(paste("NR_POSGRADOS_",fi,sep=""),"Originales/NR$"),
                col_select=c(Matricula,Campus,Bloque,Semaforo,edo.moodle,Estado.Banner,Modalidad,Ciclo,Ingreso,Facultad,Homologado)) %>%
      filter(!duplicated(Matricula),Semaforo!="Sin riesgo académico") %>% left_join(Rep2,"Matricula") %>%
      mutate(Cambio=if_else(Semaforo!=SA,"Si","No"),
             `Tipo Cambio`=if_else(!is.na(SA) & Cambio=="Si",if_else(SA=="Sin riesgo académico","Negativo",
                                                                     if_else(SA=="Riesgo académico",if_else(Semaforo=="Sin riesgo académico","Positivo","Negativo"),
                                                                             "Positivo")),"Ninguno"),
             Motivo=if_else(Cambio=="Si",if_else(EBA %in% c("ADMITIDO","MATRICULADO") & !Estado.Banner %in% c("ADMITIDO","MATRICULADO"),"Estado","Calificación"),"Ninguno"),
             `Tipo Estado`=if_else(Estado.Banner %in% c("ADMITIDO","MATRICULADO"),"Activo","Inactivo"),Semana=paste("S",s,sep=""))
  }else{
    Rep <- leer(c(paste("NR_POSGRADOS_",fi,sep=""),"Originales/NR$"),
                col_select=c(Matricula,Campus,Bloque,Semaforo,edo.moodle,Estado.Banner,Modalidad,Ciclo,Ingreso,Facultad,Homologado)) %>%
      filter(!duplicated(Matricula),Semaforo!="Sin riesgo académico") %>%
      mutate(SA="_",EMA="_",EBA="_",Cambio="_",`Tipo Cambio`="_",Motivo="_",`Tipo Estado`="_",Semana=paste("S",s,sep=""))
  }
  print(paste("Semana ",s," completa. ",fi,sep=""))
  fi <- fi + days(7); s <- s+1
  if(length(Base)==0) Base <- Rep else Base <- rbind(Base,Rep)
}
#####----------------Correos----------------#####
Rep <- leer(c("correos_docentes_posgra","CRM$"),col_select=c("Matricula"=matricula,role,Tipo,Fecha_Envio,"Correo"=status_lectura)) %>% codificacion() %>%
  filter(role=="student",Tipo=="to") %>% mutate(Fecha_Envio=as.Date(Fecha_Envio),Sem="_") %>% arrange(desc(Fecha_Envio))
fi <- fecha
for (i in 1:15) {
  Rep <- mutate(Rep,Sem=if_else(Fecha_Envio>=fi & Fecha_Envio<=(fi + days(6)),paste("S",i,sep=""),Sem))
  fi <- fi + days(7)
}
Rep <- filter(Rep,Sem!="_") %>% unite(MS,Matricula,Sem) %>% select(MS,Correo) %>% filter(!duplicated(MS))
Base <- unite(Base,MS,Matricula,Semana,remove = F) %>% left_join(Rep,"MS") %>% mutate(Correo=replace_na(Correo,"No enviado")) %>% select(-MS)
#####----------------Regla negocio y Tipo Campus----------------#####
Rep <- leer(c("reglas_negocios","Bases$")) %>% filter(!duplicated(MATRICULA)) %>% rename("Matricula"=MATRICULA)
Base <- left_join(Base,Rep,"Matricula")
Base <- mutate(Base,`Tipo Campus`=if_else(!is.na(Campus),if_else(grepl("Colombia",Campus),"Colombia",
                                                                 if_else(grepl("Ecuador",Campus),"Ecuador",
                                                                         if_else(grepl("Perú",Campus),"Perú",
                                                                                 if_else(grepl("UTEL",Campus),"México",
                                                                                         if_else(grepl("USA",Campus),"USA","ROLA"))))),"_"))
Base <- unite(Base,IDG,Matricula,Semana,remove = F) %>% mutate_all(~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
escribir(Base,c("RAS Posgrados.csv","Principales$"))











