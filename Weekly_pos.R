sapply(c("MultiLEDS","dplyr","tidyr","lubridate","vroom"), library,character.only=T)
# diremail("//Tut-095/reportes/NR Licenciatura")
diremail("C:/Users/jsalinba/Documents/Reportes","jsalinba@utel.edu.mx")
fi <- Sys.Date()
fa <- fi - days(7)
#####-----------Indicadores-----------#####
#Sabana
#diremail("//Tut-095/reportes/NR Licenciatura")
Rep <- leer(c("Orden Sab y NR","Bases$"),sheet="Inicios") %>% unite(Cie,Bimestres,Cierre,na.rm = T,remove = F) %>% select(-`Fechas inicio`,-Bimestres)
Base <- unificar(c("Históricos/Sabana$","IA")) %>% filter(wday(ymd(`Fecha extracción`))==1 | `Fecha extracción`==as.character(Sys.Date())) %>%
  mutate(`Tipo Campus`=gsub("UTEL ","",`Tipo Campus`)) %>% unite(Cie,Bimestre,`Semana extracción`,remove = F) %>%
  left_join(Rep,"Cie") %>% mutate(Cierre=if_else(!is.na(Cierre),"Cierre","No"),`Semana extracción`=if_else(Cierre=="Cierre","Cierre",`Semana extracción`)) %>%
  select(-Cierre,-Cie)
escribir(Base,c("IA sabana Posgrados.csv","Principales$"))
#NR
Base <- unificar(c("Históricos/NR$","IA")) %>% filter(wday(ymd(`Fecha extracción`))==1 | `Fecha extracción`==as.character(Sys.Date())) %>%
  mutate(`Tipo Campus`=gsub("UTEL ","",`Tipo Campus`)) %>% unite(Cie,Bimestre,`Semana extracción`,remove = F) %>%
  left_join(Rep,"Cie") %>% mutate(Cierre=if_else(!is.na(Cierre),"Cierre","No"),`Semana extracción`=if_else(Cierre=="Cierre","Cierre",`Semana extracción`)) %>%
  select(-Cierre,-Cie)
escribir(Base,c("IA NR Posgrados.csv","Principales$"))
#####-----------NR Actual-----------#####
fecha <- fi
Base <- leer(c(paste("NR_POSGRADOS_",fecha,sep=""),"NR/Nov-Dic 23$"))
Rep <- leer(c("Conteos_Interacciones","CRM$"),col_select=c("Matricula"=matricula,Contacto)) %>% filter(!duplicated(Matricula))
Base <- left_join(Base,Rep,by="Matricula")
#####-----------NR Anterior-----------#####
Rep <- leer(c(paste("NR_POSGRADOS_",(fecha-days(1)),sep=""),"NR/Nov-Dic 23$"),col_select=c(Matricula,"moras BA"=moras)) %>% filter(!duplicated(Matricula))
Base <- left_join(Base,Rep,by="Matricula") %>% mutate(`moras BA`=replace_na(`moras BA`,"Sin mensualidad"),`moras BA`=paste(`moras BA`," BA",sep=""))
#####-----------Semana Anterior-----------#####
fecha <- fa
Rep <- leer(c(paste("NR_POSGRADOS_",fecha,sep=""),"NR/Nov-Dic 23$"),col_select=c(Matricula,"Semaforo SA"=Semaforo)) %>% filter(!duplicated(Matricula))
Base <- left_join(Base,Rep,by="Matricula") %>% mutate(`Semaforo SA`=replace_na(`Semaforo SA`,"Sin Semaforo"),
                                                      `Cambio Semaforo`=if_else(`Semaforo SA`!="Sin Semaforo",
                                                                                if_else(Semaforo!=`Semaforo SA`,"Cambio","Sin Cambio"),"Sin Semaforo"),
                                                      `Semaforo SA`=paste(`Semaforo SA`," SA",sep=""))
#####-----------Tipo Campus-----------#####
Rep <- leer(c("Estatus General Alumnos","SIR$"),col_select=c(MATRICULA,PROGRAMA,CAMPUS)) %>% mutate(PROGRAMA=substring(PROGRAMA,1,10),MATRICULA=as.integer(MATRICULA)) %>%
  unite(MATPROG,MATRICULA,PROGRAMA,sep="",na.rm=T) %>% filter(!duplicated(MATPROG)) %>% rename("Clave Campus"=CAMPUS)
Base <- unite(Base,MATPROG,Matricula,Clave.Programa,remove=F,na.rm=T,sep="") %>% left_join(Rep,by="MATPROG") %>% select(-MATPROG)
#Tipos
Rep <- leer(c("Info General","gsheet"),sheet="General",secc="Campus") %>% select(`Clave Campus`,`Tipo Campus`)
Base <- left_join(Base,Rep,by="Clave Campus")
#####-----------Reglas negocio-----------#####
diremail("//Tut-095/reportes/ESTADOS")
Rep <- leer(c("reglas_negocios","ESTADOS$")) %>% filter(!duplicated(MATRICULA)) %>% rename("Matricula"=MATRICULA)
Base <- left_join(Base,Rep,by="Matricula")
#####-----------Alianzas-----------#####
Rep <- leer(c("alianzamodalida","ESTADOS$")) %>% select("MATPROG"=Matr,Alianza) %>% filter(!duplicated(MATPROG))
Base <- unite(Base,MATPROG,Matricula,Clave.Programa,sep="",remove=F)
Base <- left_join(Base,Rep,by="MATPROG")
#####-----------Tipo contacto-----------#####
Base <- mutate(Base,`Tipo Contacto`=if_else(Medio.de.Contacto %in% c("calixta","whatsapp","fb_tut_calixta"),"Whatsapp",
                                            if_else(Medio.de.Contacto %in% c("llamada_whatsapp","telefono"),"Llamada",
                                                    if_else(Medio.de.Contacto=="hangouts","Hangouts",
                                                            if_else(Medio.de.Contacto=="correo_inst","Correo",
                                                                    if_else(Medio.de.Contacto=="crisp","Crisp",
                                                                            if_else(Medio.de.Contacto=="zoom","Zoom","Sin Registro")))))))
diremail("C:/Users/jsalinba/Documents/Reportes","jsalinba@utel.edu.mx")

Base <- mutate(Base,Bloque.Ingreso=substring(Bloque.Ingreso,1,2))
Base <- unite(Base,IDG,Bloque.Ingreso,Alianza,Regla_Negocio,remove = F)
Base <- mutate(Base,Bimestre="Nov-Dic 23",`Tipo Campus`=gsub("UTEL ","",`Tipo Campus`))
escribir(Base,c("NR Posgrados Actual.csv","Principales$"))
#####-----------Contactos y semaforo sabana-----------#####
fecha <- fi
#Semaforo
Rep <- leer(c(paste("NR_POSGRADOS_",fecha,sep=""),"NR/Nov-Dic 23$"),col_select=c(Matricula,Semaforo)) %>% filter(!duplicated(Matricula))
Base <- leer(c(paste("Sabana_POSGRADOS_",fecha,sep=""),"Sabana/Nov-Dic 23$")) %>% unite(clave_corr,Matricula,Clave,sep="",remove=F) %>%
  left_join(Rep,by="Matricula") %>%
  mutate(Fecha_mat=as.Date(ymd_hm(Acceso.Materia)),Días=if_else(Sys.Date()-Fecha_mat<6,"0 a 5 días",
                                                                if_else(Sys.Date()-Fecha_mat<11,"10 días",
                                                                        if_else(Sys.Date()-Fecha_mat<21,"20 días","+ 20 días"))),
         Días=replace_na(Días,"+ 20 días"))
#Contactos
Rep <- leer(c("correos_docentes_posgra","CRM$")) %>%
  mutate(Fecha_Envio=substring(Fecha_Envio,1,10),Fecha_Envio=ymd(Fecha_Envio),matricula=if_else(nchar(matricula)<9,paste("0",matricula,sep=""),matricula)) %>%
  rename("Matricula"=matricula)
fecha <- fa
Rep <- filter(Rep,Fecha_Envio==fecha) %>% count(Matricula,asignatura,clave,id_envio,role,aula,status_lectura) %>% spread("status_lectura","n")

if (length(rownames(Rep))>0) {
  Rep <- mutate_at(Rep,c(7,8),~if_else(is.na(.),0L,.)) %>% separate(clave,c("ciclo","num","inicio_plus","materia")) %>%
    unite(clave_corr,Matricula,materia,sep = "",na.rm = T) %>% select(clave_corr,Abierto,Sin_Abrir) %>% filter(!duplicated(clave_corr))
}else{
  Rep <- mutate(Rep,Abierto=0,Sin_Abrir=0,clave_corr="_") %>% select(clave_corr,Abierto,Sin_Abrir) #Se queda vacío a falta de datos
}
Base <- left_join(Base,Rep,by="clave_corr") %>% select(-clave_corr)
Base <- mutate(Base,Sin_Envio=if_else(is.na(Abierto) & is.na(Sin_Abrir),1L,0L),Total_envios=Abierto+Sin_Abrir,Bimestre="Nov-Dic 23")
Base <- mutate_at(Base,57:60,~if_else(is.na(.),0L,as.integer(.)))

escribir(Base,c("Sabana_POSGRADOS Actual.csv","Bases$"))




Base <- vroom("C:/Users/jsalinba/Documents/Reportes/Bases/Principales/IA NR Posgrados.csv",",",locale = locale(encoding = "LATIN1"),
              col_types = cols(.default = "c")) %>%
  filter(grepl("21|Ene-Feb 22|Nov-Dic 23",Bimestre))





Rep <- count(Base,Bimestre)









