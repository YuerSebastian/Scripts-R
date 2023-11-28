library(MultiLEDS); library(tidyr); library(dplyr); library(lubridate); library(readxl); library(vroom)
diremail("C:/Users/jsalinba/Documents/Reportes","jsalinba@utel.edu.mx")
#diremail("//Tut-095/reportes","jsalinba@utel.edu.mx")
#####-----------Correos Docentes-----------#####
drive_sd("des",c("Reports/correos_docentes_posgra.csv","CRM$"))
FI <- ymd("2023-11-27")
while (FI <= Sys.Date()){
  drive_sd("des",c(paste("Niveles/Sabana_POSGRADOS_",FI,".csv",sep=""),"Sabana/Nov-Dic 23$"))
  FI <- FI + days(1)
}
#####-------------------------Unificando y filtrando-------------------------#####
FI <- ymd("2023-10-23"); FF <- FI + days(6) #Especificando solo Semana 1 como inicio
Base <- unificar("Sabana/Nov-Dic 23$",iden="nom",
                 col_select=c(Matricula,Modalidad,Bloque.seguimiento,Clave,Asignatura,Status.Materia,Estado.Banner,Duración,Inicio.de.curso,Mat.Profesor,Profesores,NR))%>%
  mutate(iden=substring(iden,18,27),iden=ymd(iden),Semana=if_else(iden>=FI & iden<=FF,"Semana 0","_"),Bimestre="Nov-Dic 23",
         Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula),inicio_plus=paste(substring(Inicio.de.curso,9,10),substring(Inicio.de.curso,6,7),sep=""))%>%
  rename("Fecha_creacion"=iden) %>% filter(Status.Materia=="Activo",Estado.Banner %in% c("ADMITIDO","MATRICULADO"),Inicio.de.curso=="2023-10-23",Duración == "Bimestral",
         !grepl("IEBSM|ONUM|SEM|UBAM|SEL|IEBSL|UNICL|COUMA|SSENM01|M1HB|FACE",Clave),Modalidad!="Doctorado")
#####-------------------------Dividiendo semanas-------------------------#####
for (i in 1:8) {
  FI <- FF + days(1); FF <- FI + days(6)
  Base <- mutate(Base,Semana=if_else(Semana=="_",
                                     if_else(Fecha_creacion>=FI & Fecha_creacion<=FF,paste("Semana ",i,sep=""),Semana),Semana))
}





# Rep <- leer("//Tut-095/REPORTES/ESTADOS/reglas_negocios.csv") %>% rename("Matricula"=MATRICULA) %>% filter(!duplicated(Matricula))
# 
# Base <- vroom("C:/Users/jsalinba/Documents/Reportes/Bases/Históricos/Sabana/Sabana Posgrados Nov-Dic 23.csv",",",
#               col_select = c(Matricula,Modalidad,Bloque.seguimiento,Clave,Asignatura,Status.Materia,Estado.Banner,Duración,Inicio.de.curso,Mat.Profesor,
#                              Profesores,NR,"Fecha_creacion"=`Fecha extracción`,"Semana" = `Semana extracción`),
#               col_types = cols(.default = "c"),locale = locale(encoding = "LATIN1")) %>%
#   filter(Status.Materia=="Activo",Estado.Banner %in% c("ADMITIDO","MATRICULADO","PREMATRICULADO"),Inicio.de.curso=="2023-10-23",Duración == "Bimestral",
#          !grepl("IEBSM|ONUM|SEM|UBAM|SEL|IEBSL|UNICL|COUMA|SSENM01|M1HB|FACE",Clave),Modalidad!="Doctorado") %>%
#   mutate(inicio_plus=paste0(substring(Inicio.de.curso,9,10),substring(Inicio.de.curso,6,7)),Bimestre="Nov-Dic 23") %>% left_join(Rep,"Matricula") %>%
#   count(Asignatura,Bloque.seguimiento,NR,Fecha_creacion,Semana,Bimestre,Regla_Negocio) %>%
#   pivot_wider(names_from = NR, values_from = n, values_fill = 0) %>% mutate(Semana = gsub("S","Semana ",Semana))
# 
# 
# vroom_write(Base,"C:/Users/jsalinba/Documents/Reportes/Bases/Principales/Aprobación Mae.csv",",","\r\n",bom = T)










#####-------------------------Sin Bloque-------------------------#####
# diremail("//Tut-095/Aprobacion_Posgrados")
# Rep <- filter(Base,is.na(Bloque.seguimiento))
# Rep2 <- leer(c("sinbloquejulago21","Posgrados$")) %>% mutate(Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula)) %>% rename("BS"=Bimestre)
# Rep <- left_join(Rep,Rep2,by="Matricula") %>% mutate(Bloque.seguimiento=BS) %>% select(-BS)
# Base <- filter(Base,!is.na(Bloque.seguimiento)) %>% rbind(Rep)
# remove(Rep2)
#####-------------------------Regla Negocio-------------------------#####
diremail("//Tut-095/reportes")
Rep <- leer(c("reglas_negocios","ESTADOS$")) %>% rename("Matricula"=MATRICULA) %>% mutate(Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula))
Base <- left_join(Base,Rep,by="Matricula")
#####-------------------------Docentes-------------------------#####
diremail("C:/Users/jsalinba/Documents/Reportes")
Base <- unite(Base,clave_corr,Matricula,Clave,sep="",remove=F)
Rep <- leer(c("correos_docentes_posgra","CRM$")) %>% mutate(Fecha_Envio=substring(Fecha_Envio,1,10),Fecha_Envio=ymd(Fecha_Envio),
                                                            matricula=if_else(nchar(matricula)<9,paste("0",matricula,sep=""),matricula)) %>%
  rename("Matricula"=matricula)
FI <- ymd("2023-10-23"); FF <- FI + days(6)
#i<-1
Rep4 <- data.frame()
for (i in 1:10) {
  Rep2 <- filter(Rep,Fecha_Envio>=FI & Fecha_Envio<=FF) %>% count(Matricula,asignatura,clave,id_envio,role,aula,status_lectura) %>% spread("status_lectura","n")
  if (length(rownames(Rep2))>0) {
    Rep2 <- mutate_at(Rep2,c(7,8),~if_else(is.na(.),0L,.)) %>% separate(clave,into = c("ciclo","num","inicio_plus","materia")) %>%
      unite(clave_corr,Matricula,materia,sep = "",na.rm = T) %>% select(clave_corr,Abierto,Sin_Abrir) %>% filter(!duplicated(clave_corr))
  }else{
    Rep2 <- mutate(Rep2,Abierto=0,Sin_Abrir=0,clave_corr="_") %>% select(clave_corr,Abierto,Sin_Abrir) #Se queda vacío a falta de datos
  }
  Rep3 <- filter(Base,Fecha_creacion>=FI & Fecha_creacion<=FF)
  if (hasName(Rep3,"Abierto")) {
    Rep3 <- select(Rep3,-Abierto,Sin_abrir)
  }
  Rep3 <- left_join(Rep3,Rep2,by="clave_corr")
  if(length(Rep4)==0) Rep4 <- Rep3 else Rep4 <- rbind(Rep4,Rep3)
  FI <- FF + days(1); FF <- FI + days(6)
}
Base <- Rep4
#Base <- mutate(Base,Abierto=0L,Sin_Abrir=0L)
Base <- mutate(Base,Sin_Envio=if_else(is.na(Abierto) & is.na(Sin_Abrir),1L,0L),
               Total_envios=Abierto+Sin_Abrir)
Base <- mutate_at(Base,19:22,~if_else(is.na(.),0L,as.integer(.)))
escribir(Base,c("Docentes Mae.csv","Principales$"))
# Rep <- count(Base,Mat.Profesor,Profesores,Modalidad,Asignatura,Bloque.seguimiento,NR,Fecha_creacion,Semana,Bimestre,Bloque.seguimiento,Regla_Negocio) %>% spread("NR","n")%>%
#   mutate_at(10:14,~if_else(is.na(.),0L,.))
#escribir(Rep,c("Docentes Mae.csv","Principales$"))
#####-------------------------Aprobación-------------------------#####
Base <- count(Base,Asignatura,Bloque.seguimiento,NR,Fecha_creacion,Semana,Bimestre,Regla_Negocio) %>% spread("NR","n")
Base <- mutate_at(Base,7:length(Base),~if_else(is.na(.),0L,.))
#Base <- mutate(Base,`Por aprobar`=0L)
escribir(Base,c("Aprobación Mae.csv","Principales$"))

















