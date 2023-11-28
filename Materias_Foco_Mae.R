sapply(c("MultiLEDS","dplyr","tidyr","lubridate"), library,character.only=T)
#diremail("//Tut-095/reportes/NR Licenciatura")
diremail("C:/Users/jsalinba/Documents/Reportes","jsalinba@utel.edu.mx")
#####-----------Descargas Sabana drive-----------#####
fecha <- ymd("2023-11-27")
while (fecha <= Sys.Date()){
  drive_sd("des",c(paste("Niveles/Sabana_POSGRADOS_",fecha,".csv",sep=""),"Sabana/Nov-Dic 23$"))
  print(paste("Niveles/Sabana_POSGRADOS_",fecha,".csv",sep=""))
  fecha <- fecha + days(1)
}
#####------Inicio------#####
Base <- unificar("Sabana/Nov-Dic 23$",iden = "nom") %>% filter(Status.Materia=="Activo") %>% filter(Inicio.de.curso=="2023-10-23") %>%
  mutate(`Tipo sabana`=if_else(grepl("cierre",iden,ignore.case = T),"Cierre","Normal"),iden=substring(iden,18,27),iden=ymd(iden),`Semana extracción`=0) %>%
  rename("Fecha extracción"=iden)
fecha <- ymd("2023-10-23")
#Semanas
for (i in 0:15) {
  Base <- mutate(Base,`Semana extracción`=if_else(`Fecha extracción`>=fecha & `Fecha extracción`<=(fecha+days(6)),i,as.integer(`Semana extracción`)))
  fecha <- fecha+days(7)
}
#Sabana Ene-Feb21, reemplazando, agregando, quitando y ordenando columnas
Rep <- leer(c("Sabana Posgrados Ene-Feb 21","Históricos/Sabana$")) %>% rename("Tipo sabana"=`Tipo Base`)
Rep <- Rep[filter(cols,!is.na(`Orden final`)) %>% .[["Orden final"]]]
#Uniendo bases
Rep["Bimestre"] <- "Ene-Feb 21"; Base["Bimestre"] <- "Nov-Dic 23"
Base <- rbind(Base,Rep)
# #Materias B1 (Foco)
# Rep <- leer(c("Materias B1 Foco","auxiliares$"),sheet="Hoja7") %>% mutate(`Tipo Materia`="Foco") %>% select(Clave,`Tipo Materia`)
# Base <- left_join(Base,Rep,by="Clave")
#Reglas negocio
#diremail("//Tut-095/reportes/ESTADOS")
Rep <- leer("//Tut-095/reportes/ESTADOS/reglas_negocios.csv") %>% filter(!duplicated(MATRICULA)) %>% rename("Matricula"=MATRICULA)
Base <- left_join(Base,Rep,by="Matricula")
#Alianzas
Rep <- leer("//Tut-095/reportes/ESTADOS/alianzamodalida.csv") %>% select("MATPROG"=Matr,Alianza) %>% filter(!duplicated(MATPROG))
Base <- unite(Base,MATPROG,Matricula,Clave_Programa,sep="",remove=F)
Base <- left_join(Base,Rep,by="MATPROG")
#Porcentages
Rep <- count(Base,Asignatura,NR,Semana,Bimestre,Alianza,Modalidad,Regla_Negocio,Bloque.seguimiento,Tutor,Supervisor) %>% filter(!is.na(NR)) %>%
  spread(key="NR",value="n") %>% mutate_at(10:14,~replace(.,is.na(.),0)) %>% mutate(Total=Aprobado+Np+Nuncas+`Por aprobar`+Reprobó,
                                                                                    Aprobación=(Aprobado+`Por aprobar`)/Total)
Rep <- mutate(Rep,Rango=if_else(Aprobación<0.21,"0-20%",
                                if_else(Aprobación<0.41,"21-40%",
                                        if_else(Aprobación<0.61,"41-60%",
                                                if_else(Aprobación<0.86,"61-85%","86-100%")))))
#Reemplazando NA e imprimiendo
Base <- mutate_all(Base,~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
Rep <- mutate_all(Rep,~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
diremail("C:/Users/jsalinba/Documents/Reportes")
escribir(Base,c("Materias Foco Mae.csv","Principales$"))
escribir(Rep,c("Materias Foco Mae Porcentajes.csv","Principales$"))














