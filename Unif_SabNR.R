sapply(c("MultiLEDS","dplyr","tidyr","lubridate"), library,character.only=T)
diremail("C:/Users/jsalinba/Documents/Reportes","jsalinba@utel.edu.mx")
#####-----------Descargas-----------#####
fecha <- ymd("2023-11-27")
while (fecha <= Sys.Date()){
  drive_sd("des",c(paste("Niveles/NR_POSGRADOS_",fecha,".csv",sep=""),"NR/Nov-Dic 23$"))
  drive_sd("des",c(paste("Niveles/Sabana_POSGRADOS_",fecha,".csv",sep=""),"Sabana/Nov-Dic 23$"))
  fecha <- fecha + days(1)
}
#####----------NR Unificado x Bimestre----------#####
B = c("Nov-Dic 23")#c("Mar-Abr 21","Jul-Ago 21","Nov-Dic 21","Ene-Feb 22","Mar-Abr 22")
info <- leer(c("Orden Sab y NR","Bases$"),sheet="Inicios") %>% filter(Bimestres %in% B)
cols <- leer(c("Orden Sab y NR","Bases$"),sheet="NR") %>% .[["Actual"]]
for (i in 1:length(info[[1]])) {
  FI <- ymd(info[[i,"Fechas inicio"]])
  Base <- unificar(paste("Originales/NR/",info[[i,"Bimestres"]],"$",sep=""),
                   c(matricula="Matricula",`bloque ingreso`="Bloque.Ingreso",`Por aprobar`="Por.aprobar",`Total general`="Total.general",edomoddle="edo.moodle",
                     Acesso="Acc.Matería",`Materias Reprobadas`="Mat.Reprobadas",edoescolares="Estado.Banner",adeudo="Adeudo",mora="moras",
                     Asignacion="Estado.Asignacion",FECHA_INICIO="Ciclo",`Tipo de Alumno`="Ingreso",`Bloque seguimiento`="Bloque",`Max estado`="Fecha.Estatus",
                     `Nombre Completo`="Nombre.Completo",`Materias Reprobadas`="Reprobadas"),
                   cols,iden="nom") %>% mutate(Semana = "_",Bimestre=info[[i,"Bimestres"]]) %>%
    mutate(`Tipo Base`=if_else(grepl("cierre",iden,T),"Cierre","Normal"),iden=substring(iden,14,23),iden=ymd(iden)) %>% rename("Fecha extracción"=iden)
  #Bucle para semanas completas
  for (j in 0:15) {
    Base <- mutate(Base,Semana=if_else(`Fecha extracción`>=FI & `Fecha extracción`<=(FI + days(6)),paste("S",j,sep=""),Semana))
    FI <- FI + days(7)
  }
  #Bucle para Semana Santa
  # j <- 0; s <- j
  # while (j<=15) {
  #   Base <- mutate(Base,Semana=if_else(`Fecha extracción`>=FI & `Fecha extracción`<=(FI + days(6)),paste0("S",s),Semana))
  #   FI <- FI + days(7)
  #   if(FI >= ymd("2023-04-03") & FI <= ymd("2023-04-09")){
  #     s <- "S"; next
  #   }
  #   j <- j+1; s <- j
  # }
  Base <- mutate_all(Base,~as.character(.)) %>% mutate_all(~replace_na(.,""))
  escribir(Base,c(paste("NR Posgrados ",info[[i,"Bimestres"]],".csv",sep=""),"Históricos/NR"))
  print(paste("Bimestre",info[[i,"Bimestres"]],"completo."))
}
#####----------Sabana Unificado x Bimestre----------#####
info <- leer(c("Orden Sab y NR","Bases$"),sheet="Inicios") %>% filter(Bimestres %in% B)
cols <- leer(c("Orden Sab y NR","Bases$"),sheet="Sabana") %>% .[["Actual"]]
for (i in 1:length(info[[1]])){
  FI <- ymd(info[[i,"Fechas inicio"]])
  Base <- unificar(paste("Originales/Sabana/",info[[i,"Bimestres"]],"$",sep=""),
                   c(Duracion="Duración",Estado.plataforma="Status.Plataforma",edo_moodle="Status.Materia",matricula="Matricula",Celular="Telefono",
                     Clave_Asigntaura="Clave",Calificación="Calificacion",nivel_riesgo="NR",Tipo_Materia="TM",Nivel="Modalidad",Mat_Profesor="Mat.Profesor",
                     profesor="Profesores",B.ingreso="Bloque.Ingreso",Asignación="Estado.Asignacion",Estado_Banner="Estado.Banner",Fecha.Inicio="Inicio.de.curso",
                     SemanasTranscurridas="Semana",AccesosMateria="Acceso.Materia",AccesoAula="Acceso.Aula",`Ubicación/Estado`="Ubicación_Estado"),
                   cols,iden="nom") %>% mutate(`Semana extracción` = "_",Bimestre=info[[i,"Bimestres"]],
                                               `Tipo Base`=if_else(grepl("cierre",iden,T),"Cierre","Normal"),
                                               iden=substring(iden,18,27),iden=ymd(iden)) %>% rename("Fecha extracción"=iden)
  #Bucle para semanas completas
  for (j in 0:15) {
    Base <- mutate(Base,`Semana extracción`=if_else(`Fecha extracción`>=FI & `Fecha extracción`<=(FI + days(6)),paste("S",j,sep=""),`Semana extracción`))
    FI <- FI + days(7)
  }
  #Bucle para Semana Santa
  # j <- 0; s <- j
  # while (j<=15) {
  #   Base <- mutate(Base,`Semana extracción`=if_else(`Fecha extracción`>=FI & `Fecha extracción`<=(FI + days(6)),paste0("S",s),`Semana extracción`))
  #   FI <- FI + days(7)
  #   if(FI >= ymd("2023-04-03") & FI <= ymd("2023-04-09")){
  #     s <- "S"; next
  #   }
  #   j <- j+1; s <- j
  # }
  Base <- mutate_all(Base,~as.character(.)) %>% mutate_all(~replace_na(.,""))
  escribir(Base,c(paste("Sabana Posgrados ",info[[i,"Bimestres"]],".csv",sep=""),"Históricos/Sabana$"))
  print(paste("Bimestre",info[[i,"Bimestres"]],"completo."))
}
#####----------Indicadores Sabana Pos----------#####
Base <- unificar(c("Históricos/Sabana$"," 23"),
                 col_select=c(Matricula,Bimestre,Bloque.seguimiento,Ciclo,`Fecha extracción`,`Tipo Base`,"Campus NR"=Campus,Facultad,Homologado,Modalidad,Semana,
                              `Semana extracción`,Estado.Banner,Status.Materia,NR)) %>%
  filter(!is.na(NR)) %>% mutate(Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula))
#Campus
Rep <- leer(c("Estatus General Alumnos","SIR$"),col_select=c("Matricula"=MATRICULA,"Clave Campus"=CAMPUS)) %>% filter(!duplicated(Matricula))
Base <- left_join(Base,Rep,by="Matricula")
Rep <- leer(c("Info General","gsheet"),secc="Campus") %>% select(`Clave Campus`,Campus,`Tipo Campus`)
Base <- left_join(Base,Rep,by="Clave Campus")
#Reglas negocios
diremail("//Tut-095/reportes/ESTADOS")
Rep <- leer(c("reglas_negocios","ESTADOS$")) %>% rename("Matricula"=MATRICULA) %>% filter(!duplicated(Matricula)) %>%
  mutate(Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula))
Base <- left_join(Base,Rep,by="Matricula")
#Reglas negocios
# Rep <- leer(c("alianzamodalida","ESTADOS$"),col_select=c("Matricula"=Matr,Alianza)) %>% filter(!duplicated(Matricula))
# Base <- left_join(Base,Rep,by="Matricula")
Base <- count(Base,Bimestre,Ciclo,`Fecha extracción`,`Tipo Base`,Bloque.seguimiento,`Campus NR`,`Clave Campus`,Campus,Facultad,Homologado,`Tipo Campus`,
              Regla_Negocio,Modalidad,Semana,`Semana extracción`,Estado.Banner,Status.Materia,NR) %>% spread(NR,n) %>% mutate_at(18:22,~replace_na(.,0))

Base <- mutate_all(Base,~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
diremail("C:/Users/jsalinba/Documents/Reportes")
escribir(Base,c("IA Sabana Posgrados 2023.csv","Históricos/Sabana$"))
#####----------Indicadores NR Pos----------#####
Base <- unificar(c("Históricos/NR$"," 23"),
                 col_select=c(Matricula,Bimestre,Bloque,Ciclo,`Fecha extracción`,`Tipo Base`,"Campus NR"=Campus,Facultad,Homologado,Modalidad,
                              "Semana extracción"=Semana,Estado.Banner,edo.moodle,Semaforo)) %>%
  filter(!Semaforo %in% c("-","_",NA)) %>% mutate(Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula))
#Campus y cálculo de edad
Rep <- leer(c("Estatus General Alumnos","SIR$"),col_select=c("Matricula"=MATRICULA,"Clave Campus"=CAMPUS,"FN"=FECHA_NAC,"Ed"=EDAD)) %>%
  filter(!duplicated(Matricula)) %>%
  mutate(FN=dmy(FN),Edad=as.integer(as.integer(Sys.Date()-FN)/365),Ed=as.integer(Ed),
         Edad=if_else(Ed==Edad,if_else(Ed>15 & Ed<96,Edad,0),
                      if_else(Ed<16 | Ed>95,if_else(Edad>15 & Edad<96,Edad,0),
                              if_else(Ed>15 & Ed<96,if_else(Edad>15 & Edad<96,Edad,Ed),0))),
         `Tipo Edad`=if_else(Ed==Edad,if_else(Ed>15 & Ed<96,"Si","No"),
                             if_else(Ed<16 | Ed>95,if_else(Edad>15 & Edad<96,"Si","No"),
                                     if_else(Ed>15 & Ed<96,"Si","No"))),
         `Tipo Edad`=if_else(`Tipo Edad`=="Si",
                             if_else(Edad<22,"Menor de 22",
                                     if_else(Edad<26,"23 a 25",
                                             if_else(Edad<31,"26 a 30",
                                                     if_else(Edad<41,"31 a 40","+ 40")))),"Sin Información"),
         Edad=replace_na(Edad,0),`Tipo Edad`=replace_na(`Tipo Edad`,"Sin Información")) %>% select(-FN,-Ed,-Edad)
Base <- left_join(Base,Rep,by="Matricula") %>% mutate(`Tipo Edad`=replace_na(`Tipo Edad`,"Sin Información"))
Rep <- leer(c("Info General","gsheet"),secc="Campus") %>% select(`Clave Campus`,Campus,`Tipo Campus`)
Base <- left_join(Base,Rep,by="Clave Campus")
diremail("//Tut-095/reportes/ESTADOS")
Rep <- leer(c("reglas_negocios","ESTADOS$")) %>% rename("Matricula"=MATRICULA) %>% filter(!duplicated(Matricula)) %>%
  mutate(Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula))
Base <- left_join(Base,Rep,by="Matricula")
# Rep <- leer(c("alianzamodalida","ESTADOS$"),col_select=c("Matricula"=Matr,Alianza)) %>% filter(!duplicated(Matricula))
# Base <- left_join(Base,Rep,by="Matricula")
Base <- count(Base,Bimestre,Ciclo,`Fecha extracción`,`Tipo Base`,Bloque,`Campus NR`,`Clave Campus`,Campus,Facultad,Homologado,`Tipo Campus`,
              Regla_Negocio,Modalidad,`Tipo Edad`,`Semana extracción`,Estado.Banner,edo.moodle,Semaforo) %>% spread(Semaforo,n) %>%
  mutate_at(18:20,~replace_na(.,0))
Base <- mutate_all(Base,~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
diremail("C:/Users/jsalinba/Documents/Reportes")
escribir(Base,c("IA NR Posgrados 2023.csv","Históricos/NR$"))
source("C:/Users/jsalinba/Documents/Reportes/Scripts/R/Weekly_pos.R",encoding = "LATIN1")

















