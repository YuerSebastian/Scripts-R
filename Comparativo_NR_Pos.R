sapply(c("MultiLEDS","dplyr","tidyr","lubridate"),library,character.only=T)
diremail("C:/Users/jsalinba/Documents/Reportes","jsalinba@utel.edu.mx")
#####-----------Descargas NR drive-----------#####
FI <- ymd("2023-11-27")
while (FI <= Sys.Date()){
  drive_sd("des",c(paste("Niveles/NR_POSGRADOS_",FI,".csv",sep=""),"NR/Nov-Dic 23$"))
  print(paste("Niveles/NR_POSGRADOS_",FI,".csv",sep=""))
  FI <- FI + days(1)
}
#####-----------Unificando-----------#####
#Actual
FI <- ymd("2023-10-23")
Base <- unificar(c("NR/Nov-Dic 23$","NR_POSGRADOS_"),iden="nom") %>%
  mutate(Semana = "_",Bimestre="Nov-Dic 23",iden=substring(iden,14,23),iden=ymd(iden),`Tipo Base`=if_else(grepl("cierre",iden,T),"Cierre","Normal")) %>%
  rename("Fecha extracción"=iden,"Campus NR"=Campus)
for (i in 0:12) {
  Base <- mutate(Base,Semana=if_else(`Fecha extracción`>=FI & `Fecha extracción`<=(FI + days(6)),paste("S",i,sep=""),Semana))
  FI <- FI + days(7)
}
#Otros bimestres
Rep <- unificar(c("Históricos/NR$","Ene-Feb 21|Mar-Abr 22|Nov-Dic 22")) %>% rename("Campus NR"=Campus)
Base <- rbind(Base,Rep)
#####-----------Info general-----------#####
Rep <- leer(c("Estatus General Alumnos","SIR$"),col_select=c("Matricula"=MATRICULA,"Clave Campus"=CAMPUS)) %>% filter(!duplicated(Matricula))
Base <- mutate(Base,Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula)) %>% left_join(Rep,by="Matricula")

Rep <- leer(c("Info General","gsheet"),secc="Campus") %>% select(`Clave Campus`,Campus,`Tipo Campus`)
Base <- left_join(Base,Rep,by="Clave Campus")
Base <- mutate(Base,`Tipo Campus`=gsub("UTEL ","",`Tipo Campus`))

diremail("//Tut-095/reportes/ESTADOS")
Rep <- leer(c("reglas_negocios","ESTADOS$")) %>% filter(!duplicated(MATRICULA)) %>% rename("Matricula"=MATRICULA) %>%
  mutate(Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula))
Base <- left_join(Base,Rep,by="Matricula")
Rep <- leer(c("alianzamodalida","ESTADOS$")) %>% select(-Modalidad) %>% filter(!duplicated(Matr)) %>%
  mutate(Matr=if_else(nchar(Matr)<19,paste("0",Matr,sep=""),Matr))
Base <- unite(Base,Matr,Matricula,Clave.Programa,sep="",remove=F) %>% left_join(Rep,by="Matr") %>%
  select(-Matr) %>% filter(!grepl("prueba|test",Nombre,ignore.case = T))
#Imprimiendo
diremail("C:/Users/jsalinba/Documents/Reportes")
escribir(Base,c("Comparativo NR Posgrados.csv","Principales$"))










