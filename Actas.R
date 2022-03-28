library(readr);library(readxl);library(lubridate);library(dplyr);library(tidyr);library(stringr); library(googledrive); library(googlesheets4)
#Variables
c <- list.dirs("D:/Trabajo")
drive_auth("jsalinba@utel.edu.mx"); gs4_auth("jsalinba@utel.edu.mx")
#Funciones
leer_drive <- function(nom,hoja=NULL,...){
  library(googledrive); library(googlesheets4); drive_auth("jsalinba@utel.edu.mx"); gs4_auth("jsalinba@utel.edu.mx")
  y <- drive_find(nom,type = "spreadsheet")
  y <- as.character(y$id)
  x <- range_speedread(y,col_types = cols(.default = "c"),sheet=hoja,...)
  return(x)
}
leer_arch <- function(c,tipo="csv",arch=NULL,pat=NULL,...){
  #Resume la lectura que se usa siempre para los reportes, se pueden usar opciones adicionales de las funciones internas.
  if (tipo=="csv") {
    x <- read_csv(paste(c[grep(pat,c)],paste(arch,tipo,sep = "."),sep = "/"),locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c"),...)
  }else if (tipo=="xlsx"){
    x <- read_excel(paste(c[grep(pat,c)],paste(arch,tipo,sep = "."),sep = "/"),col_types = "text",...)
  }else if (tipo=="txt"){
    x <- read_delim(paste(c[grep(pat,c)],paste(arch,tipo,sep = "."),sep = "/"),locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c"),...)
  }else if (tipo=="gsheet"){
    x <- range_speedread(c[length(c)],col_types = cols(.default = "c"),...)
  }else{x <- "Tipo incorrecto."}
  return(x)
}
impr_arch <- function(c,x,tipo="csv",arch=NULL,pat=NULL,...){
  write.csv(x,paste(c[grep(pat,c)],paste(arch,tipo,sep="."),sep="/"),row.names = F,...)
}
#####----------------------------------------Info Edwin (Aula)----------------------------------------#####
x <- c("Valid Calif 266","Valid Calif 268","Valid Calif 269")
for (i in 1:length(x)) {
  Base <- leer_arch(c,"xlsx",x[i],"auxiliares$",sheet="Valid")# %>% filter(ESTATUS_MATERIA=="RE")
  if(hasName(Base,"MATRICULA")) Base <- unite(Base,Clave_G,MATRICULA,SHORTNAME)
  Base <- Base[c("Clave_G","CAMPUS","NIVEL","AULA","GRUPO","MATERIA","ESTATUS_MATERIA","REGLA","CALIF","ESTATUS_PROGRAMA","FECHA_ESTATUS","FECHA_ACTIVIDAD",
                 "ORIGEN","OBSERVACIONES")]
  if(i==1) Rep <- Base else Rep <- rbind(Rep,Base)
}
Rep <- mutate(Rep,Matrícula=str_extract(Clave_G,"[^[_]]+"),Matrícula=if_else(nchar(Matrícula)<9,paste("0",Matrícula,sep=""),Matrícula))%>%
  unite(`Clave General`,Matrícula,MATERIA,GRUPO,REGLA,remove = F,na.rm = T)
#####----------------------------------------Actas x Regla----------------------------------------#####
Base_Aux <- leer_arch(c,"csv","Estatus_General_Alumno","Escolares$",
                      col_select=c("Matrícula"=MATRICULA,"Programa"=PROGRAMA,"Fecha Estatus"=FECHA_ESTATUS,"Fecha Inicio"=FECHAINICIO,"Clave Estatus"=ESTATUS_CODE))%>%
  mutate_at(c(3,4),~dmy(.)) %>% mutate(`Clave Programa`=substring(Programa,1,10),Programa=substring(Programa,12)) %>% unite(`Clave General`,Matrícula,`Clave Programa`,sep="",na.rm=T)%>%
  arrange(desc(`Fecha Estatus`),desc(`Fecha Inicio`)) %>% filter(!duplicated(`Clave General`)) %>% select(`Clave General`,`Fecha Estatus`,`Fecha Inicio`,`Clave Estatus`)

Base <- leer_arch(c,"csv","Actas X Regla","Escolares$") %>% mutate(MATRICULA=if_else(nchar(MATRICULA)<9,paste("0",MATRICULA,sep = ""),MATRICULA),
                                                                   PERIODO=if_else(nchar(PERIODO)<6,paste("0",PERIODO,sep=""),PERIODO))%>%
  unite(`Clave General`,MATRICULA,CVE_PROGRAMA,remove = F,na.rm = T,sep = "") %>% left_join(Base_Aux,by="Clave General")%>%
  unite(`Clave General`,MATRICULA,CVE_MATERIA,GRUPO,REGLA,remove = F,na.rm = T)
Base_Aux <- leer_drive("Info General","General",range="A2:C")
Base <- left_join(Base,Base_Aux,by="Clave Estatus")
#-------------
Base_Aux <- select(Base,`Clave General`,MATRICULA)
Rep2 <- left_join(Rep,Base_Aux,by="Clave General") %>% rename("Actas x Regla"=MATRICULA) %>% mutate(`Actas x Regla`=if_else(is.na(`Actas x Regla`),"No","Si")) %>% unique()

Rep_Aux <- select(Rep,`Clave General`,Matrícula)
Base2 <- left_join(Base,Rep_Aux,by="Clave General")  %>% rename("Aula"=Matrícula) %>% mutate(Aula=if_else(is.na(Aula),"No","Si")) %>% unique()

remove(Base_Aux,Rep_Aux)
#Ordenando
x <- c("Clave General","PIDM","MATRICULA","NOMBRE_ALUMNO","CAMPUS","NIVEL","CVE_PROGRAMA","PROGRAMA","Estatus","Tipo Estatus","PERIODO","GRUPO","CVE_MATERIA","MATERIA","CALIFICACION",
       "CALIF_LETRA","TIPO_EVALUACION","CLAVE_PROF","NOMBRE_PROF","EMAIL","FOLIO","REGLA","FECHA_INICIO","FECHA_FIN","Fecha Estatus","Fecha Inicio","Aula")
Base2 <- Base2[x] %>% mutate_all(~as.character(.)) %>% mutate_all(~replace_na(.,"_"))

x <- c("Clave General","Clave_G","Matrícula","CAMPUS","NIVEL","ESTATUS_PROGRAMA","AULA","GRUPO","MATERIA","ESTATUS_MATERIA","CALIF","ORIGEN","REGLA","FECHA_ESTATUS",
       "FECHA_ACTIVIDAD","OBSERVACIONES","Actas x Regla")
Rep2 <- Rep2[x] %>% mutate_all(~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
#Imprimiendo
impr_arch(c,Rep2,"csv","Aula vs Actas x Regla","auxiliares$")
#Contando Profesores.
Base <- Base2; Rep <- Rep2#; remove(Base2,Rep2)

Base <- mutate(Base,GRUPO=if_else(nchar(GRUPO)==1,paste("Grupo_0",GRUPO,sep = ""),paste("Grupo_",GRUPO,sep = "")))
cont <- count(Base,CLAVE_PROF,CVE_MATERIA,GRUPO,name = "Alumnos x Mat y Gru") %>% unite(PROF_MAT_GRU,CLAVE_PROF,CVE_MATERIA,GRUPO,sep = "")
Base <- unite(Base,PROF_MAT_GRU,CLAVE_PROF,CVE_MATERIA,GRUPO,remove = F,na.rm = T,sep = "") %>% left_join(cont,by="PROF_MAT_GRU")%>%
  unite(PROF_MAT_GRU,PROF_MAT_GRU,REGLA,sep="",remove = F)
# cont <- count(Base,CLAVE_PROF,GRUPO,name = "Alumnos X Grupo") %>% unite(PROF_GRUPO,CLAVE_PROF,GRUPO)
# Base <- unite(Base,PROF_GRUPO,CLAVE_PROF,GRUPO,remove = F,na.rm = T) %>% left_join(cont,by="PROF_GRUPO") %>% select(-PROF_GRUPO)
# cont <- count(Base,CLAVE_PROF,name = "Alumnos Totales")
# Base <- left_join(Base,cont,by="CLAVE_PROF")
#Imprimiendo
Rep <- leer_arch(c,"xlsx","Alumnos Totales Actas","auxiliares$",sheet="Regla 268") %>% select(Matrícula,`Clave asignatura`,"Alumnos Aula"=Final,regla)%>%
  unite(PROF_MAT_GRU,Matrícula,`Clave asignatura`,regla,sep="")
Base <- left_join(Base,Rep,by="PROF_MAT_GRU")
########
Rep <- leer_arch(c,"xlsx","Alumnos Totales Actas","auxiliares$",sheet="Regla 266") %>% select(Matrícula,`Clave asignatura`,"Alumnos Aula2"=Final,`Regla 266`)%>%
  mutate(`Regla 266`="266") %>% unite(PROF_MAT_GRU,Matrícula,`Clave asignatura`,`Regla 266`,sep="")
Base <- left_join(Base,Rep,by="PROF_MAT_GRU")
########






Base <- mutate(Base,`Alumnos Aula`=if_else(is.na(`Alumnos Aula`),`Alumnos Aula2`,`Alumnos Aula`)) %>% select(-`Alumnos Aula2`)
Base <- mutate(Base,Coincidencia=if_else(`Alumnos x Mat y Gru`==`Alumnos Aula`,"Si","No"),
               Diferencia=as.integer(`Alumnos x Mat y Gru`)-as.integer(`Alumnos Aula`))
Base <- mutate_all(Base,~as.character(.))

impr_arch(c,Base,"csv","Actas x Regla vs Aula","auxiliares$")
#Reporte Edwin final








