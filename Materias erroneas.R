library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(lubridate)
#Variables principales.
C <- list.dirs("D:/Cosa/Alumnos/Materias erroneas")
#Funciones
SepCol <- function(x,cols=c("x","y"),por="cruce"){
  for (i in 1:length(cols)) {
    x <- filter(Rep,Rep[[cols[i]]]!="_")
    if(i!=1) x <- select(x,c(por,cols[i])) else x <- x[c(1:(length(x)-(length(cols)-1)))]
    if(exists("y")) y <- left_join(y,x,by=por) else y <- x
  }
  return(y)
}
#####--------------------------------------Materias inscritas--------------------------------------#####
Base <- read_csv(paste(C[2],"Materias_Inscritas_por_parte_de_periodo.csv",sep="/"),locale=locale(encoding="LATIN1"))%>%
  unite(MATPROG,MATRICULA,CODE_PROG,MATERIA,remove=F) %>% mutate(FECHA_INICIO=dmy(FECHA_INICIO)) %>% arrange(desc(FECHA_INICIO)) %>% filter(duplicated(MATPROG)==F)%>%
  unite(MATPROG,MATRICULA,CODE_PROG,remove=F) %>% mutate_all(~as.character(.))
Rep <- read_excel(paste(C[3],"Info_general.xlsx",sep="/"),sheet="Campus") %>% select(-Fecha_campus)
Base <- left_join(Base,Rep,by="CAMPUS") %>% filter(Tip_campus!="OPM")
#####--------------------------------------Areas por alumno--------------------------------------#####
Rep <- read_csv(paste(C[2],"Areas_ProfesionalesX_Alumno.csv",sep="/"),locale=locale(encoding="LATIN1"))%>%
  unite(MATPROG,MATRICULA,PROGRAMA,AREA,remove=F) %>% filter(duplicated(MATPROG)==F) %>% unite(MATPROG,MATRICULA,PROGRAMA,remove=F) %>% arrange(desc(AREA))%>%
  group_by(MATPROG) %>% mutate(cont=row_number()) %>% as.data.frame()%>%
  mutate(MAJOR=if_else(cont==1,AREA,"_"),Salida_1=if_else(cont==2,AREA,"_"),Salida_2=if_else(cont==3,AREA,"_")) %>% select(-cont)
Rep <- SepCol(Rep,c("MAJOR","Salida_1","Salida_2"),"MATPROG") %>% select(MATPROG,MAJOR,Salida_1,Salida_2)
Base <- left_join(Base,Rep,by="MATPROG") %>% mutate_all(~replace_na(.,"_"))
#####--------------------------------------Materias CAPP--------------------------------------#####
Rep <- read_csv(paste(C[2],"Materias_CAPP.csv",sep="/"),locale=locale(encoding="LATIN1")) %>% mutate_all(~replace_na(.,"_"))%>%
  reshape::rename(c(SMRIEMJ_MAJR_CODE="MAJOR",SMRIECC_MAJR_CODE_CONC="CONCENTRACION"))%>%
  unite(MATPROG,PROGRAMA,CAMPUS,PERIODO,MAJOR,MATERIA,sep="",remove=F)%>%
  mutate(MATPROG=gsub("_","",MATPROG)) %>% filter(duplicated(MATPROG)==F) %>% unite(MATPROG,PROGRAMA,CAMPUS,PERIODO,MATERIA,sep="",remove=F)%>%
  select(MATPROG,MAJOR) %>% group_by(MATPROG) %>% mutate(cont=row_number()) %>% as.data.frame()%>%
  mutate(MAJOR1=if_else(cont==1,MAJOR,"_"),MAJOR2=if_else(cont==2,MAJOR,"_"),MAJOR3=if_else(cont==3,MAJOR,"_"),MAJOR4=if_else(cont==4,MAJOR,"_"))
Rep <- SepCol(Rep,c("MAJOR1","MAJOR2","MAJOR3","MAJOR4"),"MATPROG") %>% mutate_all(~replace_na(.,"_")) %>% select(MATPROG,MAJOR1,MAJOR2,MAJOR3,MAJOR4)

Base <- unite(Base,MATPROG,CODE_PROG,CAMPUS,CATALOGO,MATERIA,sep="",remove=F)
Base <- left_join(Base,Rep,by="MATPROG")
Base <- mutate(Base,Error=if_else(!is.na(MAJOR1),
                                  if_else(MAJOR==MAJOR1 | MAJOR==MAJOR2 | MAJOR==MAJOR3 | MAJOR==MAJOR4,"Correcto","Incorrecto"),"Tronco"))%>%
  mutate_all(~replace_na(.,"_"))
#####--------------------------------------Imprimiendo--------------------------------------#####
Rep <- read_excel(paste(C[3],"Info_general.xlsx",sep="/"),sheet="Orden")
Rep <- Rep$Incluir
Base <- Base[Rep]
write.csv(Base,paste(C[1],"Materias erroneas.csv",sep="/"),row.names=F)
















