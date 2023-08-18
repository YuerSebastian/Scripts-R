library(MultiLEDS); library(googledrive); library(dplyr); library(tidyr);library(lubridate)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")

Rep <- leer(c("NR_POSGRADOS_2022-08-27","Valid Mae$"),col_select=c("Matrícula"=Matricula,edo.moodle,Ciclo,"Materias totales NR"=Total.general)) %>% filter(edo.moodle=="Activo")%>%
  select(Matrícula,`Materias totales NR`)
#####-------------Reporte completo Mae-------------#####
Base <- leer(c("reporte_completomaesepoct22","Valid Mae$"),col_select=c("Matrícula"=matricula,status,"Nombre Materia"=asignatura,clave)) %>% filter(status=="Activo")%>%
  separate(clave,into=c("1","2","Inicio Plus","Clave Materia")) %>% select(-"1",-"2",-status) %>% left_join(Rep,by="Matrícula")%>%
  mutate(`Tipo Materia`=if_else(grepl("Sesión Ejecutiva",`Nombre Materia`),"Taller","Curricular"),`Materias totales NR`=replace_na(`Materias totales NR`,"0"),
         Matrícula=if_else(nchar(Matrícula)<9,paste("0",Matrícula,sep=""),Matrícula)) %>% group_by(Matrícula,`Tipo Materia`)%>%
  add_count(name = "Materias Totales R") %>% ungroup() %>% unite(MATMATER,Matrícula,`Clave Materia`,sep="",remove = F)
#####-------------Por egresar-------------#####
Rep <- leer(c("Alumnos_x_Egresar 329","Valid Mae$"),col_select=c("Matrícula"=MATRICULA)) %>% mutate(`Por Egresar`="Por Egresar")
Base <- left_join(Base,Rep,by="Matrícula") %>% mutate(`Por Egresar`=replace_na(`Por Egresar`,"_"))
#####-------------Pronóstico-------------#####
Rep <- leer(c("Pronostico_asignacion_materias 329","Valid Mae$")) %>% mutate(`Inicio Plus`=paste(substring(FECHA_INICIO,9,10),substring(FECHA_INICIO,6,7),sep=""))%>%
  filter(ESTATUS_MODL=="S") %>% select("Matrícula"=MATRICULA,"Clave Materia"=MATERIA_LEGAL,`Inicio Plus`) %>% add_count(Matrícula,name = "Materias Totales P")%>%
  unite(MATMATER,Matrícula,`Clave Materia`,sep="",remove = F)
#####-------------Cruces-------------#####
noms <- c("MATMATER",names(Base[!names(Base) %in% names(Rep)]))
Rep <- left_join(Rep,Base[noms],by="MATMATER") %>% mutate(Encontrado=if_else(is.na(`Materias Totales R`),"No","Si"))
noms <- c("MATMATER",names(Rep[!names(Rep) %in% names(Base)]))
Base <- left_join(Base,Rep[noms],by="MATMATER") %>% mutate(Encontrado=if_else(is.na(`Materias Totales P`),"No","Si"))

#####-------------Imprimiendo-------------#####
noms <- c("MATMATER","Matrícula","Clave Materia","Nombre Materia","Tipo Materia","Por Egresar",
          "Materias totales NR","Materias Totales R","Materias Totales P","Inicio Plus","Encontrado")
Base <- Base[noms]; Rep <- Rep[noms]
escribir(Base,c("Valid Mae Reporte.csv","Principales$"))
escribir(Rep,c("Valid Mae Pronóstico.csv","Principales$"))










