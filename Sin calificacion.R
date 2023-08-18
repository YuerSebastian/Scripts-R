library(MultiLEDS); library(dplyr); library(tidyr); library(lubridate); library(stringr)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#Inicio
Rep <- leer(c("1OQENajkU5Z4L7MS7FNhrvrMBS1iT-EOc5oiON9FXcns","gsheet.ID"),sheet="General",secc="Campus") %>% select(-`Fecha Campus`)
Mat <- leer(c("Materias_Inscritas_PPDP","SIR$")) %>% mutate_all(~replace_na(.,"_")) %>% rename("Clave Campus"=CAMPUS)%>%
  left_join(Rep,by="Clave Campus") %>% filter(`Tipo Campus`!="OPM") %>% unite(MATPROGMATE,MATRICULA,CODE_PROG,MATERIA,remove=F) %>% mutate(FECHA_INICIO=dmy(FECHA_INICIO))%>%
  arrange(desc(FECHA_INICIO)) %>% filter(!duplicated(MATPROGMATE))
#Materias
Mat <- mutate(Mat,Tip_eval_materia=if_else(grepl("SESO",MATERIA_PADRE),"Servicio Social",
                                           if_else(MATERIA_LEGAL=="M1HB401","Introdicción al aula",
                                                   if_else(str_sub(PERIODO,-2,-2)=="8","Nivelación","Ordinario"))),#Tip materia
              Tip_calificacion=if_else(FECHA_INICIO>="2023-05-01","En curso",
                                       if_else(ESTATUS_MAT!="RE","Baja",
                                               if_else(CALIFICACION!="0","Con calificación","Sin calificación"))))%>%#Tip calificación
  mutate_all(~as.character(.)) %>% mutate_all(~replace(.,is.na(.),"_"))
#Ordenando e imprimiendo
Rep <- leer(c("1OQENajkU5Z4L7MS7FNhrvrMBS1iT-EOc5oiON9FXcns","gsheet.ID"),sheet="Orden Base",secc="Alumnos sin Calificación")
Rep <- Rep$Incluir
Mat <- Mat[Rep]
escribir(Mat,c("Alumnos sin calificación.csv","Principales$"))









