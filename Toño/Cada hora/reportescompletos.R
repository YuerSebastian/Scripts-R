library(tidyverse)




##### Licenciaturas
shell("D://Users/jvelazhe/Desktop/Development/Python_BI/reportecompleto.py")
shell("D://Users/jvelazhe/Desktop/Development/Python_BI/modalidadevaluacion.py")


modalidad <- list.files ("G:/Mi unidad/Reports/aulascompleto/modevaluacion/", pattern = "*.csv", full.names = TRUE)

for (i in 1:length(modalidad)) {
  
  csv <-read.csv(modalidad[i],fileEncoding = "LATIN1") %>% 
    mutate(matricula= as.numeric(matricula)) %>%
    filter(!is.na(matricula)) %>% 
    unite("Con",c(matricula,clave),sep=" ",remove = T)
  
  if(i==1) modeva <- csv else modeva <- rbind(modeva,csv)
  
}

completos <- list.files ("G:/Mi unidad/Reports/aulascompleto/reportecompleto/", pattern = "*.csv", full.names = TRUE)

for (i in 1:length(completos)) {
  
  csv <-read.csv(completos[i],fileEncoding = "LATIN1") %>% 
    mutate(calificacion= as.numeric(calificacion),matricula= as.numeric(matricula),aula= if_else(i < 10,paste("Aula","0",i,sep=""),paste("Aula",i,sep=""))) %>%
    filter(!grepl("Grupo_O|Grupo_S",grupo),!is.na(matricula)) %>% 
    unite("Nombre.Alumno",c(Nombre,Apellidos),sep=" ",remove = F) %>% 
    mutate(calificacion = replace_na(calificacion,0))
    
            
if(i==1) reporte_completo <- csv else reporte_completo <- rbind(reporte_completo,csv)

 }

reporte_completos <- reporte_completo %>% unite("Con",c(matricula,clave),sep=" ",remove = F) %>% 
  left_join(modeva,by = "Con") %>% mutate(modalidad = replace_na(modalidad,"Sin Modalidad")) %>% 
  select(status_plataforma,status,institucion,id,matricula,ciudad,Nombre,Apellidos,Nombre.Alumno,correo,materia_id,clave,
       asignatura,grupo,modalidad,calificacion,ultimo_acceso_m,ultimo_acceso_auvi,aula,fecha_inicio,fecha_fin,rol) %>% 
  filter(!duplicated(.))




write.csv(reporte_completos,file = "G:/Mi unidad/Reports/reporte_completo_Lic.csv",row.names=F,fileEncoding = "LATIN1")
