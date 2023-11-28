library(tidyverse)
library(readxl)



##### Licenciaturas
shell("D://Users/jvelazhe/Desktop/Development/Python_BI/reportecompletoposgrados.py")



completos <- list.files ("G:/Mi unidad/Reports/aulascompleto/reportecompletoposgra/", pattern = "*.csv", full.names = TRUE)

for (i in 1:length(completos)) {
  
  csv <-read.csv(completos[i],fileEncoding = "LATIN1") %>% 
    mutate(calificacion= as.numeric(calificacion),matricula= as.numeric(matricula),aula= (paste("Aula",10+i,sep=""))) %>%
    mutate(calificacion = replace_na(calificacion,0))
    
            
if(i==1) Posgrados <- csv else Posgrados <- rbind(Posgrados,csv)

 }

catalogoalianza <- read_xlsx("G:/Mi unidad/Reports/CatalgoAlianzas.xlsx")
nivel <-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/nivel_maestrias.csv", header = TRUE,sep = ",",encoding="latin1")


Posgra <- Posgrados %>% mutate(ciclo = sub("^(.*?)_.*$", "\\1", clave)) %>% 
  left_join(nivel,by="ciclo") %>% mutate(Nivel=replace_na(nivel,"Sin Sin_Definir")) %>% 
  rename(`Fecha enrolado`= Fecha.enrolado,`Fecha mdf enrol`= Fecha.mdf.enrol,`fecha add grupo`=fecha.add.grupo) %>% 
  select(status_plataforma,status,institucion,nivel,matricula,Nombre,Apellidos,correo,materia_id,clave,grupo,
                           calificacion,rol,asignatura,`Fecha enrolado`,`Fecha mdf enrol`,ultimo_acceso_m,`fecha add grupo`,ultimo_acceso_auvi,aula) %>% 
  filter(!grepl("Grupo_O|Grupo_S",grupo))%>% 
  mutate(claves =sub(".+_", "",clave)) %>% left_join(catalogoalianza,by="claves") %>% 
  mutate(Inst = replace_na(Inst,"Curricular")) %>% select(-claves) %>% filter(!duplicated(.),!is.na(matricula))


write.csv(Posgra, file = "G:/Mi unidad/Reports/reporte_completo_Mae.csv", row.names = F,fileEncoding = "latin1")




