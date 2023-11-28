sapply(c("MultiLEDS","dplyr","tidyr","lubridate","vroom","stringi","data.table","googledrive"), library,character.only=T)
diremail("C:/Users/jsalinba/Documents/Reportes","jsalinba@utel.edu.mx")
#####----------Descargas----------#####
drive_sd("des",c("front/asignacion_opm.csv","auxiliares$"))
# drive_sd("des",c("Links i6/Links i6.xlsx","i6$"))
#####----------Escribiendo separado----------#####
Base <- leer(c("Asignación Posgrados","Bases$"),sheet="Base") %>%
  mutate_at(grep("Acc|^Fecha|Inicio_Curso|F\\.",names(.)),~if_else(!grepl("[A-Z\\-]",.),as.character(as.Date(as.numeric(.),"1899-12-30")),.))
escribir(filter(Base,Base=="Online") %>% select(-Base),c("asignacion_posgrados.csv","auxiliares$"))
escribir(filter(Base,Base=="Ejecutiva") %>% select(-Base),c("asignacionejec_senior.csv","auxiliares$"))
#####----------Unificando----------#####
Rep <- leer(c("asignacion_opm","auxiliares$"))
if(hasName(Rep,"...65")) Rep <- select(Rep,-...65)
Rep <- rename(Rep,"NR Autogestionable"=Incidencias)
Base <- select(Base,-Base) %>% rbind(Rep) %>% mutate_all(~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
escribir(Base,c("asignacionBanner.csv","Principales$"))
#####----------Subiendo----------#####
drive_put("C:/Users/jsalinba/Documents/Reportes/Bases/Adicionales y auxiliares/asignacion_posgrados.csv","front/")
drive_put("C:/Users/jsalinba/Documents/Reportes/Bases/Adicionales y auxiliares/asignacionejec_senior.csv","front/")
drive_put("C:/Users/jsalinba/Documents/Reportes/Bases/Principales/asignacionBanner.csv","front/")
















