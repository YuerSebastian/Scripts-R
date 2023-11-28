sapply(c("dplyr","tidyr","lubridate","readxl","vroom","data.table","googledrive","googlesheets4","stringr","reshape2"),require,character.only=T)
#####-----Master Maestría a Maestría-----#####
# Base <- fread("C:/Users/jsalinba.SCALA/Documents/Reportes/Bases/Originales/SIR/Estatus General Alumnos.csv",encoding = "Latin-1",colClasses = "character")
Base <- vroom("C:/Users/jsalinba.SCALA/Documents/Reportes/Bases/Originales/SIR/Estatus General Alumnos.csv",",",locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c"),
              col_select = c("Matricula"=MATRICULA,"Nombre"=ESTUDIANTE,"Correo"=CORREO,"Nivel"=NIVEL,"Estatus"=ESTATUS_CODE,"Fecha Estatus"=FECHA_ESTATUS,"Fecha Inicio"=FECHAINICIO)) %>%
  arrange(desc(dmy(`Fecha Inicio`))) %>% filter(Nivel %in% c("MS","MA"), substring(Matricula,1,2) %in% c("01","02"), !duplicated(Matricula)) %>%
  mutate(mat = substring(Matricula,3),Correo = gsub("@.*","",Correo)) %>%
  add_count(mat) %>% filter(n==2) %>% select(-n) %>% group_by(mat) %>% mutate(cont = row_number(),cont=if_else(cont==1,"Actual","Anterior")) %>%
  ungroup() %>% pivot_wider(names_from = cont,values_from = c(1:7)) %>% mutate(Valid = if_else(Nombre_Anterior == Nombre_Actual | Correo_Anterior == Correo_Actual,"Si","No"),
                                                                               Valid = if_else(Valid == "No",
                                                                                               if_else(Nivel_Actual == "MA","Si (validar)","No (validar)"),Valid))
#####-----Links i6-----#####
drive_auth("jsalinba@utel.edu.mx")
drive_download("Links i6.xlsx","C:/Users/jsalinba.SCALA/Documents/Reportes/Bases/Drive/Links i6.xlsx",overwrite = T)



Base <- read_excel("C:/Users/jsalinba.SCALA/Documents/Reportes/Bases/Drive/Links i6.xlsx") %>% filter(!is.na(`Matricula SIU`)) %>% arrange(desc(`Fecha Creacion`)) %>%
  filter(!duplicated(`Matricula SIU`))





fwrite(Base,"C:/Users/jsalinba.SCALA/Documents/Reportes/Bases/Valid MM.csv",bom = T)



