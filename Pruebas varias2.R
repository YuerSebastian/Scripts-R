library(vroom)
library(tidyr)
library(dplyr)
library(lubridate)
#####-----Inicio-----#####
Base <- vroom("D:/Trabajo/Bases/bloquesegmae.csv",",",locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c")) %>%
  mutate(matricula = if_else(nchar(matricula) == 8,paste0("0",matricula),matricula))

Rep <- vroom("D:/Trabajo/Bases/Originales/SIR/Estatus General Alumnos.csv",",",locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c"),
             col_select = c("matricula"=MATRICULA,"E"=ESTATUS,"FE"=FECHA_ESTATUS,"FI"=FECHAINICIO)) %>% 
  mutate_at(c("FE","FI"),~dmy(.)) %>% arrange(desc(FE),desc(FI)) %>% filter(!duplicated(matricula)) %>%
  mutate(E=if_else(E %in% c("MATRICULADO","ADMITIDO"),"Activo","_")) %>% select(matricula,E) %>% filter(E!="_")
Base <- left_join(Base,Rep,"matricula")
Base <- mutate(Base,E = if_else(grepl("Q",B.ingreso),"_",E))

Rep <- filter(Base,E != "Activo" | is.na(E))
Base <- filter(Base,E == "Activo")


Base <- mutate(Base,bim = paste0("B",as.integer(substring(B.ingreso,2,2)) + 1),
               Tipo.alumno = gsub("NI","Regular",Tipo.alumno),
               bim = if_else(bim=="B10","B9",bim))
Base <- mutate(Base,dias = paste0("(",as.integer(substring(bim,2)) * 60," días)")) %>% unite(bim, bim, dias, sep = " ")


Base <- rbind(Base,Base)

Base <- bind_rows(Base,Rep)



vroom_write(Base,"D:/Trabajo/prueba.csv",",","\r\n",bom = T)


data.table::fwrite(Base,"D:/Trabajo/prueba.csv",bom = T)





#D:\Trabajo\Bases\Originales\SIR


























