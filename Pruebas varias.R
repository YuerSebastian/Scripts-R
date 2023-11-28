sapply(c("dplyr","tidyr","lubridate","readxl","vroom","data.table","googledrive","googlesheets4","stringr"),require,character.only=T)
#####-----Inicio-----#####
Rep <- read_excel("C:/Users/jsalinba.SCALA/Documents/Reportes/Bases/Históricos/Histórico Asignación Posgrados.xlsx",col_types = "text") %>%
  select(Matricula,`NI-Q`,Modalidad,`Nivel de Riesgo`,Rango_Adeudo,Tutor,`F. ASIGNACIÓN`) %>%
  mutate(`F. ASIGNACIÓN`=if_else(!grepl("[A-Z\\-]",`F. ASIGNACIÓN`),as.character(as.Date(as.numeric(`F. ASIGNACIÓN`),"1899-12-30")),`F. ASIGNACIÓN`))
Base <- read_excel("C:/Users/jsalinba.SCALA/Documents/Reportes/Bases/Asignación Posgrados.xlsx",sheet="Base",col_types = "text") %>%
  select(Matricula,`NI-Q`,Modalidad,`Nivel de Riesgo`,Rango_Adeudo,Tutor,`F. ASIGNACIÓN`) %>%
  mutate(`F. ASIGNACIÓN`=if_else(!grepl("[A-Z\\-]",`F. ASIGNACIÓN`),as.character(as.Date(as.numeric(`F. ASIGNACIÓN`),"1899-12-30")),`F. ASIGNACIÓN`)) %>%
  rbind(Rep) %>% add_count(Matricula,name = "B") %>% add_count(Matricula,name = "F") %>% add_count(Matricula,name = "M") %>%
  filter(B==2) %>% group_by(Matricula) %>% arrange(desc(`F. ASIGNACIÓN`)) %>%
  mutate(B=row_number(),`F`=row_number(),M=row_number(),B=paste0("Bi ",B),`F`=paste0("Fe ",`F`),M=paste0("Mo ",M)) %>%
  ungroup() %>% spread(B,`NI-Q`) %>% spread(`F`,`F. ASIGNACIÓN`) %>% spread(M,Modalidad)




Base <- vroom("C:/Users/jsalinba.SCALA/Documents/Reportes/Bases/Históricos/Sabana/Sabana Posgrados Ene-Feb 21.csv",",",
              locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c"))



Base <- vroom("C:/Users/jsalinba.SCALA/Documents/Reportes/Bases/Originales/SIR/Estatus General Alumnos.csv",",",
             locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c"))

Rep <- Base
Rep <- mutate(Rep,cosa=nchar(ESTUDIANTE))
for (i in 1:length(Base)) {
  Rep[i] <- max(nchar(Rep[[i]]),na.rm = T)
}

Rep <- filter(Rep,!duplicated(MATRICULA))
Rep <- as.data.frame(t(Rep))
Rep <- mutate(Rep,col=rownames(Rep))

fwrite(Rep,"C:/Users/jsalinba.SCALA/Documents/Reportes/Bases/cols.csv",bom = T)


#Base <- fread("//Tut-095/reportes/ESTADOS/bloquesegmae.csv",colClasses = "character",encoding = "Latin-1")






















