sapply(c("MultiLEDS","dplyr","tidyr","lubridate","RMySQL"),library,character.only=T)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#####------Inicio------#####
#drive_sd("des",c("Reports/encuestaevaulaciondocentemaeenefeb23.csv","Drive$"))
drive_sd("des",c("Reports/encuestaevaulaciondocentelicenefeb23.csv","Drive$"))
#Licenciatura
Base <- leer(c("encuestaevaulaciondocentelicenefeb23","Drive$"),
             col_select=c(fullname,name...4,name...6,userid...7,aula,responsesummary)) %>% codificacion() %>%
  unite(ID,fullname,name...4,userid...7,remove = F) %>% group_by(ID) %>% mutate(responsesummary=as.integer(responsesummary)) %>%
  mutate(Total=sum(responsesummary)) %>% ungroup() %>% filter(!duplicated(ID)) %>% select(-responsesummary)
escribir(Base,c("Evaluación docenteL Mar-Abr 23.csv","Trabajo$"))
#Maestría
# Base <- leer(c("encuestaevaulaciondocentemaeenefeb23","Drive$"),
#              col_select=c(fullname,name...4,name...6,userid...7,aula,responsesummary)) %>% codificacion() %>%
#   unite(ID,fullname,name...4,userid...7,remove = F) %>% group_by(ID) %>% mutate(responsesummary=as.integer(responsesummary)) %>%
#   mutate(Total=sum(responsesummary)) %>% ungroup() %>% filter(!duplicated(ID)) %>% select(-responsesummary)
# escribir(Base,c("Evaluación docenteM Ene-Feb23.csv","Trabajo$"))



