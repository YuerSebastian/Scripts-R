library(tidyverse)
library(readxl)
library(googlesheets4)
library(googledrive)
library(readr)

gs4_auth("jvelazhe@utel.edu.mx")

fconsycom <- read_excel("D:/Users/jvelazhe/Desktop/Licenciaturas/fechasforocolabora.xlsx",sheet = "fechasapertura") %>%
  mutate(fechamaxima = ymd_hms(fechamaxima)) %>% unite("IDZ",c(iniciop,Semana),sep = "_") %>% select(-fechacierre)



SemanasRanngos <- read_excel("D:/Users/eescobev/Documents/REPORTES/ESTADOS/SemanasRanngos.xlsx",sheet = "foros") %>% select(-5:-10,-`inicio plus`,-Fecha)



linksaula <- read_excel("D:/Users/jvelazhe/Desktop/Licenciaturas/fechasforospresentacion.xlsx", sheet = "linksaula")

nomenclatura <- read_excel("D:/Users/jvelazhe/Desktop/Licenciaturas/fechasforocolabora.xlsx",sheet = "Nomenclatura") %>% 
  unite("IDZ",c(iniciop,Semana),sep = "_") 




foros <- read.csv("G:/Mi unidad/Reports/Foros_Lic.csv", header = TRUE, sep = ",",encoding = "UTF-8")


colabora1 <- foros %>% filter(grepl("Foro 1",foro)) %>% 
  mutate(rating = replace_na(rating,-1)) %>% 
  mutate(iniciop = gsub("^.*_.*_(\\d{4}).*$", "\\1", clave),fecha_evio = ymd_hms(fecha_evio),fechapost=date(fecha_evio),
         sems= ifelse(grepl("Cierre|cierre|CIERRE",asunto),"2",
           ifelse(grepl("Foro 1|foro1|FORO 1|foro 1",asunto),"1","2"))) %>% 
  filter(!is.na(fechapost)) 

colabora2 <- foros %>% filter(grepl("Foro 2",foro)) %>% 
  mutate(rating = replace_na(rating,-1)) %>% 
  mutate(iniciop = gsub("^.*_.*_(\\d{4}).*$", "\\1", clave),fecha_evio = ymd_hms(fecha_evio),fechapost=date(fecha_evio),
         sems= ifelse(grepl("Cierre|cierre|CIERRE",asunto),"4",
                      ifelse(grepl("Foro 2|foro2|FORO 2|foro 2",asunto),"3","4"))) %>% 
  filter(!is.na(fechapost))

colabora3 <- foros %>% filter(grepl("Foro 3",foro)) %>% 
  mutate(rating = replace_na(rating,-1)) %>% 
  mutate(iniciop = gsub("^.*_.*_(\\d{4}).*$", "\\1", clave),fecha_evio = ymd_hms(fecha_evio),fechapost=date(fecha_evio),
         sems= ifelse(grepl("Cierre|cierre|CIERRE",asunto),"6",
                      ifelse(grepl("Foro 3|foro3|FORO 3|foro 3",asunto),"5","6"))) %>% 
  filter(!is.na(fechapost))

colabora4 <- foros %>% filter(grepl("Foro 4",foro)) %>% 
  mutate(rating = replace_na(rating,-1)) %>% 
  mutate(iniciop = gsub("^.*_.*_(\\d{4}).*$", "\\1", clave),fecha_evio = ymd_hms(fecha_evio),fechapost=date(fecha_evio),
         sems= ifelse(grepl("Cierre|cierre|CIERRE",asunto),"8",
                      ifelse(grepl("Foro 4|foro4|FORO 4|foro 4",asunto),"7","8"))) %>% 
  filter(!is.na(fechapost))


colabora <- rbind(colabora1,colabora2,colabora3,colabora4)




                           
respuestas <- colabora %>%  filter(rating == -1) %>% 
  filter(!is.na(matricula_participante
                )) %>%
  mutate(tipomat = substr(matricula_participante, 1, 2),matdocente = if_else(tipomat == 19,"Doce","Stud"))  %>% 
  filter(matdocente == "Stud" )%>%  unite("ID",c(clave,grupo,sems),sep = "_") %>% 
  count_(c('ID')) %>% rename(Rating_Pendiente=n)
  

colaborativo <- colabora %>% filter(!grepl("Re:",asunto)) %>%   
  mutate(iniciop = gsub("^.*_.*_(\\d{4}).*$", "\\1", clave),fecha_evio = ymd_hms(fecha_evio),Semana  = sems ) %>%
  unite("IDZ",c(iniciop,Semana),sep = "_") %>% left_join(nomenclatura,by = "IDZ") %>% 
  mutate(Apertura = if_else(asunto == regexp,"Apertura_Correcta","Apertura_Incorrecta"))%>% 
  mutate(iniciop = gsub("^.*_.*_(\\d{4}).*$", "\\1", clave),fecha_evio = ymd_hms(fecha_evio)) %>% 
  mutate(Semana = paste("Semana",sems,sep = " ")) %>% 
  left_join(fconsycom,by = "IDZ") %>% 
  mutate(tiempo_apertura = if_else(fechamaxima > fecha_evio,"Apertura_entiempo","Apertura_Fueradetiempo")) %>% 
  filter(!is.na(matricula_participante)) %>%
  mutate(tipomat = substr(matricula_participante, 1, 2),matdocente = if_else(tipomat == 19,"Doce","Stud")) %>% 
  filter(matdocente == "Doce") %>% left_join(linksaula,by="aula") %>% unite("link_foro",c(link,linkforo),sep="") %>% 
  unite("ID",c(clave,grupo,sems),sep = "_") %>% 
  select(ID,tema_foro,asunto,fecha_evio,link_foro,Apertura,fechamaxima,tiempo_apertura) %>% rename(Apertura_Foro = fecha_evio) %>% 
  group_by(ID) %>% arrange(Apertura_Foro) %>% mutate(cuenteo = row_number()) %>% filter(cuenteo ==1) %>% ungroup() %>% 
  left_join(respuestas,by = "ID") %>% 
  mutate_at(c("Rating_Pendiente"),~replace(.,is.na(.),0)) %>%
  select(ID,tema_foro,asunto,Apertura_Foro,fechamaxima,tiempo_apertura,Apertura,Rating_Pendiente,link_foro)


for (i in 1:9) {
  it = as.character(i)    
  evidence <-googlesheets4::read_sheet(ss="1rK155Wv7Esaf78M2lQtj9bzfR3OCa-rOnWsXM_WORmw",range="A1:R5000",col_types = "ccccccccccdccccccc") %>% 
    rename(Gestor = `Nombre completo de GCA`,Mat_Docente = `Matricula de profesor`,Asignatura= `Nombre de materia`) %>% 
    select(Clave,Grupo,Asignatura,Mat_Docente,`Nombre titular`,`Apellidos titular`,Gestor,BLOQUE) %>% 
    unite("Docente",c(`Nombre titular`,`Apellidos titular`),sep=" ") %>% 
    mutate(ID= paste(Clave,Grupo,it,sep = "_")) %>%
    filter(!is.na(Clave)) %>% 
    mutate(Semana = paste("Semana",it,sep = " "),Tipo = "Colaborativo") %>% 
    left_join(colaborativo,by = "ID") 
  if(i==1) evidencias <- evidence else evidencias <- rbind(evidence,evidencias)
  
}



forocolaborativo <- rbind(evidencias) %>% mutate(iniciop = gsub("^.*_.*_(\\d{4}).*$", "\\1", Clave),dia=Sys.Date()) %>% 
  unite("Con",c(iniciop,dia),sep = "_") %>% 
  left_join(SemanasRanngos,by = "Con") %>% select(-Con) %>% 
  mutate(sema = as.numeric(as.character(gsub(".*?([0-9]+).*", "\\1",Semana)))) %>% 
  mutate(filtro = if_else(sema <= semans,"Dejar","Quitar")) %>% 
  filter(filtro == "Dejar") %>% select(-semans:-filtro)


write.csv(forocolaborativo,file = "G:/Mi unidad/Docentes/forocolaborativo.csv",row.names = F,fileEncoding = "LATIN1")


# 
# 
# 
# respuestas <- colabora %>%  filter(rating == -1) %>% 
#   filter(!is.na(matricula_participante
#   )) %>%
#   mutate(tipomat = substr(matricula_participante, 1, 2),matdocente = if_else(tipomat == 19,"Doce","Stud"))  %>% 
#   filter(matdocente == "Stud" )%>%  unite("ID",c(clave,grupo,sems),sep = "_") %>% 
#   count_(c('ID')) %>% rename(Rating_Pendiente=n)
# 
# 
# 
# 
# 
# 
# 
# 
# colabora <- foros %>% filter(grepl("Foro 1|Foro 2|Foro 3|Foro 4|Foro 5|Foro 6|Foro 7",foro)) %>% 
#   mutate(rating = replace_na(rating,-1)) %>% 
#   mutate(iniciop = gsub("^.*_.*_(\\d{4}).*$", "\\1", clave),fecha_evio = ymd_hms(fecha_evio),fechapost=date(fecha_evio),sems=0) %>% 
#   filter(!is.na(fechapost)) 
# 
# 
# 
# for (i in 1:length(rownames(semanas))) {
#   
#   colabora <- mutate(colabora,sems = if_else(sems == as.character(0),
#                                              if_else(iniciop == semanas[[i,1]] & fecha_evio >= semanas [[i,2]]
#                                                      & fecha_evio < semanas [[i,3]],as.character(semanas [[i,4]]),
#                                                      "0")
#                                              ,as.character(sems)))
#   
#   
# }
# 
# colabora <- mutate(colabora,sems = if_else(sems == as.character(0),"1",sems))