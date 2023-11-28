library(tidyverse)
library(readxl)
library(googlesheets4)
library(googledrive)
library(readr)

gs4_auth("jvelazhe@utel.edu.mx")

fconsycom <- read_excel("D:/Users/jvelazhe/Desktop/Licenciaturas/fechasforoconsultasycomen.xlsx",sheet = "fechasapertura") %>% 
  mutate(fechamaxima = ymd_hms(fechamaxima)) %>% unite("IDZ",c(iniciop,Semana),sep = "_") 
linksaula <- read_excel("D:/Users/jvelazhe/Desktop/Licenciaturas/fechasforospresentacion.xlsx", sheet = "linksaula")

foros <- read.csv("G:/Mi unidad/Reports/Foros_Licconsultas.csv", header = TRUE, sep = ",",encoding = "UTF-8")



respuestas <- foros %>%  filter(foro == "Espacio del profesor") %>% 
  filter(grepl("Re:",asunto))  %>% 
  filter(!is.na(matricula_participante)) %>%
  mutate(tipomat = substr(matricula_participante, 1, 2),matdocente = if_else(tipomat == 19,"Doce","Stud"))  
  
respuestasdoce <- respuestas %>%  filter(matdocente == "Doce") %>%  unite("ID",c(clave,grupo),sep = "_") %>% 
  count_(c('ID')) %>% rename(Respuestas_Docente=n)


respuestasstu <- respuestas %>%  filter(matdocente == "Stud") %>%  unite("ID",c(clave,grupo),sep = "_") %>% 
  count_(c('ID')) %>% rename(Respuestas_Estudiantes=n)

presentaciondocentes <- foros %>%  filter(foro == "Espacio del profesor") %>% 
  mutate(Apertura = if_else(grepl("CONSUL|Cons|consul|COM|Com|com",tema_foro),"Apertura_Correcta","Apertura_Incorrecta")) %>% 
  filter(!grepl("Re:",asunto)) %>% mutate(iniciop = gsub("^.*_.*_(\\d{4}).*$", "\\1", clave),fecha_evio = ymd_hms(fecha_evio)) %>% 
  mutate(Semana = "Semana 1",SEM="1") %>% 
  unite("IDZ",c(iniciop,SEM),sep = "_") %>% 
  left_join(fconsycom,by = "IDZ") %>% 
  mutate(tiempo_apertura = if_else(fechamaxima > fecha_evio,"Apertura_entiempo","Apertura_Fueradetiempo")) %>% 
  filter(!is.na(matricula_participante)) %>%
  mutate(tipomat = substr(matricula_participante, 1, 2),matdocente = if_else(tipomat == 19,"Doce","Stud")) %>% 
  filter(matdocente == "Doce") %>% left_join(linksaula,by="aula") %>% unite("link_foro",c(link,linkforo),sep="") %>% 
  unite("ID",c(clave,grupo),sep = "_") %>% 
  select(ID,tema_foro,asunto,fecha_evio,link_foro,Apertura,fechamaxima,tiempo_apertura) %>% rename(Apertura_Foro = fecha_evio) %>% 
  group_by(ID) %>% arrange(Apertura_Foro) %>% mutate(cuenteo = row_number()) %>% filter(cuenteo == 1) %>% ungroup() %>% 
  left_join(respuestasdoce,by = "ID") %>% 
  left_join(respuestasstu,by="ID") %>%
  mutate_at(c("Respuestas_Docente","Respuestas_Estudiantes"),~replace(.,is.na(.),0)) %>% 
  select(ID,tema_foro,asunto,Apertura_Foro,fechamaxima,tiempo_apertura,Apertura,Respuestas_Docente,Respuestas_Estudiantes,link_foro)  

#3185
#3239


consultasycomentarios <-googlesheets4::read_sheet(ss="1rK155Wv7Esaf78M2lQtj9bzfR3OCa-rOnWsXM_WORmw",range="A1:R5000",col_types = "ccccccccccdccccccc") %>% 
  rename(Gestor = `Nombre completo de GCA`,Mat_Docente = `Matricula de profesor`,Asignatura= `Nombre de materia`) %>% 
  select(Clave,Grupo,Asignatura,Mat_Docente,`Nombre titular`,`Apellidos titular`,Gestor,BLOQUE) %>% 
  unite("Docente",c(`Nombre titular`,`Apellidos titular`),sep=" ") %>% 
  unite("ID",c(Clave,Grupo),sep = "_",remove = F) %>%
  filter(!is.na(Clave)) %>%
  mutate(Semana = "Semana 1",Tipo = "Consultas y Comentarios") %>% 
  left_join(presentaciondocentes,by = "ID")


