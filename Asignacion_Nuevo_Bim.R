sapply(c("dplyr","tidyr","lubridate","readxl","vroom","data.table","googledrive","googlesheets4","stringr"),require,character.only=T)
#####-----Inicio-----#####
Base <- read_excel("C:/Users/jsalinba.SCALA/Documents/Reportes/Bases/Asignación Posgrados.xlsx",sheet="Base",col_types = "text") %>%
  filter(Estado %in% c("Activo","FUTUROS")) %>%
  mutate_at(grep("Acc|^Fecha|Inicio_Curso|F\\.",names(.)),~if_else(!grepl("[A-Z\\-]",.),as.character(as.Date(as.numeric(.),"1899-12-30")),.)) %>%
  mutate(NIQ2 = if_else(grepl("NI",`NI-Q`),"NI",paste0(substring(`NI-Q`,1,1),as.integer(substring(`NI-Q`,2,2))+1)),
         NIQ2 = if_else(grepl("NA",NIQ2),"Pendiente",NIQ2),
         NIQ2 = if_else(grepl("^B",NIQ2),gsub("10","9",NIQ2),if_else(grepl("^Q",NIQ2),gsub("7","6",NIQ2),NIQ2)),
         NIQ2 = if_else(`NI-Q`=="NI 01 MAY","Q2",
                        if_else(`NI-Q`=="NI 03 JUL","B2",NIQ2)),
         NIQ2 = if_else(NIQ2=="NI",`NI-Q`,NIQ2),
         Estado = if_else(Estado == "FUTUROS","Activo",Estado)) %>%
  mutate(Repartir = if_else(grepl("NI|B[46789]|Q[0-9]",NIQ2),"no",
                            if_else(NIQ2 == "B2","equiparar",
                                    if_else(NIQ2 == "Pendiente","Pendiente","si"))),
         `NI-Q`=NIQ2) %>% select(-NIQ2)
#####-----Ajustando equipos B2 etiquetas-----#####
Base <- mutate(Base,`Tutor Anterior`=if_else(Repartir == "equiparar",Tutor,`Tutor Anterior`),
               Tutor = if_else(Repartir == "equiparar",gsub("B1","B2",Tutor),Tutor),
               Repartir = if_else(grepl("LATAM",Campus),"Latam",Repartir),
               Turno = if_else(Repartir=="Latam","_",Turno),
               Etiqueta = if_else(`NI-Q` != "Pendiente",
                                  if_else(Modalidad == "Doctorado",
                                          if_else(grepl("NI",`NI-Q`),120,as.integer(substring(`NI-Q`,2,2))*120),
                                          if_else(grepl("NI",`NI-Q`),60,as.integer(substring(`NI-Q`,2,2))*60)),as.integer(Etiqueta)),
               `Tutor Anterior` = if_else(Repartir=="si",Tutor,`Tutor Anterior`)) %>%
  mutate_at(c("Tutor","Supervisor"),~if_else(grepl("LATAM",Campus),"No carterizado",.)) %>%
  mutate_all(~as.character(.))
# Rep2 <- count(Base,`Nivel de Riesgo`) %>% mutate(num = rownames(.)) %>% select(-n)
# Rep <- select(Base,`Nivel de Riesgo`) %>% left_join(Rep2,"Nivel de Riesgo") %>% arrange(num,`Nivel de Riesgo`)
#####-----Imprimiendo asignación-----#####
fwrite(Base,"C:/Users/jsalinba.SCALA/Documents/Reportes/Bases/Principales/Asignación Previo.csv",bom = T,na = "_")
Rep <- Base
#####-----Ajustando Bloque de seguimiento-----#####
Rep <- select(Rep,"matricula"=Matricula,"Bl"=`NI-Q`) %>% mutate(Bl=if_else(grepl("NI",Bl),"B1",Bl))
Base <- vroom("//Tut-095/reportes/ESTADOS/bloquesegmae.csv",",",locale = locale(encoding = "LATIN1"),col_types = "c") %>%
  left_join(Rep,"matricula") %>%
  mutate(Bl=if_else(!is.na(Bl),if_else(grepl("^B",Bl),paste0(Bl," (",60*as.integer(substring(Bl,2,2))," días)"),
                                       if_else(grepl("^Q",Bl),paste0(Bl," (",120*as.integer(substring(Bl,2,2))," días)"),"no")),"no"),
         B.ingreso=if_else(Bl=="no",B.ingreso,Bl),
         Bloque = if_else(grepl("^B",B.ingreso),paste0("Bimestre 0",substring(B.ingreso,2,2)),
                          paste0("Cuatrimesrte 0",substring(B.ingreso,2,2))),
         Tipo.alumno=if_else(grepl("[BQ][23456789]",B.ingreso),gsub("NI","Regular",Tipo.alumno),gsub("Regular","NI",Tipo.alumno))) %>%
  select(-Bl)
#####-----Imprimiendo asignación-----#####
fwrite(Base,"//Tut-095/reportes/ESTADOS/bloquesegmae.csv",bom = T,na = "_")








# sapply(c("dplyr","tidyr","lubridate","readxl","vroom","data.table","googledrive","googlesheets4","stringr"),require,character.only=T)
# #####-----Inicio-----#####
# Base <- read_excel("C:/Users/jsalinba.SCALA/Documents/Reportes/Bases/Asignación Posgrados.xlsx",sheet="Base",col_types = "text") %>%
#   mutate_at(grep("Acc|^Fecha|Inicio_Curso|F\\.",names(.)),~if_else(!grepl("[A-Z\\-]",.),as.character(as.Date(as.numeric(.),"1899-12-30")),.)) %>%
#   filter(Estado %in% c("Activo","FUTUROS"),Campus=="UTEL") %>% select(Matricula,`NI-Q`,Modalidad,`Nivel de Riesgo`,Rango_Adeudo,Tutor) %>% 
#   arrange(`NI-Q`,Modalidad,Rango_Adeudo,`Nivel de Riesgo`)
# #B1
# Rep <- filter(Base,grepl("NI",`NI-Q`),Modalidad!="Doctorado") %>% arrange(`NI-Q`,`Nivel de Riesgo`,Rango_Adeudo)
# Rep <- mutate(Rep,Tutor = rep_len(1:8,length(Rep[[1]])),
#               Tutor = paste0("POSB1 Equipo ",Tutor))
# fwrite(Rep,"C:/Users/jsalinba.SCALA/Documents/Reportes/Bases/Asignación.csv",bom = T,na = "_")
# #B2
# Rep <- filter(Base,`NI-Q`=="B2",Modalidad!="Doctorado") %>% arrange(`NI-Q`,`Nivel de Riesgo`,Rango_Adeudo)
# Rep <- mutate(Rep,Tutor = rep_len(1:5,length(Rep[[1]])),
#               Tutor = paste0("POSB2 Equipo ",Tutor))
# fwrite(Rep,"C:/Users/jsalinba.SCALA/Documents/Reportes/Bases/Asignación.csv",bom = T,na = "_",append = T)
# #B3
# Rep <- filter(Base,`NI-Q`=="B3",Modalidad!="Doctorado") %>% arrange(`NI-Q`,`Nivel de Riesgo`,Rango_Adeudo)
# Rep <- mutate(Rep,Tutor = rep_len(1:8,length(Rep[[1]])),
#               Tutor = paste0("POSB3 Equipo ",Tutor))
# fwrite(Rep,"C:/Users/jsalinba.SCALA/Documents/Reportes/Bases/Asignación.csv",bom = T,na = "_",append = T)
# #B4
# Rep <- filter(Base,`NI-Q`=="B4",Modalidad!="Doctorado") %>% arrange(`NI-Q`,`Nivel de Riesgo`,Rango_Adeudo)
# Rep <- mutate(Rep,Tutor = rep_len(1:4,length(Rep[[1]])),
#               Tutor = paste0("POSB4 Equipo ",Tutor))
# fwrite(Rep,"C:/Users/jsalinba.SCALA/Documents/Reportes/Bases/Asignación.csv",bom = T,na = "_",append = T)
# #B5+
# Rep <- filter(Base,grepl("B[56789]",`NI-Q`),Modalidad!="Doctorado") %>% arrange(`NI-Q`,`Nivel de Riesgo`,Rango_Adeudo)
# Rep <- mutate(Rep,Tutor = rep_len(1:15,length(Rep[[1]])),
#               Tutor = paste0("POSB5+ Equipo ",Tutor))
# fwrite(Rep,"C:/Users/jsalinba.SCALA/Documents/Reportes/Bases/Asignación.csv",bom = T,na = "_",append = T)
# #Doctorado
# Rep <- filter(Base,grepl("^Q|NI",`NI-Q`),Modalidad=="Doctorado") %>% arrange(`NI-Q`,`Nivel de Riesgo`,Rango_Adeudo)
# Rep <- mutate(Rep,Tutor = rep_len(c("SEGUNDO HERNANDEZ DIANA","RUIZ AMBROSIO OBED GERARDO"),length(Rep[[1]])))
# fwrite(Rep,"C:/Users/jsalinba.SCALA/Documents/Reportes/Bases/Asignación.csv",bom = T,na = "_",append = T)
# #Pendientes
# Rep <- filter(Base,`NI-Q`=="Pendiente") %>% mutate(Tutor="Pendiente")
# fwrite(Rep,"C:/Users/jsalinba.SCALA/Documents/Reportes/Bases/Asignación.csv",bom = T,na = "_",append = T)
# 
# #tamaños de cadena
# c1 <- 20
# c2 <- 5
# c3 <- 10
# c4 <- 8
# #mínimo y máximo de equipos
# min <- 1
# max <- 8
# 
# #vuelta 1
# cad <- rep_len(min:max,c1)
# print(cad)
# #vuelta 2
# res <- tail(cad,1)+1
# cad <- rep_len(res:max,(max-res)+1)
# if (c2<=length(cad)) {
#   cad <- rep_len(res:max,c2)
# }else{
#   cad <- append(cad,rep_len(min:max,c2-length(cad)))
# }
# print(cad)
# #vuelta 3
# res <- tail(cad,1)+1
# cad <- rep_len(res:max,(max-res)+1)
# if (c3<=length(cad)) {
#   cad <- rep_len(res:max,c3)
# }else{
#   cad <- c(cad,rep_len(min:max,c3-length(cad)))
# }
# print(cad)
# #vuelta 4
# res <- tail(cad,1)+1
# cad <- rep_len(res:max,(max-res)+1)
# if (c4<=length(cad)) {
#   cad <- rep_len(res:max,c4)
# }else{
#   cad <- c(cad,rep_len(min:max,c4-length(cad)))
# }
# print(cad)







