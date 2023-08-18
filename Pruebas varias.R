sapply(c("dplyr","tidyr","lubridate","readxl","vroom","data.table","googledrive","googlesheets4","stringi","stringr"),require,character.only=T)
#####-----Descargas-----#####
drive_auth("jsalinba@utel.edu.mx")
gs4_auth("jsalinba@utel.edu.mx")
des <- as_dribble(c("Reports/Foros_Posgrapresentacion.csv","Reports/Foros_posgra.csv"))
for (i in 1:length(des[[1]])) {
  drive_download(des[i,],paste0("D:/Trabajo/Bases/Drive/",des[[i,"name"]]),overwrite = T)
}
#####-----Foro de presentación-----#####
Base <- vroom("D:/Trabajo/Bases/Drive/Foros_Posgrapresentacion.csv", ",", col_types = cols(.default = "c"), locale = locale(encoding = "UTF-8")) %>%
  mutate(tipo_mat = if_else(!is.na(matricula_participante),if_else(grepl("^19",matricula_participante),"Do","Al"),"SM"),
         iniciop = gsub("^.*_.*_(\\d{4}).*$", "\\1", clave),
         fecha_evio = ymd_hms(fecha_evio),
         fechamaxima = ymd_hms(paste0("2023-",substring(iniciop,3,4),"-",substring(iniciop,1,2)," 08:00:00")),
         tiempo_apertura = if_else(fecha_evio < fechamaxima,"Apertura_entiempo","Apertura_Fueradetiempo"),
         Apertura = if_else(grepl("presen|bienv",tema_foro,ignore.case = T),"Correcta","Incorrecta"),
         link_foro = paste0("https://",tolower(aula),".utel.edu.mx/mod/forum/discuss.php?d=",linkforo),
         Canva = if_else(grepl("embed",enlace_completo),"Si","No"),
         linkcanva = if_else(Canva=="Si",paste0("https://www.canva.com/design/",enlace_completo),"Sin video")) %>% rename(Apertura_Foro=fecha_evio) %>%
  filter(matricula_participante!="SM") %>% unite(ID,clave,grupo,remove = F)
#-----Respuestas-----#
Rep <- filter(Base,grepl("Re:",asunto),tipo_mat=="Do") %>% unite(ID,clave,grupo) %>% count(ID,name="Respuestas_Docente")
Base <- left_join(Base,Rep,"ID")
Rep <- filter(Base,grepl("Re:",asunto),tipo_mat=="Al") %>% unite(ID,clave,grupo) %>% count(ID,name="Respuestas_Estudiantes")
Base <- left_join(Base,Rep,"ID")
#-----Selección de columnas-----#
Base <- filter(Base,tipo_mat=="Do") %>% arrange(Apertura_Foro) %>% filter(!duplicated(ID)) %>%
  mutate_at(c("Respuestas_Docente","Respuestas_Estudiantes"),~replace_na(.,0)) %>%
  select(ID,tema_foro,asunto,Apertura_Foro,fechamaxima,tiempo_apertura,Apertura,Respuestas_Docente,Respuestas_Estudiantes,link_foro,Canva,linkcanva)
#-----Operación Mae y Doc-----#
Rep <- list(Mae1 = read_sheet("18kyoNbSewZ5lgpoUyI-qs4Ta50Wde8HA5FAZp_fK2MM",sheet = "MAE MA024_B",col_types = "c"),
            Mae2 = read_sheet("18kyoNbSewZ5lgpoUyI-qs4Ta50Wde8HA5FAZp_fK2MM",sheet = "MAE MA024_C",col_types = "c"),
            Doc = read_sheet("1gMij94xq1jiPq8HCn5kMnV9wZrDCB3yjU8jfDu6VRu0",col_types = "c")) %>%
  lapply(select,"Clave"=Shortname,Grupo,Asignatura,"Mat_Docente"=Clave_prof,"Docente"=Nombre,Gestor,"BLOQUE"=2) %>%
  bind_rows() %>% unite(ID,Clave,Grupo,remove = F) %>% filter(!is.na(Clave)) %>% left_join(Base,"ID")
Base <- Rep
#-----Imprimiendo y subiendo-----#
fwrite(Base,"D:/Trabajo/Bases/Principales/Foros_presentacion.csv",bom = T)
drive_put("D:/Trabajo/Bases/Principales/Foros_presentacion.csv","Docentes_Posgrados/")
#####-----Foros colaborativos-----#####
#Semana:
#1. Se extraen las palabras que contengan foro y su número
#2. Se reemplazan las palabras "foro" que no tiene espacio por uno que si contenga, ejemplo de "foro2" a "foro 2"
#3. Se convierte a minúsculas
#4. Se reemplazan los números en romano con una función que permite reemplazo múltiple evitando necesidad de un bucle
#5. Se convierte a entero
#6. En la segunda transformación, si en las condiciones anteriores no da "NA": si "asunto" tiene la palabra "cierre", se multiplica el número x 2, de
#lo contrario se multiplica x 2 -1
#Si de lo contrario, en "Semana" da "NA", se queda el mismo número del foro multiplicado x 2.
#A toda esa transformación se le pega la palabra "Semana" seguido del número que haya dado en la columna.
Base <- vroom("D:/Trabajo/Bases/Drive/Foros_posgra.csv", ",", col_types = cols(.default = "c"),
              locale = locale(encoding = "UTF-8")) %>% filter(grepl("Foro [0-9]",foro),!is.na(fecha_evio)) %>%
  mutate(tipo_mat = if_else(!is.na(matricula_participante),if_else(grepl("^019",matricula_participante),"Do","Al"),"SM"),
         rating = replace_na(rating,"-1"),
         iniciop = gsub("^.*_.*_(\\d{4}).*$", "\\1", clave),
         Semana = as.integer(str_replace_all(tolower(gsub("foro(| )","",stri_extract(asunto,regex = "foro(| | semana )([0-9]|(I|II|III|IV|V|VI))",
                                                                                     stri_opts_regex(case_insensitive = T)),
                                                          ignore.case = T)),
                                             setNames(c("1","2","3","4","5","6",""),c("i","ii","iii","iv","v","vi","semana ")))),
         fecha_evio = ymd_hms(fecha_evio),
         fechamaxima = ymd_hms(paste0("2023-",substring(iniciop,3,4),"-",substring(iniciop,1,2)," 06:00:00"))+days((7*Semana)),
         Semana = paste0("Semana ",if_else(!is.na(Semana),
                                           if_else(grepl("cierre",asunto,ignore.case = T),Semana*2,(Semana*2-1)),as.integer(substring(foro,6,6))*2)),
         tiempo_apertura = if_else(fecha_evio < fechamaxima,"Apertura_entiempo","Apertura_Fueradetiempo"),
         Apertura = if_else(grepl("Foro [0-9]",asunto),"Correcta","Incorrecta"),
         link_foro = paste0("https://",tolower(aula),".utel.edu.mx/mod/forum/discuss.php?d=",linkforo),) %>% 
  rename(Apertura_Foro=fecha_evio) %>%  filter(matricula_participante!="SM") %>% unite(ID,clave,grupo,Semana,remove = F)
#-----Respuestas-----#
Rep <- filter(Base,tipo_mat=="Al",rating=="-1") %>% unite(ID,clave,grupo,Semana) %>% count(ID,name="Rating_Pendiente")
Base <- left_join(Base,Rep,"ID")
#-----Respuestas-----#
Base <- filter(Base,tipo_mat=="Do") %>% arrange(Apertura_Foro) %>% mutate(Rating_Pendiente = replace_na(Rating_Pendiente,0)) %>% filter(!duplicated(ID)) %>%
  select(ID,tema_foro,asunto,Apertura_Foro,fechamaxima,tiempo_apertura,Apertura,Rating_Pendiente,link_foro)
#-----Operación Mae y Doc-----#
Rep <- list(Mae1 = read_sheet("18kyoNbSewZ5lgpoUyI-qs4Ta50Wde8HA5FAZp_fK2MM",sheet = "MAE MA024_B",col_types = "c"),
            Mae2 = read_sheet("18kyoNbSewZ5lgpoUyI-qs4Ta50Wde8HA5FAZp_fK2MM",sheet = "MAE MA024_C",col_types = "c"),
            Doc = read_sheet("1gMij94xq1jiPq8HCn5kMnV9wZrDCB3yjU8jfDu6VRu0",col_types = "c")) %>%
  lapply(select,"Clave"=Shortname,Grupo,Asignatura,"Mat_Docente"=Clave_prof,"Docente"=Nombre,Gestor,"BLOQUE"=2) %>%
  bind_rows() %>% unite(ID,Clave,Grupo,remove = F) %>% filter(!is.na(Clave))
Rep2 <- data.frame()
for (i in 1:9) {
  Rep <- mutate(Rep,Semana = paste0("Semana ",i))
  Rep2 <- rbind(Rep2,Rep)
}
Rep <- Rep2 %>% unite(ID,ID,Semana,remove=F) %>% left_join(Base,"ID") %>% filter(as.integer(gsub("Semana ","",Semana))<=7) %>% mutate(Tipo="Colaborativo")
Base <- Rep
Base <- select(Base,Clave:BLOQUE,ID,Semana,Tipo,tema_foro:link_foro)
#-----Ordenando e imprimiendo-----#
fwrite(Base,"D:/Trabajo/Bases/Principales/forocolaborativo.csv",bom = T)
drive_put("D:/Trabajo/Bases/Principales/forocolaborativo.csv","Docentes_Posgrados/")































