sapply(c("MultiLEDS","dplyr","tidyr","lubridate","stringr","data.table","vroom"),library,character.only=T)
c1 <- "C:/Users/jsalinba/Documents/Reportes"; c2 <- "//Tut-095/reportes/ESTADOS"
diremail(c2,"jsalinba@utel.edu.mx")
#####---------Inicio---------#####
Base <- vroom("//Tut-095/reportes/ESTADOS/bloquesegmae.csv",",",locale = locale(encoding = "LATIN1"),col_types = "c")
#Base <- leer(c("bloquesegmae","ESTADOS$"))
#Base <- leer(c("bloquesegmae","ESTADOS$"),enc = "UTF-8")
diremail(c1)
Rep <- leer(c("Asignación Posgrados","Bases$"),sheet="Base") %>% select("matricula"=Matricula,Campus,`NI-Q`,Tipo,Modalidad,Estado) %>%
  filter(Estado %in% c("Activo","FUTUROS","MM","MM (Activo)","Egresado","CV")) %>%
  left_join(Base,by="matricula") %>% rename("Bim"=B.ingreso) %>%
  filter(is.na(Formación)) %>% filter(grepl("^(B|Q)|NI (29 MAY|03 JUL|31 JUL|28 AGO|25 SEP|23 OCT|20 NOV|08 ENE)|Pendiente",`NI-Q`)) %>%
  mutate(Formación=if_else(grepl("Sesi",Tipo),"Sesión Ejecutiva",if_else(grepl("Seni",Tipo),"Senior",if_else(grepl("Ejec",Tipo),"Ejecutivas","Online"))),
         Bloque = if_else(Modalidad=="Doctorado","Cuatrimestre","Bimestre"),
         Bloque = if_else(grepl("^(B|Q)",`NI-Q`),
                          if_else(nchar(`NI-Q`)==2,paste(Bloque," 0",substring(`NI-Q`,2,2),sep=""),paste(Bloque,substring(`NI-Q`,2,3),sep=" ")),
                          paste(Bloque," 01",sep="")),
         Tipo.alumno=if_else(grepl("NI",`NI-Q`),"NI","Regular"),Campus=gsub("LATAM|\\(|\\)| ","",Campus),Campus=str_to_title(Campus,"es"),
         Tipo.alumno=if_else(Formación=="Ejecutivas",paste(Tipo.alumno,"Ejecutivo",Campus,sep=" "),
                             if_else(Formación=="Online",paste(Tipo.alumno,Campus,sep=" "),paste(Tipo.alumno,Formación,sep=" "))),
         Tipo.alumno=gsub(" Utel","",Tipo.alumno),
         Bim=if_else(grepl("NI",`NI-Q`),"B1",`NI-Q`),BTI="-") %>% left_join(leer(c("Info General","gsheet"),sheet="Bloque seguimiento"),by="Bim") %>%
  select(matricula,B.ingreso,Bloque,BTI,Tipo.alumno,Formación)
Base <- rbind(Base,Rep)
diremail(c2)
escribir(Base,c("bloquesegmae.csv","ESTADOS$"))
#data.table::fwrite(Base,"//Tut-095/reportes/ESTADOS/bloquesegmae.csv",bom = T,na = "_")




























