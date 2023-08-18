sapply(c("vroom","dplyr","stringr","tidyr","data.table"), library, character.only = T)
#####-----Leyendo-----#####
#Leyendo csv, con codificación "LATIN1", todas las columnas en tipo caracter y seleccionando solo MATRICULA y ESTUDIANTE cambiando su nombre
Base <- vroom("D:/Trabajo/Bases/Originales/SIR/Estatus General Alumnos.csv",",",locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c"),
              col_select = c("Matricula"=MATRICULA,"Nom"=ESTUDIANTE)) %>% filter(!duplicated(Matricula))
#####-----Arreglando y separando-----#####
#Quitando "N/A", duplicados y espacios innecesarios en "/".
#Se cuenta el número de "/" para saber el máximo de separación en columnas.
Base <- mutate(Base,Nom = gsub(" N/A|N/A","",Nom),Nom = gsub("//| /|/ ","/",Nom),cont = str_count(Nom,"/"))
#Separando al máximo de columnas y uniendo tercer columna sobrante sin contar "NA"
#Si solo tenía un guion el nombre, se separa del primero la última palabra, se una con la segunta y se cuenta como apellido.
Base <- separate(Base,Nom,paste0("C",as.character(1:max(Base$cont))),sep = "/")
Rep <- filter(Base,cont==1) %>% mutate(C3 = gsub("^.*\\s(\\w+)$","\\1",C1),
                                       C1 = gsub("\\s\\w+$","",C1)) %>%
  unite(Apellidos,C3,C2,sep = " ") %>% rename("Nombre"=C1)
#Uniendo ambas bases
Base <- rename(Base,"Apellidos"=C2,"Nombre"=C1) %>% select(-C3) %>% filter(cont!=1) %>% rbind(Rep)
#Imprimiendo
Base <- select(Base,-cont)
Base <- fwrite(Base,"D:/Trabajo/Bases/Principales/Nombres EG arreglados.csv",bom = T)






















