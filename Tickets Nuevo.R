library(MultiLEDS); library(dplyr); library(tidyr); library(lubridate); library(stringi)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#####---------------------Unificando---------------------#####
Base <- unificar("FLOKZU$",#Se unifican y arreglan algunos nombres, se separa el proceso.
                 
                 c(Matricula="Matrícula",SIU.="Matrícula",`Student ID`="Matrícula",Student_ID="Matrícula",
                   `Correo del estudiante`="Correo",Correo_del_alumno="Correo",`Correo del prospecto`="Correo",`Correo electronico`="Correo",
                   `Student Email`="Correo",`Student email`="Correo",`student email`="Correo",
                   `Creación de la tarea`="Fecha Inicio",
                   `Nombre de Tarea`="Tarea",
                   `Area responsable`="Analista SER",`A.analista SER`="Analista SER"),
                 c("Identificador","Matrícula","Correo","Fecha Inicio","Tarea","Analista SER")) %>% mutate(Identificador=toupper(Identificador))%>%
  mutate(`Fecha Inicio`=if_else(nchar(`Fecha Inicio`)==16,paste(`Fecha Inicio`,":02",sep=""),`Fecha Inicio`),`Fecha Inicio`=ymd_hms(`Fecha Inicio`),
         `Analista SER`= gsub("Francisco Javier Sandoval Cevantes","Francisco Javier Sandoval Cervantes",`Analista SER`),
         `Analista SER`= gsub("Jocelyn Velarde Hernandez","Jocelyn Velarde Hernández",`Analista SER`),Proceso=gsub("-[0-9]+","",Identificador),
         Proceso=if_else(Proceso=="CAVE","CaVe",Proceso)) %>% arrange(desc(`Fecha Inicio`))
#####---------------------Cruce base anterior---------------------#####
#Cerrados
Rep <- leer(c("Tickets","Principales$"),col_select=c(Identificador,Matrícula,Correo,"Fecha Inicio"=`Fecha Tarea`,"Fecha Fin"=`Fecha Fin Real`,`Fecha Asignación`,
                                                     "Tarea"=`Tarea Actual`,`Analista SER`,Proceso,"Estado Anterior"=Estado,Responsable,`Responsable Apoyo`))%>%
  mutate_at(grep("^Fecha",names(.)),~if_else(nchar(.)==16,paste(.,":02",sep=""),.)) %>% mutate_at(grep("^Fecha",names(.)),~ymd_hms(.))%>%
  mutate(Estado=.[[1]] %in% Base[[1]],Estado=if_else(Estado==F,"Cerrado","_")) %>% arrange(desc(`Fecha Inicio`))
tam <- length(Rep[[1]])
#Nuevos,estado y fecha fin
Base <- left_join(Base,Rep[c("Identificador","Estado Anterior","Fecha Fin","Fecha Asignación","Responsable","Responsable Apoyo")],by="Identificador")%>%
  mutate(Estado=.[[1]] %in% Rep[[1]],Estado=if_else(Estado==F,"N","_"))%>%
  mutate(Estado=if_else(!(`Estado Anterior` %in% c("Cerrado","Regresado")) | Estado=="N",
                        if_else(grepl("SER|ARCHIVO",Tarea),"Activo","Ayuda"),"Regreasado"))%>%
  rbind(filter(Rep,Estado=="Cerrado")) %>% add_count(Identificador,name = "No. Duplicados") %>% filter(!duplicated(Identificador))%>%
  mutate(`Fecha Fin`=if_else(is.na(`Fecha Fin`) & Estado=="Cerrado",now(),`Fecha Fin`),
         `Fecha Asignación`=replace_na(`Fecha Asignación`,now()))
#####---------------------Cálculos---------------------#####
Base <- mutate(Base,Días=if_else(Estado!="Cerrado",as.integer(difftime(now(),`Fecha Inicio`,units="days")),-1L),
               Atraso=if_else(Días!=-1,if_else(Días<3,"En tiempo",#Atraso
                                               if_else(Días<5,"Atraso",
                                                       if_else(Días<8,"Urgente","Prioridad"))),"_"),
               `Rango Cerrado`=if_else(Días==-1,if_else(difftime(`Fecha Fin`,`Fecha Inicio`,units="days")<3,"0 a 2 días",#Rango Cerrado
                                                        if_else(difftime(`Fecha Fin`,`Fecha Inicio`,units="days")<5,"3 a 4 días",
                                                                if_else(difftime(`Fecha Fin`,`Fecha Inicio`,units="days")<8,"5 a 7 días","8 días o más"))),"_"),
               `Atraso Cerrado`=if_else(Días==-1,if_else(`Rango Cerrado`=="0 a 2 días","En tiempo",#Atraso Cerrado
                                                         if_else(`Rango Cerrado`=="3 a 4 días","Atraso",
                                                                 if_else(`Rango Cerrado`=="5 a 7 días","Urgente","Prioridad"))),"_"),
               Contingencia=if_else(`Fecha Inicio`<"2020-03-23","Antes","Contingencia"))#Contingencia
#####---------------------Asignación---------------------#####
Asig <- filter(Base,is.na(Responsable))
if (length(rownames(Asig))!=0) {#Solo si existen nuevos
  #Contando asignación del año actual para ordenar analistas, se excluyen cancelados, se cuentan todos los trabajados.
  Rep <- leer(c("Responsables Normal","gsheet"),sheet = wday(today(),week_start = 1)) %>% filter(Excepciones != "N") %>% select(-Excepciones)
  for (i in 2:length(Rep)) {
    Rep2 <- filter(Asig,Proceso==names(Rep)[i])
    if (length(rownames(Rep2))!=0) { #Si existen procesos que asignar...
      cont <- filter(Base,year(`Fecha Asignación`) == year(now()) & Estado != "Cancelado" & Proceso == names(Rep)[i]) %>% count(Responsable) %>% arrange(n)
      x <- subset.data.frame(Rep,Rep[i]!="_",c(1,i)) %>% left_join(cont,by = "Responsable") %>% arrange(n) %>% .[[1]]
      Rep2 <- mutate(Rep2,Responsable=rep_len(x,length(rownames(Rep2)))); Asig <- filter(Asig,Proceso!=names(Rep)[i]) %>% rbind(Rep2)
    }
  }
  Base <- filter(Base,!is.na(Responsable)) %>% rbind(Asig)
}
#####----------------------------------------------------Asignando Apoyos (Lunes, S1 y S2)----------------------------------------------------#####
if (wday(today(),week_start = 1)==1) {
  #Identificando los apoyos a asignar
  Rep <- leer(c("Responsables Normal","gsheet"),sheet = "Procesos",col_select=c("Proceso","Especial"))
  Base <- left_join(Base,Rep,by="Proceso") %>% mutate(Especial=if_else(Especial=="Apoyo",
                                                                       if_else(is.na(`Responsable Apoyo`)&(Estado=="Activo"|Estado=="Activo (regresado)"),Especial,"_"),"_"))
  #Contando para ordenar y asignando
  cont <- filter(Base,year(`Fecha Asignación`) == year(now()) & Estado != "Cancelado SER") %>% count(`Responsable Apoyo`) %>% arrange(n)
  Rep <- leer(c("Responsables Normal","gsheet"),sheet = "Apoyos") %>% left_join(cont,by="Responsable Apoyo") %>% arrange(n) %>% select(-n)
  Rep2 <- filter(Base,Especial=="Apoyo") %>% mutate(`Responsable Apoyo`=rep_len(Rep$`Responsable Apoyo`,length(rownames(.))))
  Base <- filter(Base,Especial!="Apoyo") %>% rbind(Rep2) %>% select(-Especial)
}
remove(cont,Rep2,Asig)
#####---------------------Estatus general (Campus y Nivel)---------------------#####
Rep <- leer(c("Estatus_General_Alumno","SIR$"),col_select=c("Matrícula"=MATRICULA,"Correo"=CORREO,FECHA_ESTATUS,FECHAINICIO,"Clave Campus"=CAMPUS,"Clave Nivel"=NIVEL))%>%
  mutate_at(c(grep("^FECHA",names(.))),~dmy(.)) %>% arrange(desc(FECHA_ESTATUS),desc(FECHAINICIO)) %>% filter(!duplicated(Matrícula)) %>% select(-c(grep("^FECHA",names(.))))
Rep2 <- select(Rep,-Correo)
Base <- left_join(Base,Rep2,by="Matrícula")
Rep2 <- select(Rep,-Matrícula) %>% filter(!duplicated(Correo))
Base <- left_join(Base,Rep2,by="Correo") %>% mutate(`Clave Campus.x`=if_else(is.na(`Clave Campus.x`),`Clave Campus.y`,`Clave Campus.x`,"_"),
                                                    `Clave Nivel.x`=if_else(is.na(`Clave Nivel.x`),`Clave Nivel.y`,`Clave Nivel.x`,"_"))%>%
  select(-c("Clave Campus.y","Clave Nivel.y")) %>% rename("Clave Campus"=`Clave Campus.x`,"Clave Nivel"=`Clave Nivel.x`)
remove(Rep2)
#####---------------------Cruzando con Info general---------------------#####
#NOTA: Simplificar después
x <- list(c("Info Analistas","Procesos"),c("Campus","Niveles"),c("Tickets","General"))
for (i in 1:length(x[[3]])) {
  for (j in 1:length(x[[i]])) {
    if (j==1) Rep <- leer(c("Info General","gsheet"),sheet=x[[3]][[i]])
    Rep2 <- extr_secc(Rep,x[[i]][[j]])
    #assign(paste("y",i,j,sep = "."),Rep2)
    if (hasName(Rep2,"Proceso")) Rep2 <- select(Rep2,c(1:grep("Descripción Proceso",names(Rep2))),-Sección)#Solo para seleccionar columnas de procesos necesarias
    if (hasName(Rep2,"Campus")) Rep2 <- select(Rep2,-`Fecha Campus`)
    if (hasName(Rep2,"Analista SER")) {#Solo para cruzar el coordinador de los apoyos y el turno con el responsable
      Rep3 <- select(Rep2,"Responsable Apoyo"=`Analista SER`,"Coordinador Apoyo"=Coordinador); Base <- left_join(Base,Rep3,by="Responsable Apoyo")
      Rep3 <- select(Rep2,"Responsable"=`Analista SER`,Turno); Base <- left_join(Base,Rep3,by="Responsable")
      Rep2 <- select(Rep2,-Turno)
    }
    Base <- left_join(Base,Rep2,by=names(Rep2)[1])
  }
}
remove(Rep,Rep2,Rep3,x); Base <- filter(Base,!duplicated(Identificador)) #Para quitar duplicados generados (no se por que se duplica),Posible aquí por la matrícula o correo
#####---------------------------Re ordenando e imprimiendo---------------------------#####
Base <- mutate_all(Base,~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
Rep <- leer(c("Info General","gsheet"),sheet="Orden Base",secc="Tickets")
Base <- `colnames<-`(Base,Rep$Nuevo) %>% .[c(Rep$Incluir[!is.na(Rep$Incluir)])]
if (length(Base[[1]])>tam) {
  escribir(Base,c("Tickets.csv","Principales$"))
}else{
  print("Error en la base.")
}


























