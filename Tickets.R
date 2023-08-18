library(MultiLEDS); library(dplyr); library(tidyr); library(lubridate)
#Variables principales
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
x <- list(c("FLOKZU$", "FLOKZU Global$"),
          c("Identificador","Matrícula","Correo","Fecha Inicio","Finalizado","Fecha Finalización","Tarea Actual","Fecha Tarea","Analista SER"),
          list(c(`Fecha asignación Tarea actual` = "Fecha Tarea",
                 `A.analista SER` = "Analista SER",
                 `Correo del estudiante` = "Correo",
                 `Correo del prospecto` = "Correo",
                 `SIU` = "Matrícula",
                 `Matricula` = "Matrícula"),
               c(`Date when ongoing Task was assigned` = "Fecha Tarea",
                 `Student Services Analyst` = "Analista SER",
                 `Student email` = "Correo",
                 `Prospect email` = "Correo",
                 Identifier = "Identificador",
                 `Student ID` = "Matrícula",
                 `Initiation Date` = "Fecha Inicio",
                 Completed = "Finalizado",
                 `Completion date` = "Fecha Finalización",
                 `Ongoing Task` = "Tarea Actual")))
#####---------------------Unificando reportes---------------------#####
for (i in 1:length(x[[1]])) {
  Rep <- unificar(x[[1]][i],x[[2]],x[[3]][[i]],skip=1)
  if (i==1) Base <- Rep else Base <- rbind(Base,Rep)
}
#####---------------------Separando Proceso y arreglando nombres---------------------#####
Base <- mutate(Base,Proceso=gsub("-[0-9]+","",Identificador))%>%
  mutate(`Analista SER`= gsub("Francisco Javier Sandoval Cevantes","Francisco Javier Sandoval Cervantes",`Analista SER`),
         `Analista SER`= gsub("Jocelyn Velarde Hernandez","Jocelyn Velarde Hernández",`Analista SER`))
#####---------------------Cruce base anterior---------------------#####
Rep <- leer(c("Tickets Resúmen","auxiliares$"),lazy=F,col_select = c("Identificador","Responsable","Responsable Apoyo","EstadoA"=Estado,"Fecha Asignación","FFRA"=`Fecha Fin Real`))
Base <- left_join(Base,Rep,by = "Identificador") %>% mutate(`Fecha Asignación`=if_else(is.na(`Fecha Asignación`),as.character(now()),`Fecha Asignación`),
                                                            nuevo=if_else(is.na(Responsable),if_else(grepl("\\[SER|\\[STUDENT SERVICES|\\[ARCHIVO",`Tarea Actual`),"N","x"),"_"))%>%
  filter(nuevo!="x") %>% mutate_at(c(grep("^Fecha|FFRA",names(Base))),~if_else(nchar(.)==16,paste(.,":02",sep=""),.))#Fechas con 0 segundos

# if (length(rownames(Base))>=length(rownames(Rep))) {
#   
# }else{print("La base anterior tiene más registros que la actual...hay pérdida de datos.")}

#####---------------------Estado y regresados---------------------#####
Rep <- leer(c("Info General","gsheet"),sheet="Tickets",secc="Tareas")
Base$Estado <- "Aplicado"
for (i in 1:length(rownames(Rep))) {
  Base <- mutate(Base,Estado=if_else(is.na(`Tarea Actual`),"Sin tarea",
                                     if_else(Estado=="Aplicado",if_else(grepl(Rep[[i,1]],`Tarea Actual`,ignore.case = T),Rep[[i,2]],"Aplicado"),Estado)))
}
Base <- mutate(Base,Estado=if_else(Estado=="Activo",if_else(!is.na(EstadoA)&(EstadoA!="Activo"|EstadoA=="Activo (regresado)"),"Activo (regresado)",Estado),Estado),
               `Fecha Asignación`=if_else(nuevo!="N",
                                          if_else(!grepl("Activo",EstadoA),
                                                  if_else(Estado=="Activo (regresado)",as.character(now()),`Fecha Asignación`),`Fecha Asignación`),`Fecha Asignación`))
#####---------------------Asignación---------------------#####
Asig <- filter(Base,nuevo=="N")
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
  Base <- filter(Base,nuevo!="N") %>% rbind(Asig)
}
#####---------------------Asignando Apoyos (Lunes, S1 y S2)---------------------#####
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
#####---------------------Fechas (cálculos)---------------------#####
Base <- mutate_all(Base,~replace_na(.,"_")) %>% mutate(`Fecha Fin Real`=if_else(Estado!="Activo" & Estado!="Activo (regresado)" & Estado!="Ayuda",#Fecha Fin Real
                                                                                if_else(`Fecha Finalización`=="_",
                                                                                        `Fecha Tarea`,if_else(FFRA=="_" | FFRA==`Fecha Finalización`,
                                                                                                              `Fecha Finalización`,FFRA)),"_"))%>%
  mutate_at(c(grep("^Fecha",names(.))),~ymd_hms(.))%>%
  mutate(Trabajado=if_else(Estado!="Activo",#Trabajado
                           if_else((`Fecha Asignación` > `Fecha Fin Real`) & (!is.na(`Fecha Fin Real`) & !is.na(`Fecha Asignación`)),"X",
                                   if_else(Responsable==`Analista SER`,"Si",
                                           if_else(`Responsable Apoyo`==`Analista SER`,"Si (Apoyo)","No"))),"_"),
         Días=as.integer(difftime(now(),`Fecha Tarea`,units="days")),Días=if_else(is.na(Días),-1L,Días),#Días
         Atraso=if_else(Días!=-1,if_else(Días<3,"En tiempo",#Atraso
                                         if_else(Días<5,"Atraso",
                                                 if_else(Días<8,"Urgente","Prioridad"))),"_"),
         `Rango Cerrado`=if_else(Días==-1,if_else(difftime(`Fecha Fin Real`,`Fecha Asignación`,units="days")<3,"0 a 2 días",#Rango Cerrado
                                                  if_else(difftime(`Fecha Fin Real`,`Fecha Asignación`,units="days")<5,"3 a 4 días",
                                                          if_else(difftime(`Fecha Fin Real`,`Fecha Asignación`,units="days")<8,"5 a 7 días","8 días o más"))),"_"),
         `Atraso Cerrado`=if_else(Días==-1,if_else(`Rango Cerrado`=="0 a 2 días","En tiempo",#Atraso Cerrado
                                                   if_else(`Rango Cerrado`=="3 a 4 días","Atraso",
                                                           if_else(`Rango Cerrado`=="5 a 7 días","Urgente","Prioridad"))),"_"),
         Contingencia=if_else(`Fecha Inicio`<"2020-03-23","Antes","Contingencia"),#Contingencia
         `Asignación Real`="Si")#Asignación real, solo para saber desde cuándo se tienen fechas de la asignación real de los tickets.
Base <- select(Base,-EstadoA,-FFRA,-nuevo) %>% mutate_all(~as.character(.)) %>% mutate_all(~replace_na(.,"_"))#Quitando columnas que no se necesitan y convirtiendo a caracter
#####---------------------Resumen, Histórico y Estatus general (Campus y Nivel)---------------------#####
#escribir(Base,c("Tickets Resúmen2.csv","auxiliares$"))
Rep <- leer(c("Tickets Histórico","Históricos$")); Base <- rbind(Base,Rep)
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
remove(Rep,Rep2,Rep3,x); Base <- filter(Base,!duplicated(Identificador)) #Para quitar duplicados generados (no se por que se duplica)
#####---------------------Cruzando con Info general---------------------#####



















