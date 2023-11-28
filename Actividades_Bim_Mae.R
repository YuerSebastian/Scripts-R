sapply(c("MultiLEDS","dplyr","tidyr","lubridate","stringr","vroom"),library,character.only=T)
diremail("C:/Users/jsalinba/Documents/Reportes","jsalinba@utel.edu.mx")
f <- as.Date("2023-10-23")
#####--------Descargas--------#####
drive_sd("des",c("Reports/Activities_Mae.csv","auxiliares$"))
drive_sd("des",c("Reports/cal_exas_Mae.csv","auxiliares$"))
drive_sd("des",c("Reports/Acc_Prof_Mae.csv","auxiliares$"))
drive_sd("des",c(paste("Niveles/NR_POSGRADOS_",Sys.Date(),".csv",sep=""),"Originales/NR$"))
drive_sd("des",c(paste("Niveles/Sabana_POSGRADOS_",Sys.Date(),".csv",sep=""),"Originales/Sabana$"))
#####--------Unificando examenes y actividades--------#####
Rep <- leer(c("cal_exas_Mae","auxiliares$"),col_select=c(Usuario,Ciclo,Actividad,"Fecha"=Completado,calificacion,"Aula"=aula,Grupo)) %>%
  rename("Matrícula"=Usuario,"Calificación"=calificacion) %>% mutate(`Tipo Evaluación`="Examen")
Base <- leer(c("Activities_Mae","auxiliares$"),col_select=c(matricula,"Ciclo"=clave,"Actividad"=actividad,"Fecha"=fechadeenvio,calificacion,
                                                            "Aula"=aula,"Grupo"=grupo)) %>% rename("Matrícula"=matricula,"Calificación"=calificacion) %>% 
  mutate(`Tipo Evaluación`="Actividad",Fecha=paste(Fecha,":00",sep="")) %>% rbind(Rep) %>%
  mutate(Matrícula=if_else(nchar(Matrícula)<9,paste("0",Matrícula,sep=""),Matrícula),Fecha=ymd_hms(Fecha),Fecha2=as.Date(Fecha)) %>%
  filter(!grepl("execu|registro",Actividad,ignore.case = T)) %>% separate(Ciclo,c("1","2","Inicio Plus","Clave Materia"),remove = F) %>%#Aula=="Aula12",
  select(-`1`,-`2`) %>% unite(MATEGRUP,Ciclo,Grupo,remove = F)
#####--------Drive--------#####
Rep <- leer(c("Info General","gsheet"),sheet = "Operación Pos") %>% rename("Clave Profesor"=Clave_prof,"Profesor"=Nombre) %>% unite(MATEGRUP,Shortname,Grupo) %>% select(-Aula,-`Clave de materia`)
  #filter(Aula=="12") %>% select(-Aula) %>% unite(MATEGRUP,`Clave de materia`,Grupo)
Base <- left_join(Base,Rep,by="MATEGRUP") %>% mutate(Semana="_")#Bloque.seguimiento,bloque,IDS(Ciclo)
#####--------Otra info--------#####
#Semanas
#Por alguna razón no detecta bien una fecha para las semanas con la función ymd_hms(),se usa as.Date() para evaluar las semanas de manera correcta,
#tal vez sea la diferencia de formato ymd_hms() y ymd()
for (i in 1:15) {
  Base <- mutate(Base,Semana=if_else(Fecha2>=f & Fecha2<=(f+days(6)),paste("Semana ",i,sep=""),Semana))
  f <- f+days(7)
}
Base <- select(Base,-Fecha2)
#Bloque,Semaforo y NR
f <- Sys.Date()
Rep <- leer(c(paste("NR_POSGRADOS_",Sys.Date(),sep=""),"Originales/NR$"),col_select=c("Matrícula"=Matricula,"Bloque",Ciclo,Semaforo)) %>% filter(Ciclo %in% c("2023-10-23","2023-11-20")) %>%
  mutate(Matrícula=if_else(nchar(Matrícula)<9,paste("0",Matrícula,sep=""),Matrícula)) %>% select(-Ciclo) %>% filter(!duplicated(Matrícula))
Base <- left_join(Base,Rep,by="Matrícula")
#NR
Rep <- leer(c(paste("Sabana_POSGRADOS_",Sys.Date(),sep=""),"Originales/Sabana$"),col_select=c(Matricula,Ciclo,"Nivel Riesgo"=NR)) %>%
  mutate(Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula)) %>% unite(Clave,Matricula,Ciclo) %>% filter(!duplicated(Clave))
Base <- unite(Base,Clave,Matrícula,Ciclo,remove = F) %>% left_join(Rep,by="Clave")
#####-----Tipo Actividad-----#####
Rep <- leer(c("Info General","gsheet"),sheet="Actividades Mae")
Rep <- list("Actividades" = extr_secc(Rep,"Actividades"),
            "Examenes" = extr_secc(Rep,"Examenes"))
Base <- list("Actividades"=filter(Base,`Tipo Evaluación`=="Actividad") %>% mutate(`Tipo Actividad`="_"),
             "Examenes"=filter(Base,`Tipo Evaluación`=="Examen") %>% mutate(`Tipo Actividad`="_"))
#Actividades
for (i in 1:length(Rep$Actividades[[1]])){
  Base$Actividades <- mutate(Base$Actividades,`Tipo Actividad` = if_else(grepl(Rep$Actividades[[i,"Patron"]],Actividad,T),Rep$Actividades[[i,"Tipo Actividad"]],
                                                                         `Tipo Actividad`))
}
#Examenes
for (i in 1:length(Rep$Examenes[[1]])){
  Base$Examenes <- mutate(Base$Examenes,`Tipo Actividad` = if_else(grepl(Rep$Examenes[[i,"Patron"]],Actividad,T),Rep$Examenes[[i,"Tipo Actividad"]],
                                                                   `Tipo Actividad`))
}
#Unificando filtro
Base <- rbind(Base$Actividades,Base$Examenes) %>% mutate(`Tipo Actividad` = if_else(`Tipo Actividad`=="_","Otros",`Tipo Actividad`))
####--------Ordenando e imprimiendo--------#####
Base <- Base[c("MATEGRUP","Matrícula","Programa","Ciclo","Bloque","Inicio Plus","Semana","Clave Materia","Aula","Grupo","Tipo Evaluación","Tipo Actividad","Actividad","Calificación",
               "Nivel Riesgo","Semaforo","Fecha","Clave Profesor","Profesor","Correo","Gestor","Revisor")] %>% mutate_all(~as.character(.)) %>% mutate_all(~replace_na(.,"_")) %>%
  filter(!is.na(Matrícula),Matrícula!="_")
#escribir(Base,c("Actividades Bimestrales Pos.csv","Principales$"))

vroom_write(Base,"C:/Users/jsalinba/Documents/Reportes/Bases/Principales/Actividades Bimestrales Pos.csv",",","\r\n",bom = T,quote = "needed",na = "_")






