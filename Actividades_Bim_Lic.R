sapply(c("MultiLEDS","dplyr","tidyr","lubridate"),library,character.only=T)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
f <- as.Date("2023-02-27")
#####--------Descargas--------#####
drive_sd("des",c("Reports/Actividades_Lic.csv","auxiliares$"))
drive_sd("des",c("Reports/cal_exas_Lic.csv","auxiliares$"))
drive_sd("des",c("Reports/Acc_Prof_Lic.csv","auxiliares$"))
drive_sd("des",c(paste("Niveles/NR_",Sys.Date(),".csv",sep=""),"Originales/NR$"))
drive_sd("des",c(paste("Niveles/Sabana_",Sys.Date(),".csv",sep=""),"Originales/Sabana$"))
#####--------Unificando examenes y actividades--------#####
Rep <- leer(c("cal_exas_Lic","auxiliares$"),col_select=c(matricula,perfil,"Ciclo"=ciclo,"Actividad"=actividad,"Fecha"=completado,calificacion,"Aula"=aula,"Grupo"=grupo)) %>%
  codificacion() %>% rename("Matrícula"=matricula,"Calificación"=calificacion) %>% filter(perfil=="Estudiante") %>% select(-perfil) %>% mutate(`Tipo Evaluación`="Examen")
Base <- leer(c("Actividades_Lic","auxiliares$"),col_select=c(matricula,"Ciclo"=clave,"Actividad"=actividad,"Fecha"=fechadeenvio,calificacion,
                                                            "Aula"=aula)) %>% codificacion() %>% rename("Matrícula"=matricula,"Calificación"=calificacion) %>% 
  mutate(`Tipo Evaluación`="Actividad",Fecha=paste(Fecha,":00",sep=""),Grupo="_") %>% rbind(Rep) %>%
  mutate(Matrícula=if_else(nchar(Matrícula)<9,paste("0",Matrícula,sep=""),Matrícula),Fecha=ymd_hms(Fecha),Fecha2=as.Date(Fecha)) %>%
  filter(!grepl("execu|registro",Actividad,ignore.case = T)) %>% separate(Ciclo,c("1","2","Inicio Plus","Clave Materia"),remove = F) %>% #Por aquí se filtraba aula
  select(-`1`,-`2`) %>% unite(MATEGRUP,`Clave Materia`,Grupo,remove = F)
#####--------Drive (Pendiente)--------#####
# Rep <- leer(c("Acc_Prof_Lic","auxiliares$"),col_select=c(clave,Grupo,Aula,Programa,))
# 
# Rep <- leer(c("18kyoNbSewZ5lgpoUyI-qs4Ta50Wde8HA5FAZp_fK2MM","gsheet.ID"),
#             col_select=c(`Clave de materia`,Grupo,Aula,Programa,"Clave Profesor"=Clave_prof,"Profesor"=Nombre,Correo,Gestor,Revisor)) %>%
#   filter(Aula=="12") %>% select(-Aula) %>% unite(MATEGRUP,`Clave de materia`,Grupo)
# Base <- left_join(Base,Rep,by="MATEGRUP") %>% mutate(Semana="_")

Base <- mutate(Base,Semana="_")
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
Rep <- leer(c(paste("NR_",Sys.Date(),sep=""),"Originales/NR$"),col_select=c("Matrícula"=matricula,`Bloque seguimiento`,Semaforo)) %>%
  mutate(Matrícula=if_else(nchar(Matrícula)<9,paste("0",Matrícula,sep=""),Matrícula)) %>% filter(!duplicated(Matrícula))
Base <- left_join(Base,Rep,by="Matrícula")
#NR pendiente, no hay ciclo.
# Rep <- leer(c(paste("Sabana_",Sys.Date(),sep=""),"Originales/Sabana$"),col_select=c(Matricula,Ciclo,"Nivel Riesgo"=NR)) %>%
#   mutate(Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula)) %>% unite(Clave,Matricula,Ciclo) %>% filter(!duplicated(Clave))
# Base <- unite(Base,Clave,Matrícula,Ciclo,remove = F) %>% left_join(Rep,by="Clave")
#Tipo Actividad
Rep <- leer(c("Actividades y examenes","Bases$"),sheet="Lic")
Base <- list("Actividades"=filter(Base,`Tipo Evaluación`=="Actividad"),"Examenes"=filter(Base,`Tipo Evaluación`=="Examen"))
#Actividades
Base$Actividades <- left_join(Base$Actividades,extr_secc(Rep,"Actividades"),by="Actividad")
#Examenes
Base$Examenes <- left_join(Base$Examenes,extr_secc(Rep,"Examenes"),by="Actividad")
#Unificando filtro
Base <- rbind(Base$Actividades,Base$Examenes)
#####--------Ordenando e imprimiendo--------#####
Base <- Base[c("MATEGRUP","Matrícula","Ciclo","Bloque seguimiento","Inicio Plus","Semana","Clave Materia","Aula","Grupo","Tipo Evaluación","Tipo Actividad","Actividad",
               "Calificación","Semaforo","Fecha")] %>% mutate_all(~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
escribir(Base,c("Actividades Bimestrales Lic.csv","Principales$"))






















