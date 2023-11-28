library(MultiLEDS); library(magrittr); library(dplyr); library(tidyr)
source("~/Reportes/Scripts/R/Consultas_MySQL.R")
drive <- leer(c("Info General","gsheet"),sheet="Reportes drive",secc="Niveles")
#####-------Aulas-------#####
BaseL <- data.frame(); BaseM <- data.frame()
for (mod in names(Aulas)) {#Modaldad
  for (aula in names(Aulas[[mod]])) {#Aula
    if (!grepl("ModEva$|Conx$",aula)) {#Solo si la consulta no es la modalidad de evaluación o la conexión
      eval(parse(text=Aulas[[mod]][[paste(aula,"_Conx",sep="")]]))
      Base <- leer(c(Aulas[[mod]][[aula]],"msql"))
      Rep <- leer(c(Aulas[[mod]][[paste(aula,"_ModEva",sep="")]],"msql"))
      #Cruces
      if (mod=="Licenciatura") {
        Rep <- mutate(Rep,matricula=as.numeric(as.character(matricula))) %>% select(matricula,clave,modalidad) %>% unite(Con,matricula,clave,sep="_")
        Base <- mutate_at(Base,c("matricula","calificacion"),~as.numeric(as.character(.))) %>% mutate(calificacion=replace_na(calificacion,0)) %>%
          unite(Con,matricula,clave,sep="_",remove = F) %>% left_join(Rep,by="Con") %>% mutate(modalidad=replace_na(modalidad,"Sin Modalidad"),aula=aula)
        if(length(BaseL)==0) BaseL <- Base else BaseL <- rbind(BaseL,Base)
        print(c("Aula",aula,"Modalidad",mod,"filas",length(BaseL[[1]])))
      }else{
        Base <- mutate_at(Base,c("matricula","calificacion"),~as.numeric(as.character(.))) %>% mutate(calificacion=replace_na(calificacion,0),aula=aula)
        if(length(BaseM)==0) BaseM <- Base else BaseM <- rbind(BaseM,Base)
        print(c("Aula",aula,"Modalidad",mod,"filas",length(BaseM[[1]])))
      }
    }
  }
}
BaseL <- filter(BaseL,grupo!="Grupo_O" & grupo!="Grupo_S") %>% unite(Nombre.Alumno,Nombre,Apellidos,remove = F) %>%
  select(status,institucion,id,matricula,ciudad,Nombre,Apellidos,Nombre.Alumno,correo,materia_id,clave,asignatura,grupo,modalidad,calificacion,
         ultimo_acceso_m,ultimo_acceso_auvi,aula,fecha_inicio,fecha_fin,rol) %>% filter(!is.na(matricula)) %>% mutate(aula=gsub("moodle_aula","Aula",aula))
BaseL <- codificacion(BaseL)
escribir(BaseL,c("reporte_completo_Lic.csv","Aulas/Licenciatura$"))

BaseM <- filter(BaseM,grupo!="Grupo_O" & grupo!="Grupo_S") %>% separate(clave,c("ciclo"),remove=F) %>% left_join(drive,by="ciclo") %>%
  mutate(nivel=replace_na(nivel,"Sin_Definir"),aula=gsub("moodle_aula","Aula",aula)) %>%
  select(status_plataforma,status,institucion,nivel,matricula,Nombre,Apellidos,correo,materia_id,clave,grupo,
         calificacion,rol,asignatura,`Fecha enrolado`,`Fecha mdf enrol`,ultimo_acceso_m,`fecha add grupo`,ultimo_acceso_auvi,aula)
BaseM <- codificacion(BaseM)
escribir(BaseM,c("reporte_completo_Mae.csv","Aulas/Licenciatura$"))

  



# devtools::install_github("YuerSebastian/SynerLyst")






