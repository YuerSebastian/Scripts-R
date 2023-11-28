library(MultiLEDS); library(googledrive); library(dplyr); library(tidyr);library(lubridate)
diremail("C:/Users/jsalinba.SCALA/Documents/Reportes","jsalinba@utel.edu.mx")
#shell("C:/Users/jsalinba.SCALA/Documents/Reportes/Scripts/Python/Valid_Calif.py")
#Fecha Sabana
Rep <- leer(c("1m_KV3OxKO5Pr55Nxc-uLFgbPXbqlVnEqyyu0jswzCnM","gsheet.ID")) %>% .[[1,"Fecha Sabana"]] %>% dmy() %>% as.character()
#Descargas
fecha <- Rep
# file.copy("//IP4687/Users/mafzena.SCALA/Documents/UTEL GLOBAL/reporte_completo_global.csv",
#           "C:/Users/jsalinba.SCALA/Documents/Reportes/Bases/Originales/Sabana/reporte_completo_global.csv",overwrite = T)
# drive_sd("des",c(paste("NR_UTEL_GLOBAL/Sabana_",fecha,".csv",sep=""),"Originales/Sabana$"))
# # op_arch("ren",c(paste("Sabana",fecha,sep="_"),paste("Sabana",fecha,"Global",sep="_"),"Originales/Sabana$"))
# drive_sd("des",c(paste("Niveles/Sabana_",fecha,".csv",sep=""),"Originales/Sabana$"))
# drive_sd("des",c(paste("Niveles/Sabana_POSGRADOS_",fecha,".csv",sep=""),"Originales/Sabana$"))
# drive_sd("des",c("Reports/reporte_completo_Mae.csv","Originales/Sabana$"))
#Info General
Drive <- leer(c("Info General","gsheet"),sheet="Otros",range="AE:AJ")
#####----------------Uniendo Sabanas----------------#####
#Doctorado y maestría
Rep <- leer(c(paste("Sabana_POSGRADOS",fecha,sep="_"),"Originales/Sabana$"),
            col_select=c("Matrícula"=Matricula,Ciclo,"CalificaciónS"=Calificacion,"Nivel"=Modalidad,NR,"Estado Alumno"=Estado.Banner,
                         "Estado Moodle"=Status.Materia,Aula)) %>% 
  separate(Ciclo,into=c("1","2","Inicio Plus","Clave Materia"),sep="_") %>% select(-"1",-"2")%>%
  mutate(Matrícula=if_else(nchar(Matrícula)<9,paste("0",Matrícula,sep=""),Matrícula),CalificaciónS=if_else(NR=="Np"|NR=="Nuncas","0",CalificaciónS),
         `Tipo Calif`="Mae y Doc",Base="Posgrado",`Tipo Materia`="Curricular") %>% unite(MATMATER,Matrícula,`Clave Materia`,remove=F)%>%
  .[c("MATMATER","Matrícula","Nivel","Estado Alumno","Estado Moodle","Inicio Plus","Clave Materia","CalificaciónS","NR","Tipo Calif","Tipo Materia","Base")] %>%
  filter(Nivel != "Doctorado")
Sab <- Rep
#Licenciatura
Rep <- leer(c(paste("Sabana",fecha,sep="_"),"Originales/Sabana$"),
            col_select=c("Matrícula"=matricula,"Clave Materia"=clave_materia,"CalificaciónS"=calificacion,"NR"=nivel_riesgo,"Estado Alumno"=stutus,
                         "Estado Moodle"=edo_moodle,"Inicio Plus"=inicio_plus)) %>% #filter(`Inicio Plus`=="1402" | `Inicio Plus`=="2802")%>%
  mutate(Matrícula=if_else(nchar(Matrícula)<9,paste("0",Matrícula,sep=""),Matrícula),CalificaciónS=if_else(NR=="Np"|NR=="Nuncas","0",CalificaciónS),
         Nivel="Licenciatura",`Tipo Calif`="Lic",Base="Licenciatura",`Tipo Materia`="Curricular") %>% unite(MATMATER,Matrícula,`Clave Materia`,remove=F)%>%
  .[c("MATMATER","Matrícula","Nivel","Estado Alumno","Estado Moodle","Inicio Plus","Clave Materia","CalificaciónS","NR","Tipo Calif","Tipo Materia","Base")]
Sab <- rbind(Sab,Rep)
#Global
# Rep <- leer(c(paste("Sabana",fecha,"Global",sep="_"),"Originales/Sabana$"),
#             col_select=c("Matrícula"=matricula,"Clave Materia"=clave_materia,"CalificaciónS"=calificacion,"NR"=nivel_riesgo,"Estado Alumno"=stutus,
#                          "Estado Moodle"=edo_moodle,"Inicio Plus"=inicio_plus,Nivel))%>%
#   mutate(Matrícula=if_else(nchar(Matrícula)<9,paste("0",Matrícula,sep=""),Matrícula),CalificaciónS=if_else(NR=="Np"|NR=="Nuncas","0",CalificaciónS),
#          Base="Global",Nivel=if_else(Nivel=="Bache","Licenciatura","Maestría"),`Tipo Calif`=if_else(Nivel=="Licenciatura","Lic","Mae y Doc"),
#          `Tipo Materia`="Curricular") %>% unite(MATMATER,Matrícula,`Clave Materia`,remove=F)%>%
#   .[c("MATMATER","Matrícula","Nivel","Estado Alumno","Estado Moodle","Inicio Plus","Clave Materia","CalificaciónS","NR","Tipo Calif","Tipo Materia","Base")]
# Sab <- rbind(Sab,Rep)
#Extracurriculares
Rep2 <- extr_secc(Drive,"Materias Extras") %>% rename("Materia"=`Clave Materia`)
Rep <- leer(c("reporte_completo_Mae","Sabana$"),col_select=c("Matrícula"=matricula,clave,"CalificaciónS"=calificacion,"Estado Moodle"=status,aula))%>%
  filter(!grepl("^niv_",clave),aula != "Aula13") %>% 
  mutate(Matrícula=if_else(nchar(Matrícula)<9,paste("0",Matrícula,sep=""),Matrícula),NR="No aplica",Nivel="No aplica",`Estado Alumno`="No aplica",
         Base="Completo Mae") %>% separate(clave,into = c("1","2","Inicio Plus","Clave Materia")) %>% select(-"1",-"2") %>%
  mutate(Materia=gsub("[0-9]","",`Clave Materia`)) %>%
  left_join(Rep2,by="Materia") %>% select(-Materia) %>% unite(MATMATER,Matrícula,`Clave Materia`,remove=F)%>%# %>% filter(!is.na(`Tipo Materia`))
  .[c("MATMATER","Matrícula","Nivel","Estado Alumno","Estado Moodle","Inicio Plus","Clave Materia","CalificaciónS","NR","Tipo Calif","Tipo Materia","Base")]
#Sab <- rbind(Sab,Rep)
#Extracurriculares global
# Rep2 <- extr_secc(Drive,"Materias Extras") %>% rename("Materia"=`Clave Materia`)
# Rep <- leer(c("reporte_completo_global","Sabana$"),col_select=c("Matrícula"=matricula,clave,"CalificaciónS"=calificacion,"Estado Moodle"=status))%>%
#   filter(!grepl("^niv_",clave)) %>% mutate(Matrícula=if_else(nchar(Matrícula)<9,paste("0",Matrícula,sep=""),Matrícula),NR="No aplica",Nivel="No aplica",
#                                            `Estado Alumno`="No aplica",Base="Completo Global")%>%
#   separate(clave,into = c("1","2","Inicio Plus","Clave Materia")) %>% select(-"1",-"2") %>% mutate(Materia=gsub("[0-9]","",`Clave Materia`)) %>%
#   left_join(Rep2,by="Materia") %>% select(-Materia) %>% unite(MATMATER,Matrícula,`Clave Materia`,remove=F)%>%# %>% filter(!is.na(`Tipo Materia`))
#   .[c("MATMATER","Matrícula","Nivel","Estado Alumno","Estado Moodle","Inicio Plus","Clave Materia","CalificaciónS","NR","Tipo Calif","Tipo Materia","Base")]
Sab <- rbind(Sab,Rep) %>% mutate(Conversión="_",CalificaciónS=as.numeric(CalificaciónS)); remove(Rep2)
#Arreglando tipos de materias
Rep <- extr_secc(Drive,"Materias Extras") %>% rename("Materia"=`Clave Materia`)
Sab <- mutate(Sab,Materia=gsub("[0-9]","",`Clave Materia`)) %>% left_join(Rep,by="Materia")%>%
  mutate(`Tipo Materia.x`=if_else(!is.na(`Tipo Materia.y`),`Tipo Materia.y`,`Tipo Materia.x`),
         `Tipo Calif.x`=if_else(!is.na(`Tipo Calif.y`),`Tipo Calif.y`,`Tipo Calif.x`)) %>% rename("Tipo Materia"=`Tipo Materia.x`,"Tipo Calif"=`Tipo Calif.x`)%>%
  select(-`Tipo Materia.y`,-`Tipo Calif.y`,-Materia) %>%
  mutate(`Tipo Calif`=replace_na(`Tipo Calif`,"Mae y Doc"),`Tipo materia`=replace_na(`Tipo Materia`,"Curricular"))
#Rangos Calificación
Rep <- extr_secc(Drive,"Calificaciones") %>% mutate(Conversión=if_else(is.na(Conversión),"NA",Conversión))
x <- c("Lic","Mae y Doc","Extra Mae","Extra Lic")
for (nom in x) {
  Rep2 <- filter(Rep,Nivel==nom) %>% select(2,3)
  for (i in 1:length(rownames(Rep2))) {
    Rep3 <- Rep2[i,] %>% separate(Rango,into = c("c1","c2"),sep = " a ") %>% mutate_at(c("c1","c2"),~as.numeric(.))
    Sab <- mutate(Sab,Conversión=if_else(`Tipo Calif`==nom,
                                         if_else(Conversión=="_",
                                                 if_else(CalificaciónS>=Rep3[[1,"c1"]] & CalificaciónS<=Rep3[[1,"c2"]],Rep3[[1,"Conversión"]],Conversión),Conversión),
                                         Conversión))
  }
};Sab <- mutate(Sab,CalificaciónS=Conversión) %>% select(-Conversión,-`Tipo Calif`)
#Quitando duplicados (Matrícula,Materia,Inicio Plus)
Sab <- unite(Sab,MATMATER,MATMATER,`Inicio Plus`,remove=F) %>% filter(!duplicated(MATMATER))
#####----------------Uniendo Pronósticos----------------#####
Rep <- extr_secc(Drive,"Materias Extras") %>% rename("Materia"=`Clave Materia`)
Pro <- unificar(c("Calificaciones$","Pron_"),
                col_select=c("Matrícula"=MATRICULA,"Claves"=SHORTNAME,"CalificaciónP"=CALIF,"Estado Materia"=ESTATUS_MATERIA,"Regla"=REGLA,"Origen"=ORIGEN,
                             "Nivel"=NIVEL)) %>% arrange(desc(`Estado Materia`)) %>% separate(Claves,into=c("1","2","Inicio Plus","Clave Materia"),sep="_") %>%
  select(-"1",-"2")%>%
  mutate(Origen=if_else(Origen=="ORDINARIA","Ordinaria","Extraordinaria"),Matrícula=if_else(nchar(Matrícula)<9,paste("0",Matrícula,sep=""),Matrícula),
         CalificaciónP=if_else(is.na(CalificaciónP),"NA",CalificaciónP),Conversión="_",Materia=gsub("[0-9]","",`Clave Materia`))%>%
  unite(MATMATER,Matrícula,`Clave Materia`,remove=F) %>% filter(!duplicated(MATMATER)) %>% left_join(Rep,by="Materia")%>%
  mutate(`Tipo Calif`=if_else(is.na(`Tipo Calif`),
                              if_else(Nivel=="DO"|Nivel=="MA","Mae y Doc","Lic"),`Tipo Calif`),
         `Tipo Materia`=if_else(is.na(`Tipo Materia`),"Curricular",`Tipo Materia`),CalificaciónP=if_else(CalificaciónP=="NP"|CalificaciónP=="NA","0",
                               if_else(CalificaciónP=="AC","10",CalificaciónP)),CalificaciónP=as.numeric(CalificaciónP))
#Rangos Calificación
Rep <- extr_secc(Drive,"Calificaciones") %>% mutate(Conversión=if_else(is.na(Conversión),"NA",Conversión))
x <- c("Lic","Mae y Doc","Extra Mae","Extra Lic")
for (nom in x) {
  Rep2 <- filter(Rep,Nivel==nom) %>% select(2,3)
  for (i in 1:length(rownames(Rep2))) {
    Rep3 <- Rep2[i,] %>% separate(Rango,into = c("c1","c2"),sep = " a ") %>% mutate_at(c("c1","c2"),~as.numeric(.))
    Pro <- mutate(Pro,Conversión=if_else(`Tipo Calif`==nom,
                                         if_else(Conversión=="_",
                                                 if_else(CalificaciónP>=Rep3[[1,"c1"]] & CalificaciónP<=Rep3[[1,"c2"]],Rep3[[1,"Conversión"]],Conversión),Conversión),
                                         Conversión))
  }
};Pro <- mutate(Pro,CalificaciónP=Conversión) %>% select(-Conversión,-Materia,-`Tipo Calif`); remove(Rep,Rep2,Rep3)
Pro <- unite(Pro,MATMATER,MATMATER,`Inicio Plus`,remove=F)
#####----------------Cruzando----------------#####
Sab <- left_join(Sab,Pro[c("MATMATER","CalificaciónP","Estado Materia","Regla","Origen")],by="MATMATER" )%>%
  mutate(Encontrado=if_else(!is.na(CalificaciónP),"Encontrado","Error"),
         `Calificación OK`=if_else(CalificaciónP==CalificaciónS,"Correcto","Error")) %>% mutate_all(~as.character(.)) %>% mutate_all(~replace_na(.,"No definido"))
Pro <- left_join(Pro,Sab[c("MATMATER","Estado Alumno","Estado Moodle","CalificaciónS","NR","Base","Calificación OK")],by="MATMATER")%>%
  mutate(Encontrado=if_else(!is.na(CalificaciónS),"Encontrado","Error")) %>% mutate_all(~replace_na(.,"No definido"))
x <- c("MATMATER","Matrícula","Estado Alumno","Inicio Plus","Nivel","Estado Moodle","Clave Materia","Estado Materia","Regla","Origen","Tipo Materia","NR",
       "CalificaciónP","CalificaciónS","Calificación OK","Encontrado","Base")
#####----------------Validando 5 y NP iguales----------------#####
Sab <- Sab[x] %>% filter(`Clave Materia`!="M1HB401") %>%
  mutate(`Calificación OK`=if_else((CalificaciónS=="5"&CalificaciónP=="NP") | (CalificaciónS=="NP" & CalificaciónP=="5"),"Correcto",`Calificación OK`)) %>%
  filter(Nivel != "Doctorado")
Pro <- Pro[x] %>% filter(`Clave Materia`!="M1HB401") %>%
  mutate(`Calificación OK`=if_else((CalificaciónP=="5"&CalificaciónS=="NP") | (CalificaciónP=="NP" & CalificaciónS=="5"),"Correcto",`Calificación OK`))
#####----------------Imprimiendo----------------#####
escribir(Sab,c("Validación Calif Sabana.csv","Principales$"))
escribir(Pro,c("Validación Calif Pronóstico.csv","Principales$"))

















