library(MultiLEDS); library(dplyr); library(tidyr); library(googlesheets4); library(lubridate)
c1 <- "//Tut-095/reportes/ESTADOS"; c2 <- "C:/Users/jsalinba.SCALA/Documents/Reportes"
diremail(c2,"jsalinba@utel.edu.mx")
Drive <- leer(c("1dEeJqXn944LaVSvDGAq2awfaQdDDhHveWmDGCqFcb_c","gsheet.ID"),sheet="Asignación")
#####--------------Inicio--------------#####
Rep <- leer(c("1dEeJqXn944LaVSvDGAq2awfaQdDDhHveWmDGCqFcb_c","gsheet.ID"),sheet="Correos",col_select=c(Matricula,`Clave Programa`,`Gestor A`,`Gestor N`,Razón,`Fecha Reasignación`,Base))
Base <- leer(c("Reasignados Posgrados","auxiliares")) %>% rbind(Rep)
escribir(Base,c("Reasignados Posgrados.csv","auxiliares"))
diremail(c1)
#####--------------Inicio--------------#####
#Leyendo base
Base <- leer(c("AsignacionPosgrados","Posgrados$"),sheet="Asignacion",skip=1) %>% mutate(Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula))%>%
  unite(MATPROG,Matricula,`Clave Programa`,sep="",remove=F)
diremail(c2); Rep <- leer(c("Proyectos","Reportes$")); Base <- left_join(Base,Rep,by="Programas") %>% mutate(Programas=Proyecto) %>% select(-Proyecto)
noms <- names(Base)
#Leyendo respuestas y cruzando.
Rep <- leer(c("1dEeJqXn944LaVSvDGAq2awfaQdDDhHveWmDGCqFcb_c","gsheet.ID"),sheet="Reasignación") %>% rename("Gestor"=`Gestor...8`,"Gestor Anterior"=`Gestor...11`)%>% 
  rename("Fecha"=`Marca temporal`,"Tipo asig"=`Tipo de re asignación`,"Motivo asig"=`Motivo de cambio`,"Turno asig"=Turno,"Proyecto asig"=Proyecto,"Link"=`Link CRM`)%>%
  mutate(Matrícula=if_else(nchar(Matrícula)<9,paste("0",Matrícula,sep=""),Matrícula),Fecha=dmy_hms(Fecha)) %>% unite(MATPROG,Matrícula,`Clave del programa`,sep="")%>%
  arrange(desc(Fecha)) %>% filter(!duplicated(MATPROG),Base=="Posgrados") %>% select(2:8)%>%
  filter(!MATPROG %in% (leer(c("Reasignados Posgrados","auxiliares$")) %>% unite(MATPROG,Matricula,`Clave Programa`,sep = "") %>% .[["MATPROG"]]))
Base <- left_join(Base,Rep,by="MATPROG")
#Identificando que asignar (Proyectos)
Rep <- Drive %>% select(Tutor,Proyecto) %>% group_by(Tutor) %>% mutate(cont=row_number()) %>% filter(!duplicated(Proyecto)) %>% spread(Proyecto,cont) %>% ungroup()%>%
  mutate(`Tip tut`="")
for (i in 2:13) {
  Rep <- mutate(Rep,`Tip tut`=if_else(!is.na(Rep[i]),paste(`Tip tut`,names(Rep[i]),sep="_"),`Tip tut`))
}; Rep <- select(Rep,Tutor,`Tip tut`) %>% mutate(`Tip tut`=substring(`Tip tut`,2))
Base <- left_join(Base,Rep,by="Tutor") %>% separate(`Tip tut`,into = c("t1","t2","t3"),sep = "_") %>% mutate_at(c("t1","t2","t3"),~replace_na(.,"_"))
#Identificando que asignar (Avanzados)
Rep <- Drive %>% select(Tutor,"TipTut"=Tipo) %>% group_by(Tutor,TipTut) %>% filter(!duplicated(Tutor)) %>% ungroup()
Base <- left_join(Base,Rep,by="Tutor") %>% mutate(TipTut=replace_na(TipTut,"_"))
#Identificando
Base <- mutate(Base,Asignar=if_else(`Tipo asig`=="Re asignación",
                                    if_else(`Motivo asig`=="Turno",if_else(Turno!=`Turno asig`,"si","no"),
                                            if_else(`Motivo asig`=="Proyecto",if_else(t1!=Programas & t2!=Programas & t3!=Programas,"si","no"),
                                                    if_else(TipTut!="Avanzado","si","no"))) #Esta parte puede no ser necesaria
                                    ,if_else(Tutor!=Gestor,"si","no")),Asignar=replace_na(Asignar,"_"),
               `Tutor Anterior`=if_else(Asignar=="si",Tutor,`Tutor Anterior`),
               Tipo=if_else(!is.na(`Motivo asig`) & `Motivo asig`=="Avanzados" & Asignar=="si","Avanzado",Tipo))#Se actualiza el tipo a avanzado.
#####--------------Asignación--------------#####
Rep <- Drive %>% filter(!grepl("Team",Tipo),!grepl("baja|otro equipo",Observación,ignore.case = T))%>%
  left_join(add_count(filter(Base[c("Tutor","Estado","NI-Q")],Estado %in% c("Activo","FUTUROS"),`NI-Q` %in% c("NI 29 AGO","NI 26 SEP")),Tutor) %>% select(Tutor,n)%>%
              unique(),by="Tutor") %>% arrange(n) %>% mutate(n=replace_na(n,0))
#Tutor específico, no se necesitan hacer más filtros.
Base <- mutate(Base,Tutor=if_else(Asignar=="si",
                                  if_else(is.na(`Motivo asig`),Gestor,Tutor),Tutor))
#Turno, se toma en cuenta mismo tipo y proyecto
Asig <- filter(Base,`Motivo asig`=="Turno",Asignar=="si") %>% mutate(Tutor="_")
x <- c("Matutino","Vespertino"); y <- c("Onboarding","Avanzado"); z <- filter(Rep["Proyecto"],!duplicated(Proyecto)) %>% .[[1]]
for (i in 1:2) {
  for (j in 1:2) {
    for (k in 1:length(z)) {
      Rep2 <- filter(Asig,`Turno asig`==x[i],Tipo==y[j],Programas==z[k])
      if (length(Rep2[[1]])>0) {
        Rep2 <- mutate(Rep2,Tutor=rep_len(filter(Rep[c("Tutor","Turno","Tipo","Proyecto")],Turno==x[i],Tipo==y[j],Proyecto==z[k]) %>% .[[1]],length(Rep2[[1]])))
        Asig <- filter(Asig,`Turno asig`!=x[i] | Tipo!=y[j] | Programas!=z[k]); Asig <- rbind(Asig,Rep2)
      }
    }
  }
}
if (length(Asig[[1]])>0) {
  Base <- filter(Base,!MATPROG %in% Asig$MATPROG); Base <- rbind(Base,Asig)
}
Rep <- Drive %>% filter(!grepl("Team",Tipo),!grepl("baja",Observación,ignore.case = T))%>%
  left_join(add_count(filter(Base[c("Tutor","Estado","NI-Q")],Estado %in% c("Activo","FUTUROS"),`NI-Q` %in% c("NI 05 SEP","NI 29 AGO")),Tutor) %>% select(Tutor,n)%>%
              unique(),by="Tutor") %>% arrange(n) %>% mutate(n=replace_na(n,0))
#Programa, se toma en cuenta mismo tipo y proyecto
Asig <- filter(Base,`Motivo asig`=="Proyecto",Asignar=="si")
for (i in 1:length(z)) {
  for (j in 1:2) {
    for (k in 1:2) {
      Rep2 <- filter(Asig,`Proyecto asig`==z[i],Tipo==y[j],Turno==x[k])
      if (length(Rep2[[1]])>0) {
        Rep2 <- mutate(Rep2,Tutor=rep_len(filter(Rep[c("Tutor","Proyecto","Tipo","Turno")],Proyecto==z[i],Tipo==y[j],Turno==x[k]) %>% .[[1]],length(Rep2[[1]])))
        Asig <- filter(Asig,`Proyecto asig`!=z[i] | Tipo!=y[j] | Turno!=x[k]); Asig <- rbind(Asig,Rep2)
      }
    }
  }
}
if (length(Asig[[1]])>0) {
  Base <- filter(Base,!MATPROG %in% Asig$MATPROG); Base <- rbind(Base,Asig)
}
Rep <- Drive %>% filter(!grepl("Team",Tipo),!grepl("baja",Observación,ignore.case = T))%>%
  left_join(add_count(filter(Base[c("Tutor","Estado","NI-Q")],Estado %in% c("Activo","FUTUROS"),`NI-Q` %in% c("NI 05 SEP","NI 29 AGO")),Tutor) %>% select(Tutor,n)%>%
              unique(),by="Tutor") %>% arrange(n) %>% mutate(n=replace_na(n,0))
#Avanzados, se toma en cuenta el mismo turno y proyecto
Asig <- filter(Base,`Motivo asig`=="Avanzados",Asignar=="si")#; x <- c("Matutino","Vespertino"); y <- c("LATAM","UTEL")
for (i in 1:2) {
  for (j in 1:length(z)) {
    Rep2 <- filter(Asig,Turno==x[i],Programas==z[j])
    if (length(Rep2[[1]])>0) {
      Rep2 <- mutate(Rep2,Tutor=rep_len(filter(Rep[c("Tutor","Tipo","Turno","Proyecto")],Tipo=="Avanzado",Turno==x[i],Proyecto==z[j]) %>% .[[1]],length(Rep2[[1]])))
      Asig <- filter(Asig,Turno!=x[i] | Programas!=z[j]); Asig <- rbind(Asig,Rep2)
    }
  }
}
if (length(Asig[[1]])>0) {
  Base <- filter(Base,!MATPROG %in% Asig$MATPROG); Base <- rbind(Base,Asig)
}
# Rep <- Drive %>% filter(!grepl("Team",Tipo),!grepl("baja",Observación,ignore.case = T))%>%
#   left_join(add_count(filter(Base[c("Tutor","Estado","NI-Q")],Estado %in% c("Activo","FUTUROS"),`NI-Q` %in% c("NI 05 SEP","NI 29 AGO")),Tutor) %>% select(Tutor,n)%>%
#               unique(),by="Tutor") %>% arrange(n) %>% mutate(n=replace_na(n,0))
remove(Asig,Rep2)
#####--------------Google sheets--------------#####
Rep2 <- Drive %>% select("Gestor A"=Tutor,`Correo Tutor`,`Correo Super`) %>% mutate(`Gestor N`=`Gestor A`) %>% unique()
Rep <- filter(Base,Asignar=="si") %>% select(Matricula,`Clave Programa`,"Gestor A"=`Tutor Anterior`,"Gestor N"=Tutor,"Razón"=`Motivo asig`,"Enlace CRM"=`LINK CRM`)%>%
  mutate(Razón=if_else(!is.na(Razón),
                       if_else(Razón=="Turno","Cambio de turno",
                               if_else(Razón=="Proyecto","Cambio de proyecto","Cambio a avanzados")),"Gestor específico"))%>%
  left_join(Rep2[1:3],by="Gestor A") %>% left_join(Rep2[c(4,2,3)],by="Gestor N") %>% unite(Enviar,`Correo Tutor.x`,`Correo Tutor.y`,sep=",",na.rm = T)%>%
  unite(CC,`Correo Super.x`,`Correo Super.y`,sep=",",na.rm = T) %>% mutate(`Fecha Reasignación`=as.character(Sys.Date())) %>% mutate(Base="Posgrados")
escribir(Rep,c("1dEeJqXn944LaVSvDGAq2awfaQdDDhHveWmDGCqFcb_c","gsheet_Correos.ID"))
#Imprimiendo asig nueva
Base <- Base[c(noms,"Asignar")]
Rep <- filter(Base,Asignar=="si")
Rep2 <- leer(c("1dEeJqXn944LaVSvDGAq2awfaQdDDhHveWmDGCqFcb_c","gsheet.ID"),sheet="Asignación",col_select=c("Tutor","Tur"="Turno","Sup"="Supervisor")) %>% filter(!duplicated(Tutor))
Rep <- left_join(Rep,Rep2,by="Tutor") %>% mutate(Turno=Tur,Supervisor=Sup) %>% select(-Tur,-Sup)
Base <- filter(Base,!MATPROG %in% Rep$MATPROG); Base <- rbind(Base,Rep); Base <- Base[noms]
escribir(Base,c("Reasignación Posgrados.csv","Principales$"))






# Rep <- filter(Base,Matricula %in% c("010442920")) %>% select(Matricula,`Clave Programa`,`Motivo asig`,Tutor,`Tutor Anterior`,t1,t2,t3,TipTut,Turno,`Turno asig`,Programas,`Proyecto asig`,Asignar)
# Rep <- filter(Base,Asignar=="no") %>% select(Matricula,`Clave Programa`,`Motivo asig`,Tutor,`Tutor Anterior`,t1,t2,t3,TipTut,Turno,`Turno asig`,Programas,`Proyecto asig`,Asignar)
























