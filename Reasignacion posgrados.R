library(MultiLEDS); library(dplyr); library(tidyr); library(googlesheets4); library(lubridate)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#####--------------Inicio--------------#####
#Leyendo base
Base <- leer(c("AsignacionPosgrados","Trabajo$"),sheet="Asignacion",skip=1) %>% mutate(Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula))%>%
  unite(MATPROG,Matricula,`Clave Programa`,sep="",remove=F)
#Leyendo respuestas y cruzando.
Rep <- leer(c("1dEeJqXn944LaVSvDGAq2awfaQdDDhHveWmDGCqFcb_c","gsheet.ID"),sheet="Reasignación") %>% rename("Fecha"=`Marca temporal`,"Tipo asig"=`Tipo de re asignación`,"Motivo asig"=`Motivo de cambio`,
                                                                                "Turno asig"=Turno,"Proyecto asig"=Proyecto)%>%
  mutate(Matrícula=if_else(nchar(Matrícula)<9,paste("0",Matrícula,sep=""),Matrícula),Fecha=dmy_hms(Fecha)) %>% unite(MATPROG,Matrícula,`Clave del programa`,sep="")%>%
  arrange(desc(Fecha)) %>% filter(!duplicated(MATPROG)) %>% select(2:7)
Base <- left_join(Base,Rep,by="MATPROG")
#Identificando que asignar
Base <- mutate(Base,Asignar=if_else(`Tipo asig`=="Re asignación",
                                    if_else(`Motivo asig`=="Turno",if_else(Turno!=`Turno asig`,"si","_"),
                                            if_else(`Motivo asig`=="Proyecto",if_else(Programas!=`Proyecto asig`,"si","_"),
                                                    if_else(Tipo!=`Tipo asig`,"si","_"))) #Esta parte puede no ser necesaria
                                    ,if_else(Tutor!=Gestor,"si","_")),Asignar=replace_na(Asignar,"_"),
               `Tutor Anterior`=if_else(Asignar=="si",Tutor,`Tutor Anterior`))
#####--------------Asignación--------------#####
Rep <- leer(c("Ejemplos","Trabajo$"),sheet="Tutores")%>%
  left_join(add_count(filter(Base[c("Tutor","Estado","NI-Q")],Estado %in% c("Activo","FUTUROS"),`NI-Q` %in% c("NI 29 AGO","NI 01 AGO")),Tutor) %>% select(Tutor,n)%>%
              unique(),by="Tutor") %>% arrange(n) %>% mutate(n=replace_na(n,0))
#Tutor específico, no se necesitan hacer más filtros.
Base <- mutate(Base,Tutor=if_else(Asignar=="si",
                                  if_else(is.na(`Motivo asig`),Gestor,Tutor),Tutor))
#Turno, se toma en cuenta mismo tipo y proyecto
Asig <- filter(Base,`Motivo asig`=="Turno",Asignar=="si")
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
Base <- filter(Base,!MATPROG %in% Asig$MATPROG); Base <- rbind(Base,Asig)
Rep <- leer(c("Ejemplos","Trabajo$"),sheet="Tutores")%>%
  left_join(add_count(filter(Base[c("Tutor","Estado","NI-Q")],Estado %in% c("Activo","FUTUROS"),`NI-Q` %in% c("NI 29 AGO","NI 01 AGO")),Tutor) %>% select(Tutor,n)%>%
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
Base <- filter(Base,!MATPROG %in% Asig$MATPROG); Base <- rbind(Base,Asig)
Rep <- leer(c("Ejemplos","Trabajo$"),sheet="Tutores")%>%
  left_join(add_count(filter(Base[c("Tutor","Estado","NI-Q")],Estado %in% c("Activo","FUTUROS"),`NI-Q` %in% c("NI 29 AGO","NI 01 AGO")),Tutor) %>% select(Tutor,n)%>%
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
Base <- filter(Base,!MATPROG %in% Asig$MATPROG); Base <- rbind(Base,Asig)
Rep <- leer(c("Ejemplos","Trabajo$"),sheet="Tutores")%>%
  left_join(add_count(filter(Base[c("Tutor","Estado","NI-Q")],Estado %in% c("Activo","FUTUROS"),`NI-Q` %in% c("NI 29 AGO","NI 01 AGO")),Tutor) %>% select(Tutor,n)%>%
              unique(),by="Tutor") %>% arrange(n) %>% mutate(n=replace_na(n,0))
remove(Asig,Rep2)
#####--------------Google sheets--------------#####
Rep2 <- leer(c("Ejemplos","Trabajo$"),sheet="Tutores") %>% select("Gestor A"=Tutor,Correo,`Correo Super`) %>% mutate(`Gestor N`=`Gestor A`) %>% unique()
Rep <- filter(Base,Asignar=="si") %>% select(Matricula,"Gestor A"=`Tutor Anterior`,"Gestor N"=Tutor,"Razón"=`Motivo asig`,"Enlace CRM"=`LINK CRM`)%>%
  mutate(Razón=if_else(!is.na(Razón),
                       if_else(Razón=="Turno","Cambio de turno",
                               if_else(Razón=="Proyecto","Cambio de proyecto","Cambio a avanzados")),"Gestor específico"))%>%
  left_join(Rep2[1:3],by="Gestor A") %>% left_join(Rep2[c(4,2,3)],by="Gestor N") %>% unite(Enviar,Correo.x,Correo.y,sep=",",na.rm = T)%>%
  unite(CC,`Correo Super.x`,`Correo Super.y`,sep=",",na.rm = T)
escribir(Rep,c("1dEeJqXn944LaVSvDGAq2awfaQdDDhHveWmDGCqFcb_c","gsheet_Correos.ID"))



































