library(dplyr);library(lubridate);library(tidyr);library(readr);library(readxl);library(MultiLEDS)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
Base <- leer(c("Historial Académico Compactado","SIR$")) %>% unite(Clave,MATRICULA,PROGRAMA,PERIODO_CATALOGO,remove=F) %>% filter(!duplicated(Clave))
#Campus
Rep <- leer(c("Info General","gsheet"),sheet="General")
Rep2 <- extr_secc(Rep,"Campus") %>% select(-`Fecha Campus`) %>% rename("CAMPUS"=`Clave Campus`)
Base <- left_join(Base,Rep2,by="CAMPUS")
#Niveles
Rep2 <- extr_secc(Rep,"Niveles") %>% rename("NIVEL"=`Clave Nivel`)
Base <- left_join(Base,Rep2,by="NIVEL")
#Dividiendo base (Doctorado y los demás)
Rep <- filter(Base,NIVEL=="DO") %>% select(-`Regla Negocio`)
Base <- filter(Base,NIVEL!="DO") %>% select(-`Regla Negocio`)
#Niveles sin doctorado, son bimestrales.
Base <- mutate_at(Base,10:14,~as.integer(.))%>%
  mutate(`Ene-Feb`= APROBADAS+EN_CURSO,
         `Mar-Abr`= `Ene-Feb`+EN_CURSO,
         `May-Jun`= `Mar-Abr`+EN_CURSO,
         `Jul-Ago`= `May-Jun`+EN_CURSO,
         `Sep-Oct`= `Jul-Ago`+EN_CURSO,
         `Nov-Dic`= `Sep-Oct`+EN_CURSO,
         `Ene-Feb 2024`= `Nov-Dic`+EN_CURSO)%>%
  mutate_at(19:length(.),~if_else(.>=TOTAL,.-(.-TOTAL),0L))

x <- names(Base)[19:length(Base)]
Base <- mutate(Base,Egreso="_")
for (i in length(x):1) {
  Base <- mutate(Base,Egreso=if_else(Base[x[i]]!=0,x[i],Egreso))
}

Base <- Base[c(names(Base)[1:18],"Egreso")]
#Nivel doctorado,n son cuatrimestrales
Rep <- mutate_at(Rep,10:14,~as.integer(.))%>%
  mutate(`Ene-Abr`= APROBADAS+EN_CURSO,
         `May-Ago`= `Ene-Abr`+EN_CURSO,
         `Sep-Dic`= `May-Ago`+EN_CURSO,
         `Ene-Abr 2024`= `Sep-Dic`+EN_CURSO)%>%
  mutate_at(19:length(.),~if_else(.>=TOTAL,.-(.-TOTAL),0L))

x <- names(Rep)[19:length(Rep)]
Rep <- mutate(Rep,Egreso="_")
for (i in length(x):1) {
  Rep <- mutate(Rep,Egreso=if_else(Rep[x[i]]!=0,x[i],Egreso))
}
Rep <- Rep[c(names(Rep)[1:18],"Egreso")]
#Uniendo ambas bases divididas y separando año
Base <- rbind(Base,Rep)
Base <- mutate(Base,Año=if_else(Egreso!="_",
                                if_else(grepl(" 2024",Egreso),2024L,2023L),0L)) %>% mutate(Egreso=gsub(" 2024| 2025","",Egreso))
#Reordenando, renombrando, tipificando orden de los bimestres e imprimiendo.
Base <- select(Base,Clave,"Matrícula"=MATRICULA,"Nombre"=NOMBRE,"Clave Programa"=PROGRAMA,"Programa"=NOMBRE_PROGRAMA,"Periodo Catalogo"=PERIODO_CATALOGO,"Clave Campus"=CAMPUS,
               Campus,`Tipo Campus`,Nivel,"Estatus"=ESTATUS,"Avance"=AVANCE,Egreso,Año,"En Curso"=EN_CURSO,"Por Cursar"=POR_CURSAR,"Total"=TOTAL) %>% mutate_all(~as.character(.))%>%
  mutate_all(~replace_na(.,"_"))
Base <- mutate(Base,Orden=if_else(Egreso=="Ene-Feb",1L,
                                  if_else(Egreso=="Mar-Abr",2L,
                                          if_else(Egreso=="Ene-Abr",3L,
                                                  if_else(Egreso=="May-Jun",4L,
                                                          if_else(Egreso=="Jul-Ago",5L,
                                                                  if_else(Egreso=="May-Ago",6L,
                                                                          if_else(Egreso=="Sep-Oct",7L,
                                                                                  if_else(Egreso=="Nov-Dic",8L,
                                                                                          if_else(Egreso=="Sep-Dic",9L,10L))))))))))
escribir(Base,c("Proyección Egresos.csv","Principales$"))








