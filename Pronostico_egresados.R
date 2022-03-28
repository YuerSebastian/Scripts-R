library(dplyr);library(lubridate);library(tidyr);library(readr);library(readxl)
c <- list.dirs("D:/Trabajo")
#Funciones
leer_arch <- function(c,tipo="csv",arch=NULL,pat=NULL,...){
  #Resume la lectura que se usa siempre para los reportes, se pueden usar opciones adicionales de las funciones internas.
  if (tipo=="csv") {
    x <- read_csv(paste(c[grep(pat,c)],paste(arch,tipo,sep = "."),sep = "/"),locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c"),...)
  }else if (tipo=="xlsx"){
    x <- read_excel(paste(c[grep(pat,c)],paste(arch,tipo,sep = "."),sep = "/"),col_types = "text",...)
  }else if (tipo=="txt"){
    x <- read_delim(paste(c[grep(pat,c)],paste(arch,tipo,sep = "."),sep = "/"),locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c"),...)
  }else if (tipo=="gsheet"){
    x <- range_speedread(c[length(c)],col_types = cols(.default = "c"),...)
  }else{x <- "Tipo incorrecto."}
  return(x)
}
impr_arch <- function(c,x,tipo="csv",arch=NULL,pat=NULL,...){
  write.csv(x,paste(c[grep(pat,c)],paste(arch,tipo,sep="."),sep="/"),row.names = F,...)
}
Base <- leer_arch(c,"csv","Historial_Academico_Compactado","Escolares$") %>% filter(ESTATUS=="MATRICULADO")
#Campus
Rep <- leer_arch(c,"xlsx","Info general","Trabajo$",sheet="Campus") %>% select(-`Fecha Campus`) %>% rename("CAMPUS"=Campus)
Base <- left_join(Base,Rep,by="CAMPUS")
Rep <- leer_arch(c,"xlsx","Info general","Trabajo$",sheet="Niveles") %>% rename("NIVEL"=Nivel)
Base <- left_join(Base,Rep,by="NIVEL")
#Inicio
#Dividiendo base
Rep <- filter(Base,NIVEL=="DO")
Base <- filter(Base,NIVEL!="DO")
#Niveles sin doctorado
Base <- mutate_at(Base,8:12,~as.integer(.))%>%
  mutate(`Ene-Feb`= APROBADAS+EN_CURSO,
         `Mar-Abr`= `Ene-Feb`+EN_CURSO,
         `May-Jun`= `Mar-Abr`+EN_CURSO,
         `Jul-Ago`= `May-Jun`+EN_CURSO,
         `Sep-Oct`= `Jul-Ago`+EN_CURSO,
         `Nov-Dic`= `Sep-Oct`+EN_CURSO,
         `Ene-Feb 2023`= `Nov-Dic`+EN_CURSO,
         `Mar-Abr 2023`= `Ene-Feb 2023`+EN_CURSO,
         `May-Jun 2023`= `Mar-Abr 2023`+EN_CURSO,
         `Jul-Ago 2023`= `May-Jun 2023`+EN_CURSO,
         `Sep-Oct 2023`= `Jul-Ago 2023`+EN_CURSO,
         `Nov-Dic 2023`= `Sep-Oct 2023`+EN_CURSO)%>%
  mutate_at(17:length(.),~if_else(.>=TOTAL,.-(.-TOTAL),0L))

x <- names(Base)[17:length(Base)]
Base <- mutate(Base,Egreso="_")
for (i in length(x):1) {
  Base <- mutate(Base,Egreso=if_else(Base[x[i]]!=0,x[i],Egreso))
}
Base <- Base[c(names(Base)[1:16],"Egreso")]
#Nivel doctorado
Rep <- mutate_at(Rep,8:12,~as.integer(.))%>%
  mutate(`Mar-Abr`= APROBADAS+EN_CURSO,
         `Jul-Ago`= `Mar-Abr`+EN_CURSO,
         `Nov-Dic`= `Jul-Ago`+EN_CURSO,
         `Mar-Abr 2023`= `Nov-Dic`+EN_CURSO,
         `Jul-Ago 2023`= `Mar-Abr 2023`+EN_CURSO,
         `Nov-Dic 2023`= `Jul-Ago 2023`+EN_CURSO)%>%
  mutate_at(17:length(.),~if_else(.>=TOTAL,.-(.-TOTAL),0L))

x <- names(Rep)[17:length(Rep)]
Rep <- mutate(Rep,Egreso="_")
for (i in length(x):1) {
  Rep <- mutate(Rep,Egreso=if_else(Rep[x[i]]!=0,x[i],Egreso))
}
Rep <- Rep[c(names(Rep)[1:16],"Egreso")]
#Uniendo ambas bases divididas y separando año
Base <- rbind(Base,Rep)
Base <- mutate(Base,Año=if_else(Egreso!="_",
                                if_else(grepl(" 2023",Egreso),2023L,2022L),0L)) %>% mutate(Egreso=gsub(" 2023","",Egreso))
#Reordenar e aimprimir
Base <- select(Base,"Matrícula"=MATRICULA,"Nombre"=NOMBRE,"Programa"=PROGRAMA,"Nombre Programa"=NOMBRE_PROGRAMA,"Campus"=CAMPUS,`Descripción Campus`,`Tipo Campus`,
               `Descripción Nivel`,"Avance"=AVANCE,Egreso,Año,"En Curso"=EN_CURSO,"Por Cursar"=POR_CURSAR,"Total"=TOTAL) %>% mutate_all(~as.character(.))%>%
  mutate_all(~replace_na(.,"_"))
Base <- mutate(Base,Orden=if_else(Egreso=="Ene-Feb",1L,
                                  if_else(Egreso=="Mar-Abr",2L,
                                          if_else(Egreso=="May-Jun",3L,
                                                  if_else(Egreso=="Jul-Ago",4L,
                                                          if_else(Egreso=="Sep-Oct",5L,
                                                                  if_else(Egreso=="Nov-Dic",6L,7L)))))))
impr_arch(c,Base,"csv","Proyección Egresos","Principales$")
