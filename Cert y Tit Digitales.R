library(MultiLEDS); library(dplyr); library(tidyr); library(lubridate)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#####--------------------------------Inicio--------------------------------#####
Base <- leer(c("Estatus_General_Alumno","SIR$"),
             col_select=c("Matrícula"=MATRICULA,"Clave Campus"=CAMPUS,"Clave Nivel"=NIVEL,"Programa"=PROGRAMA,"Clave Estatus"=ESTATUS_CODE,"Nombre"=ESTUDIANTE,
                          "Fecha Estatus"=FECHA_ESTATUS,"Fecha Primer Inscripción"=PRIMER_INSCRIPCION_SIN_ESTATUS))%>%
  mutate(`Clave Programa`=substring(Programa,1,10),Programa=substring(Programa,12)) %>% unite(MATPROG,Matrícula,`Clave Programa`,sep="",remove=F,na.rm=T) %>%
  filter(`Clave Estatus`=="EG" & (`Clave Nivel`=="MS" | `Clave Nivel`=="MA"))
Rep <- leer(c("Info General","gsheet"),sheet="General")
x <- c(Estatus="Clave Estatus",Niveles="Clave Nivel",Campus="Clave Campus")
for (i in 1:length(x)) {
  Rep2 <- extr_secc(Rep,names(x)[i])
  Base <- left_join(Base,Rep2,by=x[[i]])
}
Base <- select(Base,-`Fecha Campus`)
Rep <- leer(c("Cert y Tit Digitales","SIR$"),col_select=c("Matrícula"=MATRICULA,"Clave Programa"=CVE_PROGRAMA,"Tipo"=TIPO))%>%
  mutate(Tipo=if_else(Tipo=="Ti??tulo","Título","Certificado")) %>% unite(MATPROG,Matrícula,`Clave Programa`,sep="",na.rm=T) %>% arrange(desc(Tipo))%>%
  filter(!duplicated(MATPROG))

Base <- left_join(Base,Rep,by="MATPROG") %>% mutate(Tipo=if_else(is.na(Tipo),"Pendiente",Tipo))
x <- c("MATPROG","Matrícula","Clave Campus","Campus","Tipo Campus","Nivel","Estatus","Tipo Estatus","Clave Programa","Programa","Nombre","Fecha Estatus","Fecha Primer Inscripción",
       "Tipo")
Base <- Base[x]
escribir(Base,c("Cert y Tit Egresados MA y MS.csv","Bases$"))