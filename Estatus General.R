library(MultiLEDS); library(dplyr); library(tidyr); library(lubridate); library(magrittr)
#Variables
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#####-----------------------Estatus General Alumnos-----------------------#####
Rep <- leer(c("Info General","gsheet"),sheet="Orden Base",secc = "Estatus General Alumnos"); Rep <- Rep$Selección[!is.na(Rep$Selección)]
Base <- leer(c("Estatus General Alumnos","SIR$"),col_select=all_of(Rep)) %>% mutate(`Clave Programa`=substring(PROGRAMA,1,10),PROGRAMA=substring(PROGRAMA,12))%>%
  unite(MATPROG,MATRICULA,`Clave Programa`,na.rm = T,remove = F,sep = "") %>% unite(PROGCAM,`Clave Programa`,CAMPUS,na.rm = T,remove = F)%>%
  mutate_at(grep("FECHA|SIN_ESTATUS",names(.)),~dmy(.)) %>% arrange(desc(FECHA_ESTATUS),desc(PRIMER_INSCRIPCION_SIN_ESTATUS),desc(FECHAINICIO))%>%
  filter(!duplicated(MATPROG),!grepl("prue|test",ESTUDIANTE,ignore.case = T))
#####-----------------------Programas-----------------------#####
Rep <- leer(c("Info General","gsheet"),secc = "Programas") %>% unite(PROGCAM,`Clave Programa`,`Clave Campus`) %>% filter(!duplicated(PROGCAM))%>%
  select(PROGCAM,RVOE,`Programa Oficial`,Experiencia)
Base <- left_join(Base,Rep,by="PROGCAM")
#####-----------------------Generación, SEP, Bimestres y Eficiencia-----------------------#####
Base <- mutate(Base,Generación=if_else(!is.na(PRIMER_INSCRIPCION_SIN_ESTATUS),
                                       if_else(month(PRIMER_INSCRIPCION_SIN_ESTATUS)>5,as.character(year(PRIMER_INSCRIPCION_SIN_ESTATUS)+1),
                                               as.character(year(PRIMER_INSCRIPCION_SIN_ESTATUS))),"0"),Generación=paste("G ",Generación,sep=""),
               SEP=if_else(!is.na(PRIMER_INSCRIPCION_SIN_ESTATUS),
                                     if_else(month(PRIMER_INSCRIPCION_SIN_ESTATUS)==1,paste("Enero",year(PRIMER_INSCRIPCION_SIN_ESTATUS),sep=" "),
                                             if_else(month(PRIMER_INSCRIPCION_SIN_ESTATUS)<6,paste("Mayo",year(PRIMER_INSCRIPCION_SIN_ESTATUS),sep=" "),
                                                     if_else(month(PRIMER_INSCRIPCION_SIN_ESTATUS)<10,paste("Septiembre",year(PRIMER_INSCRIPCION_SIN_ESTATUS),sep=" "),
                                                             paste("Enero",year(PRIMER_INSCRIPCION_SIN_ESTATUS)+1,sep=" ")))),"_"),
               Bimestre=if_else(!is.na(PRIMER_INSCRIPCION_SIN_ESTATUS),
                                if_else(month(PRIMER_INSCRIPCION_SIN_ESTATUS)<3,"Ene-Feb",
                                        if_else(month(PRIMER_INSCRIPCION_SIN_ESTATUS)<5,"Mar-Abr",
                                                if_else(month(PRIMER_INSCRIPCION_SIN_ESTATUS)<7,"May-Jun",
                                                        if_else(month(PRIMER_INSCRIPCION_SIN_ESTATUS)<9,"Jul-Ago",
                                                                if_else(month(PRIMER_INSCRIPCION_SIN_ESTATUS)<11,"Sep-Oct","Nov-Dic"))))),"_"),
               Eficiencia=as.period(gsub("-","",as.period(interval(FECHA_ESTATUS,PRIMER_INSCRIPCION_SIN_ESTATUS)))))%>%
  separate(SEP,into = c("Periodo SEP","Año SEP"),sep = " ",fill = "right") %>% mutate(`Año SEP`=if_else(is.na(`Año SEP`),"0",`Año SEP`))
Rep <- leer(c("Info General","gsheet"),sheet="Otros",secc = "Eficiencia") %>% mutate(Rango=as.period(Rango))
Base$`Tipo Eficiencia` <- "_"; Base$`Tipo Eficiencia 2` <- "_"
for (i in 1:length(rownames(Rep))) {
  Base <- mutate(Base,`Tipo Eficiencia`=if_else(is.na(Eficiencia),"Indefinido",
                                                if_else(`Tipo Eficiencia`=="_",
                                                        if_else(Eficiencia<Rep$Rango[i],Rep$`Tipo Eficiencia`[i],"_"),`Tipo Eficiencia`)))
  Base <- mutate(Base,`Tipo Eficiencia 2`=if_else(is.na(Eficiencia),"Indefinido",
                                                if_else(`Tipo Eficiencia 2`=="_",
                                                        if_else(Eficiencia<Rep$Rango[i],Rep$`Tipo Eficiencia 2`[i],"_"),`Tipo Eficiencia 2`)))
}
x <- c(" 0[ymdHMS]","^1y","^1m","^1d"," 1m"," 1d","y","m","d","0S");y <- c("","1 Año","1 Mes","1 Día"," 1 Mes"," 1 Día"," Años"," Meses"," Días","Mismo Día")
for (i in 1:length(x)) {
  Base <- mutate(Base,Eficiencia=gsub(x[i],y[i],Eficiencia))
}
#####-----------------------Avance Curricular-----------------------#####
Rep <- leer(c("Avance Curricular","SIR$"),col_select=c(MATRICULA,PROGRAMA,"Avance Curricular"=AVANCE_CURRICULAR,"Promedio"=PROMEDIO))%>%
  unite(MATPROG,MATRICULA,PROGRAMA,sep = "") %>% mutate_at(c(2,3),~as.numeric(.))
Rep2 <- leer(c("Info General","gsheet"),sheet="Otros",secc="Promedios") %>% separate(Promedio,into = c("Pr1","Pr2"),sep = " o ")%>%
  separate(Pr1,into = c("Pr1-1","Pr1-2"),sep = " a ") %>% separate(Pr2,into = c("Pr2-1","Pr2-2"),sep = " a ") %>% mutate_at(c(1:4),~as.numeric(.))
Rep$`Tipo Promedio` <- "_"
for (i in 1:length(rownames(Rep2))) {
  Rep <- mutate(Rep,`Tipo Promedio`=if_else(`Tipo Promedio`=="_",
                                            if_else((Promedio>=Rep2[[i,"Pr1-1"]] & Promedio<=Rep2[[i,"Pr1-2"]]) | (Promedio>=Rep2[[i,"Pr2-1"]] & Promedio<=Rep2[[i,"Pr2-2"]]),
                                            Rep2[[i,"Tipo Promedio"]],`Tipo Promedio`),`Tipo Promedio`))
}
Base <- left_join(Base,Rep,by="MATPROG") %>% mutate_at(c("Avance Curricular","Promedio"),~if_else(is.na(.),-1,.)); remove(Rep2)
#####------------------------------------------Solicitudes SSB (Pago Tit)------------------------------------------#####
# Rep <- leer(c("Solicitudes SSB","SIR$")) %>% filter((COD_SERVICIO %in% c("COLF","OBGR","SOTI","EQUI","REVA")) & ESTATUS_SOLC=="PAGADO")%>%
#   select(MATRICULA,"Pago Titulación"=PROGRAMA,"COD"=COD_SERVICIO) %>% mutate(`Pago Titulación`=substring(`Pago Titulación`,1,10)) %>% unite(MATPROG,MATRICULA,`Pago Titulación`,sep="")%>%
#   mutate(`Pago Equi Rev`=if_else(COD %in% c("EQUI","REV"),"Pagado","Sin Pago"),`Pago Titulación`=if_else(!COD %in% c("EQUI","REV"),"Pagado","Sin Pago")) %>% select(-COD) %>% unique()
Rep <- leer(c("Solicitudes SSB","SIR$"),col_select=c(MATRICULA,"COD"=COD_SERVICIO,"Estatus"=ESTATUS_SOLC,PROGRAMA))%>%
  filter((COD %in% c("COLF","OBGR","SOTI","EQUI","REVA")) & Estatus=="PAGADO") %>% mutate(PROGRAMA=substring(PROGRAMA,1,10),
                                                                                          COD=if_else(COD %in% c("EQUI","REVA"),"Pago Equi Reva","Pago Titulación"))%>%
                                                                                            unite(MATPROG,MATRICULA,PROGRAMA,sep="") %>% unique() %>% spread("COD","Estatus")%>%
  mutate_at(2:3,~replace(.,.=="PAGADO","Pagado"))
Base <- left_join(Base,Rep,by="MATPROG") %>% mutate_at(c("Pago Equi Reva","Pago Titulación"),~replace_na(.,"Sin Pago"))
#####-----------------------Servicio Social-----------------------#####
Rep <- leer(c("Avance SS","Principales$"),col_select=c(MATPROG,"Servicio Social"=GLOBAL))%>%
  mutate(`Servicio Social`=if_else(`Servicio Social`=="Alinear/Liberado","Liberado",`Servicio Social`))
Base <- left_join(Base,Rep,by="MATPROG") %>% mutate(`Servicio Social`=if_else(NIVEL=="LI",
                                                                              if_else(is.na(`Servicio Social`),"Pendiente",`Servicio Social`),"_"))
#####-----------------------Documentos Recolección-----------------------#####
drive_sd("des",c("Adicionales y auxiliares/Recolección Documentos.csv","auxiliares$"))
Rep <- leer(c("Recolección Documentos","auxiliares$"),col_select=c(MATRICULA,PROGRAMA,"Estatus Expediente"=`estatus expediente`,"Expediente Digital"=`expediente digital`,
                                                                   "Certificado Digital"=`certificado digital`,"Certificado Físico"=`certificado fisico`,
                                                                   "Fecha Recuperación"=`Fecha recu`,"Documentos Equivalencia"=Equivalencia,FECHA_DECISION,FECHA_INICIO))%>%
  mutate(MATRICULA=if_else(nchar(MATRICULA)<9,paste("0",MATRICULA,sep=""),MATRICULA)) %>% unite(MATPROG,MATRICULA,PROGRAMA,sep="")%>%
  mutate_at(grep("FECHA|Fecha",names(.)),~dmy(.)) %>% arrange(desc(FECHA_DECISION),desc(FECHA_INICIO)) %>% filter(!duplicated(MATPROG)) %>% select(-FECHA_DECISION,-FECHA_INICIO)
Base <- left_join(Base,Rep,by="MATPROG") %>% mutate(`Estatus Expediente`=if_else(is.na(`Estatus Expediente`),"Sin documentos",`Estatus Expediente`),
                                                    `Expediente Digital`=if_else(is.na(`Expediente Digital`),"Incompleto",`Expediente Digital`))
#####-----------------------Cert y Tit Digitales-----------------------#####
Rep <- leer(c("Cert y Tit Digitales","SIR$")) %>% unite(MATPROG,MATRICULA,CVE_PROGRAMA,sep="")%>%
  mutate(TIPO=replace(TIPO,TIPO=="Ti??tulo","Título"),
         `Cert y Tit`=if_else(TIPO=="Título","1Liberado",
                              if_else(ESTATUS=="Generado","2Proceso de Título","3Proceso de Certificado"))) %>% arrange(`Cert y Tit`)%>%
  mutate(`Cert y Tit`=gsub("[1-3]","",`Cert y Tit`),FECHA_CREACION=dmy(FECHA_CREACION)) %>% select(MATPROG,`Cert y Tit`,"Fecha Cert y Tit"=FECHA_CREACION)%>%
  filter(!duplicated(MATPROG))
Base <- left_join(Base,Rep,by="MATPROG") %>% mutate(`Cert y Tit`=if_else(is.na(`Cert y Tit`),"Pendiente",`Cert y Tit`))
#####-----------------------Niveles de Riesgo-----------------------#####
Rep <- leer(c("Retencion","auxiliares$"),col_select=c(Matricula,Clave_Prog,"Nivel Riesgo"=Nivel_Riesgo))%>%
  mutate(Matricula=if_else(nchar(Matricula)<9,paste("0",Matricula,sep=""),Matricula)) %>% unite(MATPROG,Matricula,Clave_Prog,sep="") %>% filter(!duplicated(MATPROG))
Base <- left_join(Base,Rep,by="MATPROG")
#####-----------------------Info General-----------------------#####
Rep <- leer(c("Info General","gsheet"),sheet="General"); x <- c(Estatus="ESTATUS_CODE",Niveles="NIVEL",Campus="CAMPUS")
for (i in 1:length(x)) {
  Rep2 <- extr_secc(Rep,names(x)[i]); names(Rep2)[1] <- x[[i]]; if(hasName(Rep2,"Fecha Campus")) Rep2 <- select(Rep2,-`Fecha Campus`)
  Base <- left_join(Base,Rep2,by=x[[i]])
}
Base <- mutate_all(Base,~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
#####-----------------------Adeudo (Rangos)-----------------------#####
Base %<>% mutate(SALDO=as.numeric(SALDO),SALDO=replace_na(SALDO,0),Adeudo=if_else(`Tipo Estatus`=="Egresados",if_else(SALDO<=0,"No Adeudo","Adeudo"),
                                                                                  if_else(`Tipo Estatus`=="Activos",if_else(SALDO<=3500,"No Adeudo","Adeudo"),"_")))
#####-----------------------Materias por cursar-----------------------#####
Rep <- leer(c("Historial Académico Compactado","SIR$"),col_select=c(MATRICULA,PROGRAMA,"Materias EC"=EN_CURSO,"Materias PC"=POR_CURSAR))%>%
  unite(MATPROG,MATRICULA,PROGRAMA,sep="") %>% filter(!duplicated(MATPROG)) %>% mutate_at(c("Materias PC","Materias EC"),~as.integer(.))%>%
  mutate(`Bimestres PC` = if_else(`Materias EC`!=0,ceiling((`Materias EC` + `Materias PC`)/`Materias EC`),
                                  if_else(`Materias PC`!=0,ceiling((`Materias EC` + `Materias PC`)/2),0)),
         `Tipo Bimestres PC`=if_else(!is.na(`Bimestres PC`),
                                     if_else(`Bimestres PC` == 0,"NI",
                                             if_else(`Bimestres PC` < 2,"Último",
                                                     if_else(`Bimestres PC` < 3,"Penultimo",
                                                             if_else(`Bimestres PC` < 4,"Ante Penultimo",
                                                                     if_else(`Bimestres PC` < 7,"8 Meses a 1 Año",
                                                                             if_else(`Bimestres PC` < 13,"1 Año 2 Meses a 2 Años",
                                                                                     if_else(`Bimestres PC` < 19,"2 Años 2 Meses a 3 Años","Mayor a 3 Años"))))))),"Sin Materias"))%>%
  mutate_at(c("Materias PC","Materias EC","Bimestres PC"),~replace_na(.,-1))
Base <- left_join(Base,Rep,by="MATPROG") %>% mutate_at(c("Materias PC","Materias EC","Bimestres PC"),~replace_na(.,-2))%>%
  mutate(`Tipo Bimestres PC`=replace_na(`Tipo Bimestres PC`,"No Encontrado"),
         `Tipo Bimestres PC`=if_else(`Tipo Bimestres PC`=="NI" & `Tipo Estatus`=="Egresados","Egresado",`Tipo Bimestres PC`),
         `Tipo Bimestres PC`=if_else(`Tipo Bimestres PC`=="No Encontrado" & `Tipo Estatus`=="Prospecto","Prospecto",`Tipo Bimestres PC`))
#####-----------------------Liberaciones Equi y Rev----------------------#####
Rep <- leer(c("Equi y Rev","auxiliares$")) %>% rename("MATRICULA"=MATRICULA,"Estatus Equi Rev"=ESTATUS,"Tipo Equi Rev"=TIPO) %>% unique()%>%
  arrange(desc(`Estatus Equi Rev`))
for (i in 1:8) {
  Rep <- mutate(Rep,MATRICULA=if_else(nchar(MATRICULA)<9,
                                      if_else(nchar(MATRICULA)==7,paste("1",MATRICULA,sep=""),paste("0",MATRICULA,sep="")),MATRICULA))
}
Rep <- unite(Rep,Clave,MATRICULA,`Tipo Equi Rev`) %>% filter(!duplicated(Clave)) %>% separate(Clave,into = c("MATRICULA","Tipo Equi Rev"),sep="_")%>%
  spread("Tipo Equi Rev","Estatus Equi Rev") %>% mutate(x="")
for (i in 4:2) {
  Rep <- mutate(Rep,x=if_else(!is.na(Rep[i]),paste(x,names(Rep[i]),Rep[[i]],sep=" "),x))
}; Rep <- mutate(Rep,`Tipo Equi Rev`=x) %>% select(MATRICULA,`Tipo Equi Rev`)

Base <- left_join(Base,Rep,by="MATRICULA") %>% mutate(`Tipo Equi Rev`=replace_na(`Tipo Equi Rev`,"Sin Documento"))
#####---------------------------Re ordenando e imprimiendo---------------------------#####
Base <- mutate_all(Base,~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
Rep <- leer(c("Info General","gsheet"),sheet="Orden Base",secc="Estatus General Alumnos")
Base <- `colnames<-`(Base,Rep$Nuevo) %>% .[c(Rep$Incluir[!is.na(Rep$Incluir)])]
escribir(Base,c("Estatus General Alumnos.csv","Principales$"))





















