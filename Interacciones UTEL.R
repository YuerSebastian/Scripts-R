library(MultiLEDS); library(dplyr); library(tidyr); library(lubridate)
diremail("C:/Users/jsalinba.SCALA/Documents/Reportes","jsalinba@utel.edu.mx")
#####--------------Tipificaciones--------------#####
drive_sd("des",c("Interacciones_gral.csv","CRM$"))
Rep <- leer(c("Interacciones_gral","CRM$")) %>% mutate(fecha_hora=ymd_hms(fecha_hora))
c <- 1; f1 <- ymd("2023-08-28"); f2 <- f1 + days(6)
while (f1 <= Sys.Date()) {
  Rep2 <- filter(Rep,fecha_hora>=f1 & fecha_hora<=f2) %>% mutate(Semana=paste("Semana ",c,sep = ""))
  if(c==1) Base <- Rep2 else Base <- rbind(Rep2,Base)
  c <- c+1; f1 <- f2 + days(1); f2 <- f1 + days(6)
}
for (i in 1:8) {
  Base <- mutate(Base,matricula=if_else(nchar(matricula)<9,
                                        if_else(nchar(matricula)==7,paste("1",matricula,sep=""),paste("0",matricula,sep="")),matricula))
}
Rep <- leer(c("1OQENajkU5Z4L7MS7FNhrvrMBS1iT-EOc5oiON9FXcns","gsheet.ID"),sheet="Catálogo CRM")
x <- c(Primarios="mtvo_prim_c",Subclass="sub_clas_c")
for (i in 1:length(x)) {
  Rep2 <- extr_secc(Rep,names(x)[i])
  Base <- left_join(Base,Rep2,by=x[[i]])
}; remove(Rep2)
Base <- group_by(Base,matricula) %>% arrange(`Tipo de Contacto`,desc(fecha_hora)) %>% mutate(Importancia=row_number()) %>% ungroup()
#####--------------Estatus General--------------#####
Rep <- leer(c("Estatus General Alumnos","SIR$"),
            col_select=c("matricula"=MATRICULA,"Clave Campus"=CAMPUS,"Clave Nivel"=NIVEL,"Clave Estatus"=`ESTATUS_CODE`,"Fecha Estatus"=FECHA_ESTATUS,
                         "Fecha Inicio"=FECHAINICIO)) %>% mutate_at(grep("Fecha|PISE",names(.)),~dmy(.))%>%
  arrange(desc(`Fecha Estatus`),desc(`Fecha Inicio`)) %>% filter(!duplicated(matricula)) %>% select(matricula,grep("Clave",names(.)))
Base <- left_join(Base,Rep,by="matricula")
#####--------------Info General--------------#####
Rep <- leer(c("Info General","gsheet"),sheet="General")
x <- c("Estatus","Niveles","Campus")
for (nom in x) {
  Rep2 <- extr_secc(Rep,nom)
  Base <- left_join(Base,Rep2,by=names(Rep2)[1])
}; Base <- select(Base,-`Fecha Campus`) %>% mutate_at(c("Estatus","Tipo Estatus"),~if_else(is.na(`Clave Campus`),"_",.))
#####-------------------------Re ordenando e imprimiendo-------------------------#####
Rep <- leer(c("Info General","gsheet"),sheet="Orden Base",secc="Contactos (Sebastian)")
Base <- `colnames<-`(Base,Rep$Nuevo) %>% .[c(Rep$Incluir[!is.na(Rep$Incluir)])]
Base <- mutate_all(Base,~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
escribir(Base,c("Interacciónes UTEL.csv","Principales$"))
















