library(MultiLEDS); library(dplyr); library(tidyr); library(lubridate)
#Inicio
diremail("D:/Trabajo","jsalinba@utel.edu.mx")

Base <- leer(c("Histórico Decisiones","SIR$")) %>% mutate(`Clave Programa`=substring(PROGRAMA,1,10),PROGRAMA=substring(PROGRAMA,12))
x <- c("ESTATUS_CODE","NIVEL","CAMPUS","CANAL_FINAL"); y <- c("Estatus","Niveles","Campus","Campus Canales")
Rep <- leer(c("1OQENajkU5Z4L7MS7FNhrvrMBS1iT-EOc5oiON9FXcns","gsheet.ID"),sheet="General")
for (i in 1:length(x)) {
  Rep2 <- extr_secc(Rep,y[i])
  if (hasName(Rep2,"Fecha Campus")) Rep2 <- select(Rep2,-`Fecha Campus`)
  names(Rep2)[1] <- x[i]
  Base <- left_join(Base,Rep2,by=x[i])
}
Rep <- leer(c("Info General","gsheet"),sheet="Validación",secc="Usuarios Decisión") %>% rename("USUARIO_DECISION"=Usuario)
Base <- left_join(Base,Rep,by="USUARIO_DECISION")

Base <- unite(Base,MATPROG,MATRICULA,`Clave Programa`,sep="",remove=F,na.rm=T) %>% mutate_at(grep("FECHA",names(.)),~dmy(.)) %>% group_by(MATPROG)%>%
  arrange(desc(FECHA_DES),desc(FECHA_ACTIVIDAD),desc(FECHA_CREACION)) %>% ungroup()
Base <- mutate(Base,`Días Asp-Dec`=as.integer(FECHA_DES-FECHA_ACTIVIDAD), `Tiempo Decisión`= if_else(`Días Asp-Dec`<2,"0h-24h",
                                                                                                                     if_else(`Días Asp-Dec`<3,"48h","72h o más")),
               `Días Pros-Asp`=as.integer(FECHA_ACTIVIDAD-FECHA_CREACION), `Tiempo Aspitante`= if_else(`Días Pros-Asp`<2,"0h-24h",
                                                                                                                  if_else(`Días Pros-Asp`<3,"48h","72h o más")),
               `Ult Act`="_")
Base$`Ult Act`[1] <- as.character(now())
Base <- add_count(Base,MATPROG,name = "No. Decisiones")
Base <- mutate_all(Base,~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
#Ordenando e imprimiendo
Rep <- leer(c("1OQENajkU5Z4L7MS7FNhrvrMBS1iT-EOc5oiON9FXcns","gsheet.ID"),sheet="Orden Base",secc="Decisiones")
Base <- `colnames<-`(Base,Rep$Nuevo) %>% .[Rep$Incluir[!is.na(Rep$Incluir)]]
escribir(Base,c("Decisiones.csv","Principales$"))











