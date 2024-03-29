library(MultiLEDS); library(dplyr); library(tidyr); library(lubridate)
#Inicio
diremail("D:/Trabajo","jsalinba@utel.edu.mx")

Base <- leer(c("Historico_Decisiones","SIR$")) %>% mutate(`Clave Programa`=substring(PROGRAMA,1,10),PROGRAMA=substring(PROGRAMA,12))
x <- c("ESTATUS_CODE","NIVEL","CAMPUS","CANAL_FINAL"); y <- c("Estatus","Niveles","Campus","Campus Canales")
Rep <- leer(c("1OQENajkU5Z4L7MS7FNhrvrMBS1iT-EOc5oiON9FXcns","gsheet.ID"),sheet="General")
for (i in 1:length(x)) {
  Rep2 <- extr_secc(Rep,y[i])
  if (hasName(Rep2,"Fecha Campus")) Rep2 <- select(Rep2,-`Fecha Campus`)
  names(Rep2)[1] <- x[i]
  Base <- left_join(Base,Rep2,by=x[i])
}
Rep <- leer(c("Info General","gsheet"),sheet="Validaci�n",secc="Usuarios Decisi�n") %>% rename("USUARIO_DECISION"=Usuario)
Base <- left_join(Base,Rep,by="USUARIO_DECISION")

Base <- unite(Base,MATPROG,MATRICULA,`Clave Programa`,sep="",remove=F,na.rm=T) %>% mutate_at(grep("FECHA",names(.)),~dmy(.)) %>% group_by(MATPROG)%>%
  arrange(desc(FECHA_DECISION),desc(FECHA_ACTIVIDAD),desc(FECHA_CREACION)) %>% as.data.frame()

Base <- mutate(Base,`D�as Asp-Dec`=as.integer(FECHA_DECISION-FECHA_ACTIVIDAD), `Tiempo Decisi�n`= if_else(`D�as Asp-Dec`<2,"0h-24h",
                                                                                                                     if_else(`D�as Asp-Dec`<3,"48h","72h o m�s")),
               `D�as Pros-Asp`=as.integer(FECHA_ACTIVIDAD-FECHA_CREACION), `Tiempo Aspitante`= if_else(`D�as Pros-Asp`<2,"0h-24h",
                                                                                                                  if_else(`D�as Pros-Asp`<3,"48h","72h o m�s")),
               `Ult Act`="_")
Base$`Ult Act`[1] <- as.character(now())
Base <- mutate_all(Base,~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
#Ordenando e imprimiendo
Rep <- leer(c("1OQENajkU5Z4L7MS7FNhrvrMBS1iT-EOc5oiON9FXcns","gsheet.ID"),sheet="Orden Base",secc="Decisiones")
Base <- `colnames<-`(Base,Rep$Nuevo) %>% .[Rep$Incluir[!is.na(Rep$Incluir)]]
escribir(Base,c("Decisiones.csv","Principales$"))


imprimir(Rep,c("cosa.csv","Trabajo$"))


write_csv(Rep,"D:/Trabajo/cosa.csv")
write.table(Rep,"D:/Trabajo/cosa.txt",row.names = F)









