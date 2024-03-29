library(MultiLEDS); library(lubridate); library(dplyr); library(tidyr)
#Variables principales
diremail("C:/Users/Jsalinba/Documents/Reportes","jsalinba@utel.edu.mx")
h1 <- hm("08:30")
h2 <- hm(paste(hour(now()),minute(now()),sep = ":"))
shell("C:/Users/Jsalinba/Documents/SCRIPTS/Python/Validacion.py")#Script py
#####-------------------------Leyendo base anterior y cruzando-------------------------#####
Rep <- leer(c("Validaci�n","Principales$"),col_select = c("MATPROG","Responsable"))
Base <- leer(c("Inscritos_NI_RI","SIR$")) %>% mutate_at(grep("FECHA",names(.)),~dmy(.))%>%
  arrange(desc(FECHA_DECISION),desc(FECHAINICIO),desc(FECHA_ACTIVIDAD),desc(FECHA_REGISTRO)) %>% mutate(`Clave Programa`=substring(PROGRAMA,1,10),PROGRAMA=substring(PROGRAMA,12))%>%
  unite(MATPROG,MATRICULA,`Clave Programa`,sep = "",na.rm = T,remove = F) %>% filter(!duplicated(MATPROG))
#Se genera la validaci�n �nicamente si no existe p�rdida de datos.
if (length(rownames(Base))>=length(rownames(Rep))) {
  Base <- left_join(Base,Rep,by="MATPROG") %>% mutate(Responsable=if_else(is.na(Responsable),"N",Responsable))
  #####-------------------------Asignando nuevos-------------------------#####
  Rep <- filter(Base,Responsable=="N")
  if (length(rownames(Rep))>0) {
    cont <- count(Base,Responsable)
    x <- leer(c("Info General","gsheet"),sheet="Validaci�n",secc="Responsables") %>% left_join(cont,by="Responsable") %>% arrange(n) %>% .[["Responsable"]]
    Rep <- mutate(Rep,Responsable=rep_len(x,length(rownames(Rep))))
    Base <- filter(Base,Responsable!="N") %>% rbind(Rep); remove(x,cont)
  }
  #####-------------------------Info General-------------------------#####
  x <- c("ESTATUS_CODE","NIVEL","DECISION","CAMPUS","CANAL_FINAL"); y <- c("Estatus","Niveles","Decisiones","Campus","Campus Canales")
  Rep <- leer(c("1OQENajkU5Z4L7MS7FNhrvrMBS1iT-EOc5oiON9FXcns","gsheet.ID"),sheet = "General")
  for (i in 1:length(x)) {
    Rep2 <- extr_secc(Rep,y[i])
    if (hasName(Rep2,"Fecha Campus")) {
      Rep2 <- select(Rep2,-`Fecha Campus`)
    }
    names(Rep2)[1] <- x[i]; Base <- left_join(Base,Rep2,by=x[i])
  }
  Rep <- leer(c("1OQENajkU5Z4L7MS7FNhrvrMBS1iT-EOc5oiON9FXcns","gsheet.ID"),sheet = "Validaci�n",secc = "Usuarios Decisi�n") %>% rename("USUARIO_DECISION"=Usuario)
  Base <- left_join(Base,Rep,by="USUARIO_DECISION")
  #####-------------------------Por trabajar, d�as y adicionales-------------------------#####
  Base <- mutate_at(Base,c("Nombre","Equipo"),~if_else(is.na(.),"Ventas",.))
  Base <- mutate(Base,Nombre=if_else(Nombre=="Sistemas","Por validar",Nombre),
                 Acci�n=if_else(Decisi�n=="Por validar",
                                if_else(CANAL_FINAL=="B12"|CANAL_FINAL=="CONVERTIA"|CANAL_FINAL=="MAD TORO MEDIA"|CANAL_FINAL=="RESPONSABILIDAD SOCIAL","Prioridad",
                                        if_else(CANAL_FINAL=="REINGRESOS"|TIPO=="FUTURO"|TIPO=="NUEVO INGRESO"|TIPO=="REINGRESO","Trabajar","_")),"_"),
                 DECISION=if_else(is.na(DECISION),"Por validar",DECISION),
                 D�as=if_else(!is.na(FECHA_ACTIVIDAD),if_else(Decisi�n=="Por validar",
                                                              as.integer(as.Date(now())-FECHA_ACTIVIDAD),-1L),-2L),
                 `Ultima Actualizaci�n`="_")
  Base$`Ultima Actualizaci�n`[1] <- as.character(now())
  #####-------------------------Rechazos-------------------------#####
  Base$`Motivo Rechazo` <- "_"
  Rep <- filter(Base,Decisi�n=="Rechazado")
  Rep2 <- leer(c("Info interna","Reportes$"))
  for (i in 1:length(rownames(Rep2))) {
    Rep <- mutate(Rep,`Motivo Rechazo`=if_else(`Motivo Rechazo`=="_",
                                               if_else(grepl(Rep2[[i,1]],COMENTARIO,ignore.case = T),Rep2[[i,2]],"_"),`Motivo Rechazo`))
  }
  Base <- filter(Base,Decisi�n!="Rechazado") %>% rbind(Rep); remove(Rep2)
  #####-------------------------Re ordenando e imprimiendo-------------------------#####
  Rep <- leer(c("Info General","gsheet"),sheet="Orden Base",secc="Validaci�n")
  Base <- `colnames<-`(Base,Rep$Nuevo) %>% .[c(Rep$Incluir[!is.na(Rep$Incluir)])]
  Base <- mutate_all(Base,~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
  escribir(Base,c("Validaci�n.csv","Principales$"))
  if (h1>h2){
    op_arch("cop",c("Validaci�n_Aux","Validaci�n_C1","auxiliares$","Anteriores$"))
    op_arch("cop",c("Validaci�n","Validaci�n_Aux","Principales$","auxiliares$"))
  }
}else{print("La base anterior tiene m�s registros que la actual...hay p�rdida de datos.")}



















