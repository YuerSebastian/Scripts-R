library(MultiLEDS); library(lubridate); library(dplyr); library(tidyr)
#Variables principales
diremail("C:/Users/Jsalinba/Documents/Reportes","jsalinba@utel.edu.mx")
h1 <- hm("08:30")
h2 <- hm(paste(hour(now()),minute(now()),sep = ":"))
shell("C:/Users/Jsalinba/Documents/SCRIPTS/Python/Validacion.py")#Script py
#####-------------------------Leyendo base anterior y cruzando-------------------------#####
Rep <- leer(c("Validación","Principales$"),col_select = c("MATPROG","Responsable"))
Base <- leer("C:/Users/Jsalinba/Documents/SIR Reportes/Validación/Descargas/Inscritos_NI_RI.csv") %>% mutate_at(grep("FECHA",names(.)),~dmy(.))%>%
  arrange(desc(FECHA_DECISION),desc(FECHAINICIO),desc(FECHA_ACTIVIDAD),desc(FECHA_REGISTRO)) %>% mutate(`Clave Programa`=substring(PROGRAMA,1,10),PROGRAMA=substring(PROGRAMA,12))%>%
  unite(MATPROG,MATRICULA,`Clave Programa`,sep = "",na.rm = T,remove = F) %>% filter(!duplicated(MATPROG))
#Se genera la validación únicamente si no existe pérdida de datos.
if (length(rownames(Base))>=length(rownames(Rep))) {
  Base <- left_join(Base,Rep,by="MATPROG") %>% mutate(Responsable=if_else(is.na(Responsable),"N",Responsable))
  #####-------------------------Asignando nuevos-------------------------#####
  Rep <- filter(Base,Responsable=="N")
  if (length(rownames(Rep))>0) {
    cont <- count(Base,Responsable)
    x <- leer(c("Info General","gsheet"),sheet="Validación",secc="Responsables") %>% left_join(cont,by="Responsable") %>% arrange(n) %>% .[["Responsable"]]
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
  Rep <- leer(c("1OQENajkU5Z4L7MS7FNhrvrMBS1iT-EOc5oiON9FXcns","gsheet.ID"),sheet = "Validación",secc = "Usuarios Decisión") %>% rename("USUARIO_DECISION"=Usuario)
  Base <- left_join(Base,Rep,by="USUARIO_DECISION")
  #####-------------------------Por trabajar, días y adicionales-------------------------#####
  Base <- mutate_at(Base,c("Nombre","Equipo"),~if_else(is.na(.),"Ventas",.))
  Base <- mutate(Base,Nombre=if_else(Nombre=="Sistemas","Por validar",Nombre),
                 Acción=if_else(Decisión=="Por validar",
                                if_else(CANAL_FINAL=="B12"|CANAL_FINAL=="CONVERTIA"|CANAL_FINAL=="MAD TORO MEDIA"|CANAL_FINAL=="RESPONSABILIDAD SOCIAL","Prioridad",
                                        if_else(CANAL_FINAL=="REINGRESOS"|TIPO=="FUTURO"|TIPO=="NUEVO INGRESO"|TIPO=="REINGRESO","Trabajar","_")),"_"),
                 DECISION=if_else(is.na(DECISION),"Por validar",DECISION),
                 Días=if_else(!is.na(FECHA_ACTIVIDAD),if_else(Decisión=="Por validar",
                                                              as.integer(as.Date(now())-FECHA_ACTIVIDAD),-1L),-2L),
                 `Ultima Actualización`="_")
  Base$`Ultima Actualización`[1] <- as.character(now())
  #####-------------------------Rechazos-------------------------#####
  Base$`Motivo Rechazo` <- "_"
  Rep <- filter(Base,Decisión=="Rechazado")
  Rep2 <- leer(c("Info interna","Reportes$"))
  for (i in 1:length(rownames(Rep2))) {
    Rep <- mutate(Rep,`Motivo Rechazo`=if_else(`Motivo Rechazo`=="_",
                                               if_else(grepl(Rep2[[i,1]],COMENTARIO,ignore.case = T),Rep2[[i,2]],"_"),`Motivo Rechazo`))
  }
  Base <- filter(Base,Decisión!="Rechazado") %>% rbind(Rep); remove(Rep2)
  #####-------------------------Re ordenando e imprimiendo-------------------------#####
  Rep <- leer(c("Info General","gsheet"),sheet="Orden Base",secc="Validación")
  Base <- `colnames<-`(Base,Rep$Nuevo) %>% .[c(Rep$Incluir[!is.na(Rep$Incluir)])]
  Base <- mutate_all(Base,~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
  escribir(Base,c("Validación.csv","Principales$"))
  if (h1>h2){
    op_arch("cop",c("Validación_Aux","Validación_C1","auxiliares$","Anteriores$"))
    op_arch("cop",c("Validación","Validación_Aux","Principales$","auxiliares$"))
  }
}else{print("La base anterior tiene más registros que la actual...hay pérdida de datos.")}



















