library(MultiLEDS); library(lubridate); library(dplyr); library(tidyr)
#Variables principales
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
h1 <- hm("08:30")
h2 <- hm(paste(hour(now()),minute(now()),sep = ":"))
#####-------------------------Leyendo base anterior y actual-------------------------#####
Rep <- leer(c("Validación","Principales$"),col_select = c("Matrícula","Responsable","Fecha Decisión","Fecha Actividad","Fecha Registro","Fecha Inicio")); Tam <- length(rownames(Rep))
Rep <- Rep %>% mutate_at(grep("Fecha",names(.)),~ymd(.)) %>% arrange(desc(`Fecha Decisión`),desc(`Fecha Actividad`),desc(`Fecha Registro`),desc(`Fecha Inicio`))%>%
  filter(!duplicated(Matrícula)) %>% select("MATRICULA"=Matrícula,Responsable)
Base <- leer(c("Inscritos_NI_RI","SIR$")) %>% mutate_at(grep("FECHA",names(.)),~dmy(.)) %>% mutate(`Clave Programa`=substring(PROGRAMA,1,10),PROGRAMA=substring(PROGRAMA,12))%>%
  unite(MATPROG,MATRICULA,`Clave Programa`,sep = "",na.rm = T,remove = F)
#Se genera la validación únicamente si no existe pérdida de datos.
if (length(rownames(Base))>=Tam) {
  Base <- left_join(Base,Rep,by="MATRICULA") %>% mutate(Responsable=if_else(is.na(Responsable),"N",Responsable))
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
  #####-------------------------Asignando nuevos-------------------------#####
  Rep2 <- leer(c("Info General","gsheet"),sheet="Validación",secc="Asignación"); x <- names(Rep2)[2:length(Rep2)]
  Rep <- filter(Base,Responsable=="N"); cont <- count(Base,Responsable)
  c <- left_join(Rep2,cont,by="Responsable") %>% select(Responsable,n) %>% mutate(n=if_else(is.na(n),0L,n)) %>% arrange(n,na.rm=F) %>% .[[1]]
  #Ordenando nuevos
  #---------------------listo, acomodar------------------------
  Asig <- filter(Rep,CAMPUS==x[[1]])
  Rep3 <- Rep2[c("Responsable",x[[1]])] %>% filter(.[2]!="_")
  cont <- count(Base,Responsable,CAMPUS) %>% filter(CAMPUS==x[[1]])
  Rep3 <- left_join(Rep3,cont[c(1,3)],by="Responsable") %>% arrange(n)
  n <- c[c %in% Rep3$Responsable]; nn <- c[!(c %in% Rep3$Responsable)]

  c <- carrete(c,length(rownames(Asig))) %>% append(nn,0) %>% .[!duplicated(.)]
  
  Asig <- filter(Rep,CAMPUS==x[[2]])
  Rep3 <- Rep2[c("Responsable",x[[2]])] %>% filter(.[2]!="_")
  cont <- count(Base,Responsable,CAMPUS) %>% filter(CAMPUS==x[[2]])
  Rep3 <- left_join(Rep3,cont[c(1,3)],by="Responsable") %>% arrange(n)
  n <- c[c %in% Rep3$Responsable]; nn <- c[!(c %in% Rep3$Responsable)]
  
  c <- carrete(c,length(rownames(Asig))) %>% append(nn,0) %>% .[!duplicated(.)]
  
  Asig <- filter(Rep,CAMPUS==x[[17]])
  Rep3 <- Rep2[c("Responsable",x[[17]])] %>% filter(.[2]!="_")
  cont <- count(Base,Responsable,CAMPUS) %>% filter(CAMPUS==x[[17]])
  Rep3 <- left_join(Rep3,cont[c(1,3)],by="Responsable") %>% arrange(n)
  n <- c[c %in% Rep3$Responsable]; nn <- c[!(c %in% Rep3$Responsable)]
  
  c <- carrete(c,length(rownames(Asig))) %>% append(nn,0) %>% .[!duplicated(.)]
  #---------------------------------------------------#
  
  
  
  
  
  if (length(rownames(Rep))>0) {
    cont <- count(Base,Responsable)
    x <- leer(c("Info General","gsheet"),sheet="Validación",secc="Responsables") %>% left_join(cont,by="Responsable") %>% arrange(n) %>% .[["Responsable"]]
    Rep <- mutate(Rep,Responsable=rep_len(x,length(rownames(Rep))))
    Base <- filter(Base,Responsable!="N") %>% rbind(Rep); remove(x,cont)
  }
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
  Rep2 <- leer(c("Info interna","Trabajo$"))
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





















