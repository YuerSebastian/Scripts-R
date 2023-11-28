library(MultiLEDS); library(dplyr); library(tidyr); library(lubridate); library(googlesheets4)
diremail("C:/Users/jsalinba/Documents/Reportes","jsalinba@utel.edu.mx")
#####----------------Descara de archivos----------------#####
fecha1 <- ymd("2022-02-27")
drive_sd("des",c("correos_docentes.csv","CRM$")); drive_sd("des",c("Interacciones_gral.csv","CRM$"))
while (fecha1 <= Sys.Date()){
  drive_sd("des",c(paste("Niveles/Sabana_",fecha1,".csv",sep=""),"Sabana$"))
  fecha1 <- fecha1 + days(7)
}
#####----------------Interaciiones----------------#####
Reps <- list("llam"=leer(c("Interacciones_gral","CRM$")) %>% filter(mtvo_prim_c=="TU_ER" & direction=="Outbound") %>%
               mutate(fecha_hora=ymd_hms(fecha_hora)) %>% arrange(desc(fecha_hora)) %>% mutate(matricula=if_else(nchar(matricula)<9,paste("0",matricula,sep=""),matricula)),
             "corr"=leer(c("correos_docentes","CRM$")) %>% filter(grepl("Lo que necesitas",Titulo,ignore.case = T)) %>%
               mutate(Fecha_Envio=ymd_hm(Fecha_Envio)) %>% arrange(desc(Fecha_Envio)) %>% mutate(matricula=if_else(nchar(matricula)<9,paste("0",matricula,sep=""),matricula))%>%
               arrange(status_lectura) %>% separate(clave,into = c("ciclo","num","inicio_plus","materia"),sep = "_") %>% unite(clave_corr,matricula,materia,sep = "",na.rm = T)%>%
               select(clave_corr,Fecha_Envio,status_lectura))
#Llamadas
Rep <- leer(c("Info General","gsheet"),sheet="Catálogo CRM"); x <- c("Primarios","Subclass")
for (nom in x) {
  Rep2 <- extr_secc(Rep,nom)
  Reps[["llam"]] <- left_join(Reps[["llam"]],Rep2,by=names(Rep2)[1])
};remove(Rep2)
Reps[["llam"]] <- arrange(Reps[["llam"]],`Tipo Contacto Global`) %>% select(matricula,fecha_hora,`Tipo Contacto Global`)
#####----------------Inicio Reporte----------------#####
c <- 1; fecha1 <- ymd("2022-02-27"); fecha2 <- fecha1 + days(7)
while (fecha1 <= Sys.Date()) {
  #####----------------Sabana----------------#####
  Base <- leer(c(paste("Sabana_",fecha1,sep=""),"Sabana$"))%>%
    mutate(Prioridad=if_else(edo_moodle=="Activo" & nivel_riesgo=="Reprobó" & (stutus=="ADMITIDO" | stutus=="MATRICULADO"),"P","Sin Prioridad"))%>%
    mutate_at(c("calificacion","Semana en curso"),~as.numeric(.)) %>% mutate(matricula=if_else(nchar(matricula)<9,paste("0",matricula,sep=""),matricula))
  #####----------------Prioridades----------------#####
  Rep <- leer(c("Info General","gsheet"),sheet="Otros",secc="Prioridades Aula")
  for (i in 1:(length(Rep)-1)) {
    Rep2 <- Rep[c(i,length(Rep))] %>% filter(.[1]!="_") %>% separate(1,into = c("C1","C2"),sep = " a ") %>% mutate_at(c(1,2),~as.numeric(.))
    for (j in 1:length(rownames(Rep2))) {
      Base <- mutate(Base,Prioridad=if_else(Prioridad=="P",
                                            if_else(calificacion >= Rep2[[j,1]] & calificacion <= Rep2[[j,2]] & `Semana en curso`==i,Rep2[[j,3]],"P"),Prioridad))
    }
  }; remove(Rep2)
  #####----------------Contactos CRM----------------#####
  Rep <- filter(Reps[["llam"]],fecha_hora>=(fecha1 + days(3)) & fecha_hora<=(fecha2 + days(2)))
  cont <- count(Rep,matricula,name = "No. Llamadas")
  Rep <- left_join(Rep,cont,by="matricula") %>% filter(!duplicated(matricula)) %>% mutate(`No. Llamadas`=as.integer(`No. Llamadas`)) %>% select(-fecha_hora)
  Base <- left_join(Base,Rep,by="matricula") %>% mutate(`No. Llamadas`=if_else(is.na(`No. Llamadas`),0L,`No. Llamadas`))%>%
    mutate(`Tipo Contacto Global`=if_else(`No. Llamadas` == 0,"Sin intento",`Tipo Contacto Global`))
  #####----------------Correos CRM----------------#####
  Base <- unite(Base,clave_corr,matricula,clave_materia,sep="",na.rm=T,remove=F)
  Rep <- filter(Reps[["corr"]],Fecha_Envio>=(fecha1 + days(3)) & Fecha_Envio<=(fecha2 + days(2)))
  cont <- count(Rep,clave_corr,name = "No. Correos")
  Rep <- left_join(Rep,cont,by="clave_corr") %>% filter(!duplicated(clave_corr)) %>% mutate(`No. Correos`=as.integer(`No. Correos`)) %>% select(-Fecha_Envio)
  Base <- left_join(Base,Rep,by="clave_corr") %>% mutate(`No. Correos`=if_else(is.na(`No. Correos`),0L,`No. Correos`))%>%
    mutate(status_lectura=if_else(`No. Correos` == 0,"No enviado",status_lectura)) %>% select(-clave_corr)
  #####----------------Cruce con Base siguiente----------------#####
  if (file.exists(paste("C:/Users/Jsalinba/Documents/Reportes/Bases/Originales/Sabana/Sabana_",fecha2,".csv",sep=""))) {
    Rep <- leer(c(paste("Sabana_",fecha2,sep=""),"Sabana$"),
                col_select=c("id_us","calif_posterior"=calificacion,"nr_posterior"=nivel_riesgo,"edo_moodle_posterior"=edo_moodle,"estado_posterior"=`estado escolares`))
    Base <- left_join(Base,Rep,by="id_us") %>% mutate(calif_posterior=if_else(is.na(calif_posterior),"0",calif_posterior),calif_posterior=as.numeric(calif_posterior))%>%
      mutate_at(c("nr_posterior","edo_moodle_posterior","estado_posterior"),~if_else(is.na(.),"No encontrado",.))
    Base <- mutate(Base,Cambio=if_else(calif_posterior > calificacion,"Realizó actividad","Sin actividad"),Semana=paste("S",c,sep = ""))
  }else{
    Base <- mutate(Base,calif_posterior=0,nr_posterior="Sin definir",edo_moodle_posterior="Sin definir",estado_posterior="Sin definir",Cambio="Sin definir",
                   Semana=paste("S",c,sep = ""))
  }
  #####----------------Fin bucle----------------#####
  print(paste("Semana",c,"completa",sep = " "))
  if(hasName(Base,"links")) Base <- select(Base,-links)
  if(c==1) BG <- Base else BG <- rbind(BG,Base)
  c <- c+1
  fecha1 <- fecha2
  fecha2 <- fecha1 + days(7)
};remove(Base,cont)
#####----------------Imprimiendo----------------#####
escribir(BG,c("Prioridades.csv","Sabana$"))
googledrive::drive_put("C:/Users/jsalinba/Documents/Reportes/Bases/Originales/Sabana/Prioridades.csv","Principales/")



















