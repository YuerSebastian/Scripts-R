library(MultiLEDS); library(dplyr); library(tidyr); library(DBI);library(RMySQL); library(lubridate); library(googlesheets4)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#####----------------Descara de archivos----------------#####
fecha1 <- ymd("2022-03-11")
drive_sd("des",c("Interacciones_gral.csv","CRM$"))
while (fecha1 < Sys.Date()){
  drive_sd("des",c(paste("Sabana_",fecha1,".csv",sep=""),"Sabana$"))
  fecha1 <- fecha1 + days(7)
}
#####----------------Inicio Reporte----------------#####
c <- 1; fecha1 <- ymd("2022-03-11"); fecha2 <- fecha1 + days(7)
while (fecha2 < Sys.Date()) {
  #####----------------Sabana----------------#####
  Base <- leer(c(paste("Sabana_",fecha1,sep=""),"Sabana$"))%>%
    mutate(Prioridad=if_else(edo_moodle=="Activo" & nivel_riesgo=="Reprobó" & (stutus=="ADMITIDO" | stutus=="MATRICULADO"),"P","Sin Prioridad"))%>%
    mutate_at(c("calificacion","Semana en curso"),~as.numeric(.))
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
  #drive_sd("des",c("Interacciones_gral.csv","CRM$"))
  Rep <- leer(c("Interacciones_gral","CRM$")) %>% mutate(fecha_hora=ymd_hms(fecha_hora)) %>% arrange(desc(fecha_hora))%>%
    filter(fecha_hora>=fecha1 & fecha_hora<=(fecha2 + days(2)))
  
  Rep2 <- leer(c("Info General","gsheet"),sheet="Catálogo CRM")
  Rep3 <- extr_secc(Rep2,"Primarios")
  Rep <- left_join(Rep,Rep3,by=names(Rep3)[1])
  Rep3 <- extr_secc(Rep2,"Subclass")
  Rep <- left_join(Rep,Rep3,by=names(Rep3)[1])
  remove(Rep2,Rep3)
  
  Rep <- filter(Rep,mtvo_prim_c=="TU_ER" & direction=="Outbound") %>% mutate(matricula=if_else(nchar(matricula)<9,paste("0",matricula,sep=""),matricula))
  cont <- count(Rep,matricula,name = "No. Llamadas")
  Rep <- left_join(Rep,cont,by="matricula")
  
  Rep <- select(Rep,matricula,`No. Llamadas`,`Tipo Contacto Global`) %>% arrange(`Tipo Contacto Global`) %>% filter(!duplicated(matricula))%>%
    mutate(`No. Llamadas`=as.integer(`No. Llamadas`))
  Base <- left_join(Base,Rep,by="matricula") %>% mutate(`No. Llamadas`=if_else(is.na(`No. Llamadas`),0L,`No. Llamadas`))%>%
    mutate(`Tipo Contacto Global`=if_else(`No. Llamadas` == 0,"Sin intento",`Tipo Contacto Global`))
  #####----------------Cruce con Base siguiente----------------#####
  Rep <- leer(c(paste("Sabana_",fecha2,sep=""),"Sabana$"),
              col_select=c("id_us","calif_posterior"=calificacion,"nr_posterior"=nivel_riesgo,"edo_moodle_posterior"=edo_moodle,"estado_posterior"=`estado escolares`))
  Base <- left_join(Base,Rep,by="id_us") %>% mutate(calif_posterior=if_else(is.na(calif_posterior),"0",calif_posterior),calif_posterior=as.numeric(calif_posterior))%>%
    mutate_at(c("nr_posterior","edo_moodle_posterior","estado_posterior"),~if_else(is.na(.),"No encontrado",.))
  Base <- mutate(Base,Cambio=if_else(calif_posterior > calificacion,"Positivo","Sin cambio"),Semana=paste("S",c,sep = ""))
  #####----------------Fin bucle----------------#####
  print(paste("Semana",c,"completa",sep = " "))
  if(c==1) BG <- Base else BG <- rbind(BG,Base)
  c <- c+1
  fecha1 <- fecha2
  fecha2 <- fecha1 + days(7)
};remove(Base,cont)
#####----------------Imprimiendo----------------#####
#escribir(BG,c("Prioridades.csv","Sabana$"))
googledrive::drive_put("D:/Trabajo/Bases/Originales/Sabana/Prioridades.csv","Principales/")

















