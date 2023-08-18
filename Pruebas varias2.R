library(MultiLEDS); library(dplyr); library(tidyr); library(lubridate); library(googlesheets4)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#####----------------Descarga de archivos----------------#####
F1 <- ymd("2023-03-03")
drive_sd("des",c("Reports/correos_docentes.csv","CRM$")); drive_sd("des",c("Contactacion/Interacciones_gral.csv","CRM$"))
while (F1 <= Sys.Date()){
  drive_sd("des",c(paste("Niveles/Sabana_POSGRADOS_",F1,".csv",sep=""),"Sabana$"))
  drive_sd("des",c(paste("Niveles/NR_POSGRADOS_",F1,".csv",sep=""),"NR$"))
  F1 <- F1 + days(7)
}
drive <- leer(c("Info General","gsheet"),sheet="Otros",secc="Prioridades Aula Mae")
#####----------------Inicio----------------#####
F2 <- ymd("2023-03-03"); F1 <- F2 - days(7); sem <- 1; BG_NR <- data.frame(); BG_S <- data.frame()
while (F2 <= Sys.Date()) {
  #NR
  if (sem!=1) {
    Rep <- leer(c(paste("NR_POSGRADOS_",F1,sep=""),"NR$"),col_select=c(Matricula,"Semaforo SA"=Semaforo)) %>% filter(!duplicated(Matricula))
    Base <- leer(c(paste("NR_POSGRADOS_",F2,sep=""),"NR$")) %>% left_join(Rep,"Matricula") %>% mutate(`Semana Extracción`=sem,`Fecha Extracción`=F2)
  }else{
    Base <- leer(c(paste("NR_POSGRADOS_",F2,sep=""),"NR$")) %>% mutate(`Semaforo SA`="_",`Semana Extracción`=sem,`Fecha Extracción`=F2)
  }
  if(length(BG_NR)==0) BG_NR <- Base else BG_NR <- rbind(BG_NR,Base)
  #Sabana
  if (sem!=1) {
    Rep <- leer(c(paste("Sabana_POSGRADOS_",F1,sep=""),"Sabana$"),col_select=c(Matricula,Clave,"Calificacion SA"=Calificacion,"NR SA"=NR)) %>%
      unite(MATMATER,Matricula,Clave,sep="",remove=F) %>% filter(!duplicated(MATMATER))
    Base <- leer(c(paste("Sabana_POSGRADOS_",F2,sep=""),"Sabana$")) %>% mutate_at(c("Semana","Calificacion"),~as.numeric(.)) %>%
      mutate(Prioridad="Sin prioridad",`Semana Extracción`=sem,`Fecha Extracción`=F2) %>% unite(MATMATER,Matricula,Clave,sep="") %>% left_join(Rep,"MATMATER")
  }else{
    Base <- leer(c(paste("Sabana_POSGRADOS_",F2,sep=""),"Sabana$")) %>% mutate_at(c("Semana","Calificacion"),~as.numeric(.)) %>%
      mutate(Prioridad="Sin prioridad",`Semana Extracción`=sem,`Fecha Extracción`=F2) %>% unite(MATMATER,Matricula,Clave,sep="",remove=F) %>%
      mutate(`Calificacion SA`="_",`NR SA`="_")
  }
  #####----------------Prioridades----------------#####
  Rep <- drive
  for (i in 1:(length(Rep)-1)) {
    Rep2 <- Rep[c(i,length(Rep))] %>% separate(1,c("C1","C2")," a ") %>% mutate_at(c("C1","C2"),~as.numeric(.)) %>% arrange(desc(Prioridad))
    if (length(Rep2[[1]])!=0) {
      for (j in 1:length(Rep2[[1]])) {
        Base <- mutate(Base,Prioridad=if_else(Semana==i,
                                              if_else(Calificacion >= Rep2[[j,"C1"]] & Calificacion <= Rep2[[j,"C2"]],paste("Prioridad ",Rep2[[j,"Prioridad"]],sep=""),Prioridad),
                                              Prioridad))
      }
    }
  }
  if(length(BG_S)==0) BG_S <- Base else BG_S <- rbind(BG_S,Base)
  F1 <- F2; F2 <- F2+days(7); sem <- sem+1
}
remove("Base","Rep","Rep2")
BG_NR <- mutate_all(BG_NR,~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
BG_S <- mutate_all(BG_S,~as.character(.)) %>% mutate_all(~replace_na(.,"_")) %>% select(-MATMATER)
escribir(BG_NR,c("Prioridades NR Mae.csv","Principales$"))
escribir(BG_S,c("Prioridades Mae.csv","Principales$"))





cosa <- filter(BG_NR,`Semaforo SA`=="Alto riesgo académico",`Semana Extracción`=="3")



cosa <- count(cosa,`Semaforo SA`,Semaforo)













