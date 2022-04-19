library(MultiLEDS); library(dplyr); library(tidyr)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#####----------------Sabana----------------#####
Base <- leer(c("Sabana_2022-03-11","Bases$"))%>%
  mutate(Prioridad=if_else(edo_moodle=="Activo" & nivel_riesgo=="Reprobó" & (stutus=="ADMITIDO" | stutus=="MATRICULADO"),"P","_"))%>%
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
#####----------------Contactos----------------#####
library(RMySQL);library(DBI)
cons <- dbListConnections(MySQL())
for(con in cons)dbDisconnect(con)

Rep<- dbConnect(MySQL(),user="jsalinba",host="10.1.46.182",password="Utel2022",dbname="crm")

interacciones<- dbGetQuery(CRM,statement = "select * from rep_interaccs_cons")
interacciones[,4] <- as.numeric(as.character(interacciones[,4]))
actuales <- dbGetQuery(CRM,statement = "select * from rep_interacs_cons_hoy")
actuales[,4] <- as.numeric(as.character(actuales[,4]))
actuales$department <- NULL
interacciones$department <- NULL
CRM <- rbind(interacciones,actuales)



installr::updateR()
