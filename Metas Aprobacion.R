library(MultiLEDS); library(dplyr); library(tidyr); library(lubridate)
diremail("C:/Users/jsalinba/Documents/Reportes","jsalinba@utel.edu.mx")
#####---------------------Moviendo Archivos---------------------#####
file.copy("//TUT-095/Conectivida/actual.csv","C:/Users/jsalinba/Documents/Reportes/Bases/Adicionales y auxiliares/Aprobación actual.csv",overwrite = T)
file.copy("//TUT-095/Conectivida/may-jun21.csv","C:/Users/jsalinba/Documents/Reportes/Bases/Adicionales y auxiliares/Aprobación anterior.csv",overwrite = T)
#####---------------------Bases---------------------#####
Base <- leer(c("Aprobación actual","auxiliares$"))
Rep <- leer(c("Aprobación anterior","auxiliares"))
Base <- rbind(Base,Rep)
Rep <- leer(c("Info General","gsheet"),sheet="Otros",secc="Reglas de Negocio")
Base <- left_join(Base,Rep,by="Regla_Negocio") %>% filter(`Tipo Regla Negocio`!="OPM"&`Tipo Regla Negocio`!="_")
escribir(Base,c("Metas de Aprobación.csv","Principales$"))
#####---------------------Metas---------------------#####
Rep <- filter(Base,Bimestre=="May-Jun22") %>% count(`Tipo Regla Negocio`,Bloque,Semana) %>% mutate(Semana=as.integer(substring(Semana,8))) %>% arrange(desc(Semana))%>%
  group_by(`Tipo Regla Negocio`,Bloque) %>% filter(Semana==max(Semana)) %>% ungroup() %>% select(-n) %>% unite(Clave,`Tipo Regla Negocio`,Bloque)

Base <- leer(c("Info General","gsheet"),sheet="Otros",secc="Metas") %>% unite(Clave,`Tipo Regla Negocio`,Bloque,remove = F) %>% left_join(Rep,by="Clave") %>% select(-Clave)%>%
  rename("Semana Actual"=Semana) %>% .[c("Tipo Regla Negocio","Bloque","Semanas","Semana Actual","Meta X Bloque","Meta General")]

escribir(Base,c("Metas.csv","Principales$"))