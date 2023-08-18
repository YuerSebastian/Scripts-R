library(MultiLEDS); library(googledrive); library(dplyr); library(tidyr);library(lubridate)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#####---------------------Bases---------------------#####
Rep <- leer(c("Aprobación Mae anterior","auxiliares$"))
Base <- leer(c("Aprobación actual Mae","auxiliares$")) %>% rbind(Rep) %>% mutate(Per=if_else(!is.na(Bloque.seguimiento),
                                                                                             if_else(grepl("Bimestre",Bloque.seguimiento),"Bim","Cua"),"Sin"),
                                                                                 Num=if_else(Per=="Bim",as.integer(substring(Bloque.seguimiento,10,11)),
                                                                                             if_else(Per=="Cua",as.integer(substring(Bloque.seguimiento,14,15)),0L)),
                                                                                 `Tipo Bloque`=if_else(Num>=4,
                                                                                                       if_else(Per=="Bim","B4+","C4+"),"_"))# %>% filter(!is.na(Bloque.seguimiento))
for (i in 1:4) {
  Base <- mutate(Base,`Tipo Bloque`=if_else(`Tipo Bloque`=="_",
                                            if_else(Num!=0,
                                                    if_else(Num==i & Per=="Bim",paste("B",i,sep = ""),
                                                            if_else(Num==i & Per=="Cua",paste("C",i,sep = ""),`Tipo Bloque`)),"Sin bloque"),`Tipo Bloque`))
}
Rep <- leer(c("Info General","gsheet"),sheet="Otros",secc="Reglas de Negocio")
Base <- left_join(Base,Rep,by="Regla_Negocio") %>% filter(`Tipo Regla Negocio`!="OPM"&`Tipo Regla Negocio`!="_") %>% .[c(1,2,14,3:6,15,7:11)]
escribir(Base,c("Metas Aprobación Mae.csv","Principales$"))
#####---------------------Metas---------------------#####
Rep <- filter(Base,Bimestre=="Sep-Oct22") %>% count(`Tipo Regla Negocio`,`Tipo Bloque`,Semana) %>% mutate(Semana=as.integer(substring(Semana,8))) %>% arrange(desc(Semana))%>%
  group_by(`Tipo Regla Negocio`,`Tipo Bloque`) %>% filter(Semana==max(Semana)) %>% ungroup() %>% select(-n) %>% unite(Clave,`Tipo Regla Negocio`,`Tipo Bloque`)

Base <- leer(c("Info General","gsheet"),sheet="Otros",secc="Metas Mae") %>% unite(Clave,`Tipo Regla Negocio`,Bloque,remove = F)%>%
  left_join(Rep,by="Clave") %>% select(-Clave) %>% rename("Semana Actual"=Semana) %>% .[c("Tipo Regla Negocio","Bloque","Semanas","Semana Actual","Meta X Bloque","Meta General")]

escribir(Base,c("Metas Mae.csv","Principales$"))
