sapply(c("MultiLEDS","dplyr","tidyr","lubridate","googledrive"), library,character.only=T)
#####---------Leyendo---------#####
diremail("C:/Users/jsalinba.SCALA/Documents/Reportes","jsalinba@utel.edu.mx")
Base <- leer(c("1lWPjZkB904wbLbeLTk2QBeozoOnPfQEpTbDs0_yLOBg","gsheet.ID"),sheet="RetencionesBajas") %>%
  mutate_at(c("Estrategia de retencion EE","Estrategia de retencion RT"),~replace_na(.,"Sin estrategia")) %>%
  arrange(desc(as.integer(gsub("[A-z]-*","",Identificador)))) %>% mutate(Matricula=if_else(nchar(Matricula)<9,paste0("0",Matricula),Matricula)) %>%
  group_by(Matricula) %>% filter(!duplicated(Matricula)) %>% ungroup()
#####---------Filtrando y reemplazando EE---------#####
Rep <- filter(Base,grepl('([\\[\\]\\{\\}]|"lbl":|"val":)',`Estrategia de retencion EE`)) %>%
  mutate(`Estrategia de retencion EE` = strsplit(gsub('(\\[|\\]|\\{|\\}|[":]|lbl|val)',"",`Estrategia de retencion EE`),","),
         `Estrategia de retencion EE` = lapply(`Estrategia de retencion EE`, unique),
         `Estrategia de retencion EE` = lapply(`Estrategia de retencion EE`, paste, collapse = "@"))
#####---------Filtrando, uniendo y reemplazando EE---------#####
Base <- filter(Base,!grepl('([\\[\\]\\{\\}]|"lbl":|"val":)',`Estrategia de retencion EE`)) %>% rbind(Rep)
#####---------Filtrando y reemplazando RT---------#####
Rep <- filter(Base,grepl('([\\[\\]\\{\\}]|"lbl":|"val":)',`Estrategia de retencion RT`)) %>%
  mutate(`Estrategia de retencion RT` = strsplit(gsub('(\\[|\\]|\\{|\\}|[":]|lbl|val)',"",`Estrategia de retencion RT`),","),
         `Estrategia de retencion RT` = lapply(`Estrategia de retencion RT`, unique),
         `Estrategia de retencion RT` = lapply(`Estrategia de retencion RT`, paste, collapse = "@"))
#####---------Filtrando, uniendo y reemplazando RT---------#####
Base <- filter(Base,!grepl('([\\[\\]\\{\\}]|"lbl":|"val":)',`Estrategia de retencion RT`)) %>% rbind(Rep)
#####---------Reemplazando ";"---------#####
Base <- mutate_at(Base,c("Estrategia de retencion EE","Estrategia de retencion RT"),~gsub(";","@",.))
#####---------Separando columnas---------#####
cols <- list("EE" = paste0("EE ",1:max(sapply(strsplit(Base$`Estrategia de retencion EE`,"@"), length))),
             "RT" = paste0("RT ",1:max(sapply(strsplit(Base$`Estrategia de retencion RT`,"@"), length))))
Base <- separate(Base,`Estrategia de retencion EE`,cols[["EE"]],"@") %>% separate(`Estrategia de retencion RT`,cols[["RT"]],"@")
escribir(Base,c("Estrategias Bajas cols.csv","Bases$"),na="_")
#####---------Alargando---------#####
Rep <- list("EE" = select(Base,Matricula,"Ticket"=Identificador,`Fecha Inicio`,`Tarea actual`,12:22) %>%
              gather(`No. Estrategia EE`,`Estrategia EE`,5:(length(cols[["EE"]])+4),na.rm = T),
            "RT" = select(Base,Matricula,"Ticket"=Identificador,`Fecha Inicio`,`Tarea actual`,23:33) %>%
              gather(`No. Estrategia RT`,`Estrategia RT`,5:(length(cols[["RT"]])+4),na.rm = T))

Rep$EE <- mutate(Rep$EE,`No. Estrategia EE` = gsub("EE","Estrategia",`No. Estrategia EE`)) %>% filter(`No. Estrategia EE`=="Estrategia 1") %>%
  mutate(`Estrategia EE`=if_else(`Estrategia EE`=="","Sin estrategia",`Estrategia EE`))
Rep$RT <- mutate(Rep$RT,`No. Estrategia RT` = gsub("RT","Estrategia",`No. Estrategia RT`)) %>% filter(`No. Estrategia RT`=="Estrategia 1") %>%
  mutate(`Estrategia RT`=if_else(`Estrategia RT`=="","Sin estrategia",`Estrategia RT`))
#Base <- select(Base,-(12:34),-num)
#####---------Cruzando con info---------#####
escribir(Rep$EE,c("Estrategias RB EE.csv","Principales$"))
escribir(Rep$RT,c("Estrategias RB RT.csv","Principales$"))
escribir(Base,c("Estrategias completo.csv","Principales$"),na="_")
#####---------Subiendo---------#####
for (nom in c("Estrategias RB EE.csv","Estrategias RB RT.csv","Estrategias completo.csv")){
  drive_put(paste0("C:/Users/jsalinba.SCALA/Documents/Reportes/Bases/Principales/",nom),"Principales/")
}







































