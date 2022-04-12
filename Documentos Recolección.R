library(lubridate);library(dplyr);library(tidyr); library(MultiLEDS)
#Variables
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#####---------------------------Documentos Recolección---------------------------#####
Base <- leer(c("Info KPI's","gsheet"),sheet="Documentos") %>% mutate(MATRICULA=if_else(nchar(MATRICULA)<9,paste("0",MATRICULA,sep=""),MATRICULA))%>%
  mutate_at(c(grep("fecha",names(.),ignore.case = T)),~mdy(.)) %>% mutate(Guía=if_else(is.na(Guía),"_",
                                                                                       if_else(grepl("no se envía",Guía,ignore.case=T),"No enviada","Enviada")),
                                                                          Contacto=if_else(is.na(Contacto),"No","Si"))
Rep <- leer(c("Info General","gsheet"),sheet="General",secc="Campus") %>% select(-`Fecha Campus`) %>% rename("CAMPUS"=`Clave Campus`)
Base <- left_join(Base,Rep,by="CAMPUS")
#####---------------------------re ordenando---------------------------#####
Rep <- leer(c("Info General","gsheet"),sheet="Orden Base",secc="Documentos Recolección")
Base <- `colnames<-`(Base,Rep$Nuevo) %>% .[c(Rep$Incluir[!is.na(Rep$Incluir)])]
Base <- mutate_all(Base,~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
escribir(Base,c("Documentos Recolección.csv","auxiliares$"))
