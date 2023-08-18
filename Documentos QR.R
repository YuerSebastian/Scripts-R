library(MultiLEDS); library(dplyr); library(tidyr); library(lubridate)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
acc <- c("AVCU","CAPS","COES","COTE","DIMA","DPIN","HIAC","TIPR")
resp <- c("Leticia Farias Pablo","Leticia García Martínez","Dayan Manzano Balderas","Víctor Morales Méndez")
#####-----------Inicio-----------#####
Rep <- leer(c("Info General","gsheet"),sheet="Documentos QR")
Base <- leer(c("Documentos QR","SIR$")) %>% mutate_at(c("FECHA","FECHAENVIO"),~dmy_hms(.)) %>% filter(ACCESORIO %in% acc) %>% unite(MATACCE,MATRICULA,ACCESORIO,remove = F) %>%
  arrange(desc(FECHA)) %>% filter(!duplicated(MATACCE)) %>% left_join(leer(c("Documentos QR","Principales"),col_select=c(MATACCE,Responsable)) %>% 
                                                                        filter(!duplicated(MATACCE)),by="MATACCE") %>%
  repartir("Responsable",extr_secc(Rep,"Responsables")[[1]]) %>% mutate(Envío=if_else(is.na(ENVIO),"No enviado","Enviado"))
#####-----------Info General-----------#####
Base <- left_join(Base,extr_secc(Rep,"Usuarios SIU") %>% rename("USUARIO_ENVIO"=Usuario),by="USUARIO_ENVIO")%>%
  left_join(extr_secc(Rep,"Accesorios") %>% rename("ACCESORIO"=Clave),by="ACCESORIO") %>%
  mutate(Días=if_else(is.na(FECHAENVIO),difftime(now(),FECHA,units = "days"),-1),Días=as.integer(Días))
#####-----------Documentos Recolección-----------#####
drive_sd("des",c("Adicionales y auxiliares/Recolección Documentos.csv","auxiliares$"))
Rep <- leer(c("Recolección Documentos","auxiliares$"),col_select=c(MATRICULA,CAMPUS,ESTATUS,Tipo_Ingreso,Equivalencia,`estatus expediente`)) %>%
  mutate(MATRICULA=ifelse(nchar(MATRICULA)==8,paste("0",MATRICULA,sep = ""),MATRICULA)) %>% filter(!duplicated(MATRICULA))
Base <- left_join(Base,Rep,by="MATRICULA")
#####-----------Académico Financiero-----------#####
Rep <- leer(c("Académico Financiero","SIR$"),col_select=c(MATRICULA,SALDO_VENCIDO,PROX_FECHA_LIMITE_PAG)) %>%
  mutate(SALDO_VENCIDO=as.numeric(SALDO_VENCIDO),Adeudo=if_else(SALDO_VENCIDO>500,"Si","No"),PROX_FECHA_LIMITE_PAG=dmy(PROX_FECHA_LIMITE_PAG)) %>%
  arrange(desc(PROX_FECHA_LIMITE_PAG)) %>% filter(!duplicated(MATRICULA))
Base <- left_join(Base,Rep,by="MATRICULA")
#####-----------Reordenando e imprimiendo-----------#####
Base <- mutate_all(Base,~as.character(.)) %>% mutate_all(~replace_na(.,"_"))
Rep <- leer(c("Info General","gsheet"),sheet="Orden Base",secc="Documentos QR")
Base <- `colnames<-`(Base,Rep$Nuevo) %>% .[c(Rep$Incluir[!is.na(Rep$Incluir)])]
escribir(Base,c("Documentos QR.csv","Principales$"))

























