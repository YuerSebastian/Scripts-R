library(MultiLEDS); library(dplyr); library(tidyr); library(DBI);library(RMySQL); library(lubridate)
diremail("C:/Users/Jsalinba/Documents/Reportes","jsalinba@utel.edu.mx")
#####----------------Sabana----------------#####
Base <- leer(c("Sabana_2022-03-11","Bases$"))%>%
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
conn <- dbConnect(MySQL(),user="jsalinba",host="10.1.46.182",password="Utel2022!!!",dbname="crm")
x <- c("rep_interaccs_cons","rep_interacs_cons_hoy")
for (i in 1:length(x)) {
  Rep2 <- dbGetQuery(conn,statement = paste("select * from ",x[i],sep = ""))
  if(i==1) Rep <- Rep2 else Rep <- rbind(Rep,Rep2)
};dbDisconnect(conn); remove(Rep2,conn)

Rep <- mutate(Rep,fecha_hora=ymd_hms(fecha_hora),sub_clas_c=gsub("Ã³","ó",sub_clas_c)) %>% arrange(desc(fecha_hora))%>%
  filter(fecha_hora>="2022-03-11" & fecha_hora<="2022-03-20")

Rep2 <- leer(c("Info General","gsheet"),sheet="Catálogo CRM")
Rep3 <- extr_secc(Rep2,"Primarios")
Rep <- left_join(Rep,Rep3,by=names(Rep3)[1])
Rep3 <- extr_secc(Rep2,"Subclass")
Rep <- left_join(Rep,Rep3,by=names(Rep3)[1])
remove(Rep2,Rep3)

Rep <- filter(Rep,mtvo_prim_c=="TU_ER" & `Tipo de Contacto`=="Contacto Efectivo") %>% mutate(matricula=if_else(nchar(matricula)<9,paste("0",matricula,sep=""),matricula))
cont <- count(Rep,matricula,name = "No. Llamadas")
Rep <- left_join(Rep,cont,by="matricula")
Rep <- mutate_all(Rep,~iconv(.,"UTF-8","LATIN1"))#Codificación
Rep <- select(Rep,matricula,`No. Llamadas`) %>% filter(!duplicated(matricula)) %>% mutate(`No. Llamadas`=as.integer(`No. Llamadas`))
Base <- left_join(Base,Rep,by="matricula") %>% mutate(`No. Llamadas`=if_else(is.na(`No. Llamadas`),0L,`No. Llamadas`))%>%
  mutate(`Tipo Llamada`=if_else(`No. Llamadas` > 0,"Contactado","Sin contacto"))
#####----------------Cruce con Base siguiente----------------#####
Rep <- leer(c("Sabana_2022-03-18","Bases$"),
            col_select=c("id_us","calif_posterior"=calificacion,"nr_posterior"=nivel_riesgo,"edo_moodle_posterior"=edo_moodle,"estado_posterior"=`estado escolares`))
Base <- left_join(Base,Rep,by="id_us") %>% mutate(calif_posterior=if_else(is.na(calif_posterior),"0",calif_posterior),calif_posterior=as.numeric(calif_posterior))%>%
  mutate_at(c("nr_posterior","edo_moodle_posterior","estado_posterior"),~if_else(is.na(.),"No encontrado",.))
Base <- mutate(Base,Cambio=if_else(calif_posterior > calificacion,"Positivo","Sin cambio"))


escribir(Base,c("Prioridades.csv","Reportes$"))



















