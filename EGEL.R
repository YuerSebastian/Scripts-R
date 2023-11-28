sapply(c("MultiLEDS","dplyr","tidyr","lubridate"),library,character.only=T)
#shell("C:/Users/jsalinba/Documents/Reportes/Scripts/Python/EGEL.py")
diremail("C:/Users/jsalinba.SCALA/Documents/Reportes","jsalinba@utel.edu.mx")
#####-----------Uniendo Calificaciones finales y de actividades-----------#####
Rep <- leer(c("Calif_exam EGEL","Aula 14$")) %>% group_by(usuario,asignatura,actividad) %>% mutate(calificación=as.numeric(calificación)) %>% 
  arrange(desc(calificación)) %>% filter(!duplicated(usuario)) %>% ungroup() %>% unite(id,usuario,asignatura,sep="_")%>%
  select(id,actividad,comenzado,completado,requerido,calificación,intento) %>% left_join(
    leer(c("Calif_final EGEL","Aula 14$")) %>% select(matricula,asignatura,`ultimo acceso aula`,"Calificación Final"=calificacion)%>%
      unite(id,matricula,asignatura,sep="_"),by="id")
#####-----------Cruzando con base principal Acceso status-----------#####
Base <- leer(c("Acc_status EGEL","Aula 14$")) %>% unite(id,matricula,asignatura,sep="_",remove=F) %>% left_join(Rep,by="id") %>% mutate_all(~as.character(.))%>%
  mutate_at(13:16,~replace_na(.,"No realizada")) %>% mutate_at(17:18,~replace_na(.,"0"))%>%
  mutate(`ultimo acceso aula`=replace_na(`ultimo acceso aula`,"Nunca"),
         `Calificación Final`=replace_na(`Calificación Final`,"0.0"),
         `Rango Último Acceso`=as.Date(ymd_hm(`ultimo acceso`)),`Rango Último Acceso`=as.integer(today()-`Rango Último Acceso`),
         `Rango Último Acceso`=if_else(!is.na(`Rango Último Acceso`),
                                       if_else(`Rango Último Acceso`<1,"Hoy",
                                               if_else(`Rango Último Acceso`<4,"1 a 3 días",
                                                       if_else(`Rango Último Acceso`<7,"4 a 6 días","1 semana o más"))),"Nunca"))
#####-----------Cruzando con base principal Acceso status-----------#####
Rep <- leer(c("Info General","gsheet"),sheet="Orden Base Academia",secc="Nivel Riesgo EGEL")
Base <- `colnames<-`(Base,Rep$Nuevo)
Base <- Base[c(Rep$Incluir[!is.na(Rep$Incluir)])]
escribir(Base,c("Nivel Riesgo EGEL.csv","Principales$"))


