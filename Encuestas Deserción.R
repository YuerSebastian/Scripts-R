library(lubridate);library(dplyr);library(tidyr); library(MultiLEDS)
#Variables
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#####---------------------------Encuesta---------------------------#####
Base <- leer(c("1f4ywqZMreDOjYkUve4j5uIs_7e5f0Datzb4gUlZ1kOM","gsheet.ID"))
Rep <- leer(c("Info General","gsheet"),sheet="Orden Base",secc="Encuesta Deserción")
Base <- `colnames<-`(Base,Rep$Nuevo) %>% mutate(Fecha=dmy_hms(Fecha))
Base <- mutate(Base,Edad=as.integer(gsub("[[:alpha:][:punct:]\\pS]","",Edad)),
               Edad=if_else(is.na(Edad),0L,Edad),
               `Rango Edad`=if_else(!is.na(Edad),
                                    if_else(Edad<20,"Menores de 20",
                                            if_else(Edad<26,"20 a 25 años",
                                                    if_else(Edad<31,"26 a 30 años","Mayores a 30 años"))),"_"),
               Bimestre=if_else(Avance=="1 al 10 %","B1 y B2",
                                ifelse(Avance=="11 al 25 %","B3",
                                       if_else(Avance=="26 al 50 %","B4 a B6",
                                               if_else(Avance=="51 al 75 %","B7 y B8","B9 o mayor")))))
escribir(Base,c("Encuesta Deserción.csv","auxiliares$"))




