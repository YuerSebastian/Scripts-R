library(MultiLEDS);library(dplyr);library(tidyr);library(lubridate)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#Inicio
drive_sd("des",c("Interacciones_gral.csv","CRM$"))
Rep <- leer(c("Info General","gsheet"),sheet="Catálogo CRM")
Base <- leer(c("Interacciones_gral","CRM$")) %>% mutate(fecha_hora=ymd_hms(fecha_hora))
x <- c("Primarios","Subclass")
for (i in x) {
  Rep2 <- extr_secc(Rep,i); Base <- left_join(Base,Rep2,by=names(Rep2)[1])
}


