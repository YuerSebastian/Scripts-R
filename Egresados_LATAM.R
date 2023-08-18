library(MultiLEDS); library(dplyr); library(tidyr); library(lubridate)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#Inicio
Rep <- leer(c("1E3fVWjyCyqK7dU0iOhWTlDtFK2fgZLjtQYakCh2718Y","gsheet.ID"),sheet="Seguimiento MAE LATAM",
            col_select=c("Matrícula"=MATRICULA,"Programa Egreso"=`PROGRAMA DE EGRESO`,"Asesor"=`NOMBRE DEL ASESOR`,"Opción Titulación"=`OPCIÓN DE TITULACIÓN`,
                         "Formato Academia"=`FORMATO LIBERADO POR ACADEMIA`,"Formato SER"=`FORMADO LIBERADO A SERVICIOS ESCOLARES`,"Contacto"=`CONTACTO CON ASESOR`))

Base <- leer(c("1S7TsK68cJ9R3uv9lS3hIu7I0-uLnEcR2IwMCUq5724k","gsheet.ID"),sheet="Respuestas de formulario 2") %>%
  `colnames<-`(c("Fecha","Matrícula","Nombre","Programa Egreso F","Nivel Egreso F","100% Créditos","Opción Titulación F","Correo","Correo Contacto","Asesor Asignado"))%>%
  mutate(Fecha=dmy_hms(Fecha)) %>% group_by(Matrícula) %>% arrange(desc(Fecha)) %>% filter(!duplicated(Matrícula)) %>%
  select(Matrícula,`Asesor Asignado`,`Opción Titulación F`,`Programa Egreso F`,Fecha) %>% left_join(Rep,by="Matrícula") %>%
  select(Matrícula,`Programa Egreso F`,`Programa Egreso`,`Asesor Asignado`,Asesor,`Opción Titulación F`,`Opción Titulación`,`Formato Academia`,`Formato SER`,`Contacto`,Fecha) %>%
  add_count(Matrícula,name = "Duplicado")

escribir(Base,c("Egresados LATAM.csv","Bases$"),na="_")





















