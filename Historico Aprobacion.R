library(MultiLEDS); library(googledrive); library(tidyr); library(dplyr)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#####--------------Descargas--------------#####
# x <- leer(c("Sabanas Cierre 2021","Trabajo$"))
# for (i in 1:length(rownames(x))) {
#   drive_sd("des",c(paste("NR Licenciatura/",x[[i,2]],"/",x[[i,1]],sep=""),"Cierres$"))
# }
x <- leer(c("Sabanas Cierre 2021","Trabajo$"),sheet="Columnas")
#####--------------Lectura--------------#####
Base <- unificar("Cierres$",
                 c(Status.Plataforma="Estado Plataforma",Estado.plataforma="Estatus Plataforma",
                   edo_moodle="Estado Moodle",Status.Materia="Estado Moodle",
                   Matricula="Matrícula",matricula="Matrícula",
                   Telefono="Celular",
                   Clave="Clave Materia",Clave_Asigntaura="Clave Materia",
                   NR="Nivel Riesgo",nivel_riesgo="Nivel Riesgo",
                   TM="Tipo Materia",Tipo_Materia="Tipo Materia",
                   Modalidad="Nivel",
                   Profesores="Profesor",profesor="Profesor",
                   Bloque.Ingreso="Bloque Ingreso",B.ingreso="Bloque Ingreso",
                   Estado.Asignacion="Estado Asignación",Asignación="Estado Asignación",
                   Estado.Banner="Estado Banner",Estado_Banner="Estado Banner",
                   Inicio.de.curso="Fecha Inicio",Fecha.Inicio="Fecha Inicio",
                   SemanasTranscurridas="Semana",
                   Acceso.Materia="Acceso Materia",AccesosMateria="Acceso Materia",
                   Acceso.Aula="Acceso Aula",AccesoAula="Acceso Aula",
                   Ubicación_Estado="Ubicación",`Ubicación/Estado`="Ubicación",
                   Duracion="Duración",
                   Mat.Profesor="Matrícula Profesor",Mat_Profesor="Matrícula Profesor",
                   Clave_Programa="Clave Programa",
                   Asignatura="Materia",
                   Fecha_Estado="Fecha Estado",
                   Bloque.seguimiento="Bloque Seguimiento",
                   Tipo.Alumno="Tipo Alumno",Tipo.alumno="Tipo Alumno",
                   TutorNivel="Nivel Tutor",
                   documentos="Documentos",
                   FechaNacimiento="Fecha Nacimiento",
                   Correo_Alterno="Correo Alterno",
                   Saldo_Total="Saldo Total",
                   FechaMatriculacion="Fecha Matriculación",
                   Ultimo_Contacto="Último Contacto",
                   Tutor_Contacto="Tutor Contacto",
                   mail_profesor="Correo Profesor"),x[["Incluir"]],iden = "nom")%>%
  mutate(`Fecha Creación`=substring(iden,18,27)) %>% filter(`Estado Moodle`=="Activo",`Estado Banner` %in% c("ADMITIDO","MATRICULADO"))
x <- leer(c("Sabanas Cierre 2021","Trabajo$")) %>% rename("iden"=Nombre) %>% select(-Carpeta)
Base <- left_join(Base,x,by="iden") %>% select(-iden) %>% mutate_all(~replace(.,.=="-"|is.na(.),"_"))

escribir(Base,c("Aprobación Mae Histórico.csv","Principales$"))






























