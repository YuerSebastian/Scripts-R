library(MultiLEDS); library(dplyr); library(tidyr); library(lubridate)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#####--------------------------------Respuestas vs Estatus general--------------------------------#####
x <- c(`TRA Mae`="1Jr5Ppj_4Yr2fhRo7_YPdpU87xdMY8DWbDdmT-yF7bZ0",`TRA Lic`="1B57ZipoLyZjghXoWW-cPhMX2B9wQ4mVrDGuJ7KOpYpg",
       `TAA Mae`="1Ded8EMDsEY6kCGB60HP8stJxji_8IrOriTs45tVTsIw",`TAA Lic`="1wmwwF9XbPjVNlEVkCbmBCUrnjTQxVRPNBXSUY5kY60k")
for (i in 1:length(x)) {
  Rep <- leer(c(x[[i]],"gsheet.ID"),col_select=c("Matrícula","Fecha Respuesta"=`Marca temporal`,"Nombre"=`Nombre completo`,"Materia"))
  if (length(rownames(Rep))>0) {
    Rep <- mutate(Rep,Hoja=names(x)[i])
    if(exists("Base")) Base <- rbind(Base,Rep) else Base <- Rep
  }else{print(paste("Sin datos en ",names(x)[i]))}
}
Base <- mutate(Base,Matrícula=gsub("[a-z\u00C0-\u017F]|[[:punct:]]| ","",Matrícula,ignore.case = T),
               Matrícula2=gsub("[a-z\u00C0-\u017F]|[[:punct:]]| ","",Nombre,ignore.case = T),
               Matrícula=if_else(Matrícula2!="" & !is.na(Matrícula2),Matrícula2,Matrícula),
               Matrícula=if_else(nchar(Matrícula)<9,paste("0",Matrícula,sep=""),Matrícula)) %>% select(-Matrícula2,-Nombre) %>% mutate(`Fecha Respuesta`=dmy_hms(`Fecha Respuesta`))

Rep <- leer(c("Estatus_General_Alumno","SIR$"),
            col_select=c("Matrícula"=MATRICULA,"Clave Campus"=CAMPUS,"Clave Nivel"=NIVEL,"Clave Estatus"=`ESTATUS_CODE`,"Fecha Estatus"=FECHA_ESTATUS,
                         "Fecha PISE"=PRIMER_INSCRIPCION_SIN_ESTATUS,"Fecha Inicio"=FECHAINICIO)) %>% mutate_at(grep("Fecha|PISE",names(.)),~dmy(.))%>%
  arrange(desc(`Fecha Estatus`),desc(`Fecha PISE`),desc(`Fecha Inicio`)) %>% filter(!duplicated(Matrícula))
Base <- left_join(Base,Rep,by="Matrícula") %>% mutate(Encontrado=if_else(!is.na(`Clave Campus`),"Si","No")) %>% mutate_all(~as.character(.))%>%
  mutate_all(~replace(.,.==""|is.na(.),"_"))
x <- c("Matrícula","Clave Campus","Clave Nivel","Clave Estatus","Materia","Fecha Respuesta","Fecha Estatus","Fecha PISE","Fecha Inicio","Encontrado","Hoja")
Base <- Base[x]
escribir(Base,c("Respuestas talleres.csv","Principales$"))

googledrive::drive_put("D:/Trabajo/Bases/Principales/Respuestas talleres.csv","Principales/")














