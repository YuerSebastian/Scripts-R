sapply(c("data.table","tidyr","lubridate","googledrive","dplyr","vroom"),require,character.only=T)
#####-----Fechas-----#####
options("lubridate.week.start"=1) #Configurando inicio de semana el lunes
f <- dmy("23/10/2023") + 6 #Día domingo de la semana del inicio de ciclo
sd <- Sys.Date() + (7 - wday(Sys.Date())) #Día domingo de la semana en curso
fechas <- c(as.character(f)) #Iniciando vector de fechas a tomar en cuenta
#Fechas a tomar en cuenta, solo domingos en este caso.
while (f + 7 < sd){
  f <- f + 7
  fechas <- append(fechas,as.character(f))
}
#####-----Descargas-----#####
drive_auth("jsalinba@utel.edu.mx")
desc <- drive_ls("Niveles/") %>% .[grepl(paste0("Sabana_POSGRADOS_(",stringi::stri_join_list(list(fechas),"|"),")"),.$name),]
for (i in 1:length(desc[[1]])) drive_download(desc$id[i],paste0("D:/Trabajo/Bases/Drive/Sabana/",desc$name[i]),overwrite = T)
#####-----Unificando-----#####
Base <- bind_rows(lapply(paste0("D:/Trabajo/Bases/Drive/Sabana/Sabana_POSGRADOS_",fechas,".csv"), fread, encoding = "Latin-1", colClasses = "character") %>%
                    setNames(fechas),.id = "Fecha extracción")
#####-----Semanas-----#####
fechas <- as.data.frame(fechas) %>% mutate(`Semana extracción`=paste0("Semana ",row_number()-1)) %>% rename("Fecha extracción" = fechas)
Base <- left_join(Base,fechas,"Fecha extracción")
#####-----Transformaciones-----#####
Base <- filter(Base,Status.Materia=="Activo",Estado.Banner %in% c("MATRICULADO","ADMITIDO")) %>%
  count(Asignatura, NR, Clave, Grupo, Programa, Clave_Programa, Modalidad, Campus, Facultad, Ciclo, Bloque.seguimiento, Profesores,
        Tutor, Aula, `Semana extracción`) %>% pivot_wider(names_from = NR, values_from = n, values_fill = 0)
#####-----Imprimiendo-----#####
write.table(Base,"D:/Trabajo/Bases/Principales/Aprobación x materia.csv",sep = ",",row.names = F)
fwrite(Base,"D:/Trabajo/Bases/Principales/Aprobación x materia.csv")
vroom_write(Base,"D:/Trabajo/Bases/Principales/Aprobación x materia.csv",",")

vroom_write()

Rep <- count(Base,Facultad)















