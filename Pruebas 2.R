sapply(c("dplyr","tidyr","lubridate","vroom","data.table","googledrive","googlesheets4","stringi"),require,character.only=T)
gs4_auth("jsalinba@utel.edu.mx")
Rep <- read_sheet("1OQENajkU5Z4L7MS7FNhrvrMBS1iT-EOc5oiON9FXcns","FI Pos") %>% .[.$Tipo=="Normal",]





















#####-----Fechas-----#####
options("lubridate.week.start"=1) #Configurando inicio de semana el lunes
f <- ymd("2023-10-23") + 6 #Día domingo de la semana del inicio de ciclo
d <- ymd("2024-01-08") + 6 #Día domingo de la última semana antes del inicio
fechas <- c(as.character(f)) #Iniciando vector de fechas a tomar en cuenta
#Fechas a tomar en cuenta, solo domingos en este caso.
while (f + 7 < d){
  f <- f + 7
  fechas <- append(fechas,as.character(f))
}
#####-----Descarga-----#####
#Otros bimestres
drive_auth("jsalinba@utel.edu.mx")
archs <- drive_ls("NR Licenciatura/2021") %>% .[grepl("jul.*.ago",.$name,ignore.case = T),] %>% drive_ls() %>%
  .[grepl(paste0("POSGRADOS_(",stri_join_list(list(fechas),"|"),"|.*.cierre)"),.$name,ignore.case = T),] %>% arrange(desc(name))
wd <- getwd()
setwd("C:/Users/jsalinba/Documents/Reportes/Bases/Históricos/Cierres Semanales/2021/Jul-Ago")
sapply(archs$id, drive_download,overwrite = T)
setwd(wd)
#Actual
drive_auth("jsalinba@utel.edu.mx")
archs <- drive_ls("Niveles") %>% .[grepl(paste0("POSGRADOS_(",stri_join_list(list(fechas),"|"),"|.*.cierre)"),.$name,ignore.case = T),] %>%
  arrange(desc(name))
wd <- getwd()
setwd("C:/Users/jsalinba/Documents/Reportes/Bases/Históricos/Cierres Semanales/2023/Nov-Dic")
sapply(archs$id, drive_download,overwrite = T)
setwd(wd)



















#####-----Fechas inicio-----#####
gs4_auth("jsalinba@utel.edu.mx")
Rep <- read_sheet("1OQENajkU5Z4L7MS7FNhrvrMBS1iT-EOc5oiON9FXcns","FI Pos") %>% .[.$Tipo=="Normal",] %>% 
  arrange(Inicio) %>% select(-Tipo) %>% mutate(Inicio = as.character(Inicio))
Rep <- mutate(Rep,Fin = Inicio)
Rep["Fin"] <- ymd(append(Rep[-1,"Fin"] %>% .[["Fin"]],0))-1
Rep <- mutate(Rep,Inicio = ymd(Inicio),Semanas = replace_na(as.integer((Fin - (Inicio + 6))/7),0),
              v = 0,dias = "")
for (i in 1:15) {
  Rep <- mutate(Rep, dias = if_else(v == i,as.character(Inicio + 6),
                                 if_else(v < Semanas,paste0(dias,(Inicio + 6) + (v * 7),","),
                                         if_else(v == Semanas,paste0(dias,(Inicio + 6) + (v * 7)),dias))),
                v = v+1)
}
Rep <- select(Rep,-v) %>% mutate(Faltantes = "") 
#####-----Comprobando archivos-----#####
for (a in c("2021","2022","2023")) {
  for (bim in c("Ene-Feb","Mar-Abr","May-Jun","Jul-Ago","Sep-Oct","Nov-Dic")){
    archs <- list.files(paste0("C:/Users/jsalinba/Documents/Reportes/Bases/Históricos/Cierres Semanales/",a,"/",bim))
    archs <- if_else(grepl("NR",archs),substring(archs,14,23),substring(archs,18,27)) %>% .[!duplicated(.)] %>% sort()
    sems <- str_split_1(Rep$dias[Rep$Año==a & Rep$Bimestre==bim],",")
    Rep$Faltantes[Rep$Año==a & Rep$Bimestre==bim] = stri_c(sems[!sems %in% archs],collapse = ",")
  }
}









































