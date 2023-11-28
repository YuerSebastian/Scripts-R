sapply(c("dplyr","tidyr","lubridate","vroom","data.table","googledrive","googlesheets4"),require,character.only=T)
#####-----Descarga-----#####
drive_auth("jsalinba@utel.edu.mx")
dirs <- drive_ls("NR Licenciatura/2023")
archs <- drive_ls(dirs$id[4]) %>% .[grepl("_POSGRADOS_",.$name),] %>% arrange(desc(name))
wd <- getwd()
setwd("C:/Users/jsalinba/Documents/Reportes/NR Posgrados/2023/Ene-Feb")
sapply(archs$id, drive_download,overwrite = T)
setwd(wd)




















