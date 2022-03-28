library(lubridate)
library(readxl)
library(openxlsx)
library(dplyr)

arch_control <- "D:/Cosa/Sincronización/control-prueba.xlsx"
Fin_Proceso <- function(proc){
  shell("TASKLIST /FO CSV>D://Cosa//Procesos.csv")
  pro <- read.csv("D:/Cosa/Procesos.csv")
  names(pro)[1] <- "Proceso"
  pro <- select(pro, Proceso)
  pro <- pro[grep(proc, pro$Proceso, ignore.case = T),]#cuando hay solo una columna se convierte en lista
  pro <- pro[!duplicated(pro)]
  if (length(pro)!=0) {
    for (i in 1:length(pro)) {
      shell(paste("taskkill /f /im",pro[i],sep = " "))
    }
  }
}
Info_control <- function(arch,opc="Reset",cod="Ninguno",c=0,carp_arch=NULL,num=0){
#arch = El dataframe de la información
#opc = La opción a elegir, por defecto "Reset"
#cod = El nombre código que se va a ejecutar
#c = El número que se va a usar como contador, por defecto 0
#carp_arch = La carpeta para evaluar el tamaño de descargas
#num = El número de descargas que se supone debe haber.
  if (opc=="Reset") {
    print("Reseteando Info")
    arch <- mutate(arch, CODIGO="Ninguno",
                   ULT_CODIGO="Ninguno",
                   ESTADO="Sin ejecutar",
                   INICIO=as.character(now()),
                   FIN="Ninguno",
                   ARCHIVOS=0,
                   EJECUCIONES=c,
                   ERROR="NO",
                   INI_ULT_EJ="Ninguno",
                   TER_ULT_EJ="Ninguno")
    return(arch)
  }
  if (opc=="Inicio") {
    print("Info inicio")
    arch <- mutate(arch, CODIGO="Ninguno",
                   ULT_CODIGO=cod,
                   ESTADO="Ejecutando...",
                   ARCHIVOS="Calculando...",
                   EJECUCIONES=c,
                   INI_ULT_EJ=as.character(now()))
    return(arch)
  }
  if (opc=="Fin") {
    print("Info Fin")
    if (is.null(carp_arch)!=T) {
      des <- length(list.files(carp_arch))
    }else{
      des <- 0
    }
    arch$ERROR <- if_else(des<num,"SI","NO")
    arch <- mutate(arch, ARCHIVOS=des,
                      ESTADO="Sin ejecutar",
                      TER_ULT_EJ=as.character(now()))
    return(arch)
  }
}
Guardar <- function(data,arch,nom="Hoja"){
  if (file.exists(arch)) file.remove(arch)
  write.xlsx(data,file = arch,sheetName=nom, row.names = F)
}
#Reiniciando Info
c <-0
control <- read_excel(arch_control)
control <- Info_control(control)
Guardar(control, arch_control,"control")
Sys.sleep(1)
#Mientras la hora sea menor a las 8pm, se seguirá ejecutando cada 300 segundos (5 minutos) y lee nuevamente el archivo.
while (hour(now())<21) {
  control <- read_excel(arch_control)
  if (control$CODIGO=="asig") {
    #Inicio info
    c <- c+1
    control <- Info_control(control,"Inicio","asig",c)
    Guardar(control, arch_control,"control")
    #Script
    Fin_Proceso("chrome|FLOKZU|RuntimeBroke|GoogleCrash")
    shell("D:/Scripts/Python/FLOKZU_Auto/FLOKZU_Chrome/FLOKZU_Chrome.py")
    #Fin info
    control <- Info_control(control,"Fin",carp_arch = "D:/Cosa/Asignación prueba/FLOKZU",num = 36)
    Guardar(control, arch_control,"control")
  }
  
  if (control$CODIGO=="valid") {
    c <- c+1
    control <- Info_control(control,"Inicio","valid",c)
    Guardar(control, arch_control,"control")
    
    Fin_Proceso("chrome|SIR_NI_RI|RuntimeBroke|GoogleCrash")
    shell("D:/Scripts/Python/REPORTES_SIR/SIR_NI-RI_Validación/SIR_NI-RI_Validación.py")
    
    control <- Info_control(control,"Fin")
    Guardar(control, arch_control,"control")
  }
  
  Sys.sleep(120)
}
control$FIN <- as.character(now())
Guardar(control, arch_control,"control")


















