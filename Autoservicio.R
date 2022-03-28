library(dplyr)
library(stringi)

descargas <- list.files ("D:/Cosa/Asignación prueba/Autoservicio")
###################################---Uniendo autoservicio---###################################
for (i in 1:length(descargas)) {
  Aux <- read.csv(descargas[i])
  Aux <- select(Aux, -X)
  Aux$MATRICULA <- as.character(Aux$MATRICULA)
  if(i==1) Auto <- Aux else Auto <- rbind(Auto,Aux)
}
Auto <- tidyr::separate(Auto, PROGRAMA, into = c("CLAVE","PROGRAMA"), sep = "-")
#Filtrando
Aux <- readxl::read_excel("D:/Cosa/Asignación prueba/Tablas/Autoservicio_Tip.xlsx")
Auto <- filter(Auto, ESTATUS_SOLC=="PAGADO")
Auto <- left_join(Auto, Aux, by="SERVICIO")
Auto <- Auto[grep("SER", Auto$AREA, ignore.case = T),]#Auto <- filter(Auto, AREA=="SER-BACK"|AREA=="SER-FRONT"|AREA=="SER-TIT")   También se úede usar
###################################Cruzando con base anterior###################################
Aux <- read.csv("D:/Cosa/Asignación prueba/Autoservicio_Anterior.csv")
Aux <- select(Aux, SEQ_NO, Responsable, Acción)
Auto <- left_join(Auto, Aux, by="SEQ_NO")
###################################---Asignando nuevos---###################################
Auto$Responsable[is.na(Auto$Responsable)] <- "nuevo"
Aux <- filter(Auto, Responsable=="nuevo")
#Conteo
if (length(Aux$Responsable)!=0) {
  Aux2 <- readxl::read_excel("D:/Cosa/Asignación prueba/Asignación_Analistas/Responsables_Auto.xlsx")
  x <- rep(1:length(Aux2$Responsable), length(Aux$Responsable)/length(Aux2$Responsable))
  if ((length(Aux$Responsable)-length(x))!=0) {
    y <- c(1:(length(Aux$Responsable)-length(x)))
    x <- c(x,y)
  }
  Aux$Responsable <- x
  Aux <- left_join(Aux, Aux2, by="Responsable")
  remove(Aux2)
  Aux$Responsable <- Aux$Responsable2
  Aux <- select(Aux, -Responsable2)
  Aux$Acción <- "Trabajar"
  #Uniendo nuevos
  Auto <- filter(Auto, Responsable!="nuevo")
  Auto <- rbind(Aux, Auto)
}
#Separando programa

Auto <- Auto[c(1,2,29,3,30,31,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)]
Auto <- mutate_all(Auto, ~replace(.,is.na(.),"."))
Auto <- mutate_all(Auto, ~stri_trim(.))

write.csv(Auto, file = "D:/Cosa/Asignación prueba/Autoservicio.csv",row.names = F)
write.csv(Auto, file = "D:/Cosa/Asignación prueba/Autoservicio_Anterior.csv",row.names = F)






















