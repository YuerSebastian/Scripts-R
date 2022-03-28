shell("tasklist /fo CSV>D:/Cosa/procesos.csv")
pro <- read.csv("D:/Cosa/procesos.csv")
names(pro)[1] <- "Proceso"
pro <- dplyr::select(pro, Proceso)
pro <- pro[grep("chrome|FLOKZU|RuntimeBroker", pro$Proceso, ignore.case = T),]#cuando hay solo una columna se convierte en lista
pro <- pro[!duplicated(pro)]
if (length(pro)!=0) {
  for (i in 1:length(pro)) {
    shell(paste("taskkill /f /im",pro[i],sep = " "))
  }
}






