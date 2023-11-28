sapply(c("MultiLEDS","dplyr","tidyr","lubridate"), library,character.only=T)
diremail("//Tut-095/reportes/NR Licenciatura")
x <- c("Ene-Feb","Mar-Abr","May-Jun","Jul-Ago","Sep-Oct","Nov-Dic")
y <- c("Enero - Febrero","Marzo - Abril","Mayo - Junio","Julio - Agosto","Septiembre - Octubre","Noviembre - Diciembre")
#####------------Inicio------------#####
for (i in 21:22) {
  for (j in 1:length(x)) {
    diremail("//Tut-095/reportes/NR Licenciatura")
    Base <- unificar(c(paste(y[j],"20$",i,"$"),"Sabana_POSGRADOS"),iden="nom") %>% mutate(Bimestre=paste(x[j]," ",i,sep="")) %>%
      mutate(`Tipo Sabana`=if_else(grepl("cierre",iden,ignore.case=T),"Cierre","Normal"),iden=substring(iden,18,27),iden=ymd(iden)) %>% rename("Fecha extraccón"=iden)
    diremail("C:/Users/jsalinba/Documents/Reportes")
    escribir(Base,c(paste("His_Sab_Mae ",x[j],"21",".csv",sep=""),"Históricos/Sabana"))
  }
}




















