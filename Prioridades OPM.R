sapply(c("MultiLEDS","dplyr","tidyr"),library,character.only=T)
diremail("C:/Users/jsalinba/Documents/Reportes")
#####-----------Materias-----------#####
Rep <- leer(c("Ponderaciones_Prioridades","Bases$"),sheet="Prioridades1")
mats <- filter(Rep,!duplicated(Clave)) %>% .[["Clave"]] #Se debe crear for completo
Base <- leer(c("reporte_completo_OPM","Bases$")) %>% mutate_at(c("Semana","calificacion"),~as.numeric(.)) %>% mutate(Prioridad="Sin prioridad") %>%
  separate(ciclo,c("col1","col2","col3","col4"),sep="_",remove = F) %>%
  mutate(`Clave Materia`=if_else(Modalidad=="Bachillerato",col2,
                                 if_else(Modalidad=="Doctorado"|Institucion=="UNAG",col3,col4))) %>% select(-c(col1,col2,col3,col4))
mat <- "13203"
for (mat in mats) {
  #####-----------Transformando a formato columnas y rangos-----------#####
  Rep2 <- filter(Rep,Clave==mat) %>% select(semana,grep("prioridad",names(.),ignore.case=T)) %>% mutate_all(~as.numeric(.)) %>% mutate(Calif="_",Calif2="_")
  if (hasName(Rep2,"Prioridad 3")) {
    if (is.na(Rep2[[1,"Prioridad 3"]])) {
      Rep2 <- select(Rep2,-`Prioridad 3`)
    }
  }
  for (i in 2:(length(Rep2)-1)){
    if (i==2) {
      Rep2["Calif"] <- paste("0 a ",Rep2[[2]],sep="")
    }else{
      Rep2["Calif2"] <- Rep2["Calif"]
      Rep2["Calif"] <- paste((Rep2[[i-1]] + 0.01),"a",Rep2[[i]],sep=" ")
      Rep2[i-1] <- Rep2["Calif2"]
    }
  }
  #Trasponiendo
  Rep2 <- select(Rep2,-Calif,-Calif2) %>% arrange(semana) %>% t() %>% as.data.frame() %>% `colnames<-`(.[1,]) %>% .[-1,] %>%
    mutate(Prioridad=rownames(.),Prioridad=gsub("prioridad","",Prioridad,ignore.case=T)) %>% `rownames<-`(row_number(rownames(.))) %>% mutate(Prioridad=as.integer(Prioridad))
  #Rep2 <- Rep2[-grep("NA",Rep2[,1]),] #Quitando todo lo que ya no tenga rango prioridad
  #####-----------Definiendo prioridades-----------#####
  for (j in 1:(length(Rep2)-1)) {
    Rep3 <- Rep2[c(j,length(Rep2))] %>% separate(1,c("C1","C2")," a ") %>% mutate_at(c("C1","C2"),~as.numeric(.)) %>% arrange(desc(Prioridad))
    if (length(Rep3[[1]])!=0) {
      for (k in 1:length(Rep3[[1]])) {
        Base <- mutate(Base,Prioridad=if_else(`Clave Materia`==mat & Semana==j,
                                              if_else(calificacion >= Rep3[[k,"C1"]] & calificacion <= Rep3[[k,"C2"]],paste("Prioridad ",Rep3[[k,"Prioridad"]],sep=""),Prioridad),
                                              Prioridad))
      }
    }
  }
}
Base <- codificacion(Base)
escribir(Base,c("Prioridades OPM_1.csv","Bases$"))

























