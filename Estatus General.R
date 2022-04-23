library(MultiLEDS); library(dplyr); library(tidyr); library(lubridate)
#Variables
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#####-----------------------Estatus General Alumnos-----------------------#####
Base <- leer(c("Estatus_General_Alumno","SIR$")) %>% mutate(`Clave Programa`=substring(PROGRAMA,1,10),PROGRAMA=substring(PROGRAMA,12))%>%
  unite(MATPROG,MATRICULA,`Clave Programa`,na.rm = T,remove = F,sep = "") %>% mutate_at(grep("FECHA|SIN_ESTATUS",names(.)),~dmy(.)) %>% filter(!duplicated(MATPROG))
#####-----------------------Eficiencia-----------------------#####
Base <- mutate(Base,Eficiencia=as.period(gsub("-","",as.period(interval(FECHA_ESTATUS,PRIMER_INSCRIPCION_SIN_ESTATUS)))))
Rep <- leer(c("Info General","gsheet"),sheet="Otros",secc = "Eficiencia") %>% mutate(Rango=as.period(Rango))
Base$`Tipo Eficiencia` <- "_"
for (i in 1:length(rownames(Rep))) {
  Base <- mutate(Base,`Tipo Eficiencia`=if_else(is.na(Eficiencia),"Indefinido",
                                                if_else(`Tipo Eficiencia`=="_",
                                                        if_else(Eficiencia<Rep$Rango[i],Rep$`Tipo Eficiencia`[i],"_"),`Tipo Eficiencia`)))
}
x <- c(" 0[ymdHMS]","^1y","^1m","^1d"," 1m"," 1d","y","m","d","0S");y <- c("","1 Año","1 Mes","1 Día"," 1 Mes"," 1 Día"," Años"," Meses"," Días","Mismo Día")
for (i in 1:length(x)) {
  Base <- mutate(Base,Eficiencia=gsub(x[i],y[i],Eficiencia))
}
#####-----------------------Avance Curricular-----------------------#####
Rep <- leer(c("Avance_Curricular","SIR$")) %>% unite(MATPROG,MATRICULA,PROGRAMA,sep = "") %>% select(MATPROG,AVANCE_CURRICULAR,PROMEDIO) %>% mutate_at(c(2,3),~as.numeric(.))
Rep2 <- leer(c("Info General","gsheet"),sheet="Otros",secc="Promedios") %>% separate(Promedio,into = c("Pr1","Pr2"),sep = " o ") %>% mutate_at(c(1,2),~as.integer(.))
Rep$`Tipo Promedio` <- "_"
for (i in 1:length(rownames(Rep2))) {
  Rep <- mutate(Rep,`Tipo Promedio`=if_else(`Tipo Promedio`=="_",
                                            if_else(PROMEDIO>=Rep2[[i,1]] | PROMEDIO>=Rep2[[i,2]],Rep2[[i,3]],"_"),`Tipo Promedio`))
}




Base <- left_join(Base,Rep,by="MATPROG")
