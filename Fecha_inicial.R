sapply(c("MultiLEDS","dplyr","tidyr","lubridate"), library,character.only=T)
diremail("D:/Trabajo")

Base <- leer(c("Fecha_Inicial","SIR$")) %>% mutate(INICIO_CLASES=dmy(INICIO_CLASES)) %>%
  unite(MATPROG,MATRICULA,PROGRAMA,NIVEL,remove = F) %>% unite(Clave,MATPROG,INICIO_CLASES,remove = F)

Rep <- filter(Base,!duplicated(Clave)) %>% add_count(MATPROG,name="Bimestre") %>% filter(Bimestre<10)




Rep <- leer(c("Materias_Inscritas_PPDP","SIR$"))


Rep <- filter(Base,!duplicated(Clave)) %>% add_count(MATRICULA,name = "Bimestres")


Rep2 <- filter(Rep,)





















