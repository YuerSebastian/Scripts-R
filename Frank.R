sapply(c("MultiLEDS","dplyr","tidyr","lubridate"),library,character.only=T)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#####-------Inicio-------#####
Base <- leer(c("Historial Académico Compactado","SIR$")) %>% mutate_at(9:14,~as.integer(.)) %>% mutate_at(9:14,~replace_na(.,0)) %>%
  mutate(`Avance siguiente`=round((APROBADAS + EN_CURSO) / TOTAL*100,2),
         `Avance 70`=if_else(NIVEL=="LI",
                             if_else(AVANCE < 70 & `Avance siguiente`>=70,"Si","No"),"No aplica"),
         `Avance siguiente`=replace(`Avance siguiente`,is.nan(`Avance siguiente`),0.00),
         `Avance 70`=replace_na(`Avance 70`,"No")) %>% filter(`Avance 70`!="No aplica") %>%
  mutate_all(~as.character(.)) %>% mutate_all(~replace_na(.,""))

escribir(Base,c("Alumnos Avance 70.csv","Principales$"))