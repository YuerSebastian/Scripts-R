sapply(c("MultiLEDS","dplyr","tidyr","lubridate","SynerLyst"), library,character.only=T)
radix(list(dir="C:/Users/jsalinba/Documents/Reportes"))

source("C:/Users/jsalinba/Documents/Reportes/Scripts/R/Unif_SabNR.R",encoding = "LATIN1")
source("C:/Users/jsalinba/Documents/Reportes/Scripts/R/Aprobacion Mae.R",encoding = "LATIN1")
source("C:/Users/jsalinba/Documents/Reportes/Scripts/R/Comparativo_NR_Pos.R",encoding = "LATIN1")
source("C:/Users/jsalinba/Documents/Reportes/Scripts/R/Materias_Foco_Mae.R",encoding = "LATIN1")




sapply(c("data.table","dplyr","tidyr","lubridate","vroom"), library,character.only=T)
archs <- list.files("C:/Users/jsalinba/Documents/Reportes/Bases/Originales/SIR/Materias Inscritas PPDP",full.names = T)

Base <- bind_rows(lapply(archs, fread, colClasses = "character", encoding = "Latin-1") %>% setNames(basename(archs)))
Base <- mutate(Base, FECHA_INICIO = dmy(FECHA_INICIO),STUDY_PATH = as.integer(STUDY_PATH)) %>% group_by(MATRICULA) %>%
  arrange(desc(FECHA_INICIO)) %>% group_by(MATRICULA) %>% filter(!duplicated(MATERIA_PADRE)) %>%
  filter(STUDY_PATH == max(STUDY_PATH), ESTATUS_MAT == "RE", !grepl("SESO",MATERIA_PADRE), !is.na(NOMBRE_PROF))
Base <- group_by(Base,MATRICULA) %>% mutate(Programas = n_distinct(CODE_PROG)) %>% group_by(MATRICULA,CODE_PROG) %>%
  mutate(Materias = n_distinct(MATERIA_PADRE), Inicios = n_distinct(FECHA_INICIO))

Rep <- fread("C:/Users/jsalinba/Documents/Reportes/Bases/Originales/SIR/Historial_Academico_Compactado.csv",
             colClasses = "character", encoding = "Latin-1", select = c("MATRICULA","PROGRAMA","TOTAL")) %>%
  distinct(MATRICULA,PROGRAMA,.keep_all = T)
Base <- left_join(Base,Rep,join_by(MATRICULA == MATRICULA, CODE_PROG == PROGRAMA))

Base <- filter(Base, NIVEL %in% c("LI","MA","MS","DO"))


# Rep <- select(Base,MATRICULA,ESTATUS,TIPO,CAMPUS,NIVEL,CODE_PROG,PERIODO,FECHA_INICIO,CRN,PARTE_PERIODO,MATERIA,MATERIA_PADRE,GRUPO,NOMBRE_MATERIA,CALIFICACION,
#               NOMBRE_PROF,MATERIA_LEGAL,STUDY_PATH,Programas,Materias,Inicios,TOTAL) %>%
#   mutate(Bloque = if_else(NIVEL == "LI",
#                           if_else(Inicios > 25, "B25 y más",paste0("B",Inicios)),
#                           if_else(NIVEL == "DO",
#                                   if_else(Inicios > 6,paste0("Q",Inicios," (Revisar)"),paste0("Q",Inicios)),
#                                   if_else(Inicios > 9,paste0("B",Inicios,"(Revisar)"),paste0("B",Inicios)))))







Rep <- fread("C:/Users/jsalinba/Documents/Reportes/Bases/Originales/SIR/Historial_Academico_Compactado.csv", colClasses = "character", encoding = "Latin-1",
             select = c("MATRICULA","PROGRAMA","NIVEL","TOTAL","AVANCE")) %>%
  mutate(across(c(TOTAL,AVANCE),as.integer)) %>% filter(NIVEL %in% c("MA","MS","DO")) %>%
  mutate(mats = round(TOTAL*AVANCE/100),
         Bim = round((if_else(NIVEL == "DO",6,9) * mats) / TOTAL) + 1) %>% mutate(Bim = if_else(NIVEL == "DO" & Bim == 7,6,
                                                                                                if_else(Bim == 10,9,Bim)),
                                                                                  Bim = if_else(NIVEL == "DO",paste0("Q",Bim),paste0("B",Bim))) %>%
  select(MATRICULA,"B2"=Bim) %>% filter(!duplicated(MATRICULA)) %>% mutate(B2 = gsub("NA","1",B2))





Base <- fread("//Tut-095/REPORTES/ESTADOS/bloquesegmae.csv",encoding = "Latin-1",colClasses = "character") %>% select("MATRICULA"=matricula,"B1"=B.ingreso) %>%
  mutate(MATRICULA = ifelse(nchar(MATRICULA) < 9,paste0("0",MATRICULA),MATRICULA),
         B1 = substring(B1,1,2)) %>% left_join(Rep,"MATRICULA") %>% mutate(comp = if_else(B1==B2,"si","no")) %>% filter(comp=="no")



#fwrite(Rep2,"C:/Users/jsalinba/Documents/Reportes/Bases/Bloques Pos.csv",bom = T,quote = "auto")




























































