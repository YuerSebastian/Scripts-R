sapply(c("MultiLEDS","dplyr","tidyr","lubridate","vroom","stringi","data.table"), library,character.only=T)
diremail("C:/Users/jsalinba/Documents/Reportes","jsalinba@utel.edu.mx") #C:/Users/jsalinba/Documents/Reportes     //TUT-095/Licenciaturas/Retencion
#####----------Inicio----------#####
Rep <- leer(c("semanas retención","Históricos$"),sheet="Cols")[[1]]
Base <- unificar("Ret$",iden="nom") %>% select(all_of(Rep)) %>% filter(grepl("Más|Mas|Mae",Modalidad)) %>% filter(grepl("UTEL",Alianza))
Rep <- leer(c("semanas retención","Históricos$"),sheet="Calendario")
Base <- left_join(Base,Rep,"iden")
Rep <- leer(c("Histórico Retenciones2","Históricos$")) %>% mutate(fil = if_else(Bimestre=="Nov-Dic 23","fil","_")) %>%
  filter(fil!="fil") %>% select(-fil) %>% rbind(Base)
escribir(Rep,c("Histórico Retenciones2.csv","Históricos$"))
# Base <- vroom("C:/Users/jsalinba/Documents/Reportes/Bases/Históricos/Histórico Retenciones2.csv",",",col_select = Rep,col_types = cols(.default = "c"),
#               locale = locale(encoding = "LATIN1")) %>% filter(grepl("Más|Mas|Mae",Modalidad))
# Rep <- leer(c("semanas retención","Históricos$"),sheet="Calendario")
# Base <- left_join(Base,Rep,"iden") %>% filter(!is.na(`Semana Ret`))
#####----------Bajas----------#####
Rep <- leer(c("General Retención","Retención$"),sheet="BASE") %>% select(matricula,Semana,Bimestre,Estatus,modalidad) %>% filter(grepl("Más|Mas|Mae",modalidad)) %>%
  left_join(leer(c("semanas retención","Históricos$"),sheet="Orden"),"Bimestre") %>%
  mutate(sem=if_else(Semana!="Cierre de ciclo",gsub("Semana ","",Semana),"7")) %>% mutate_at(c("Orden","sem"),~as.integer(.)) %>% arrange(Orden,sem) %>%
  group_by(matricula) %>% mutate(mat=row_number()) %>% mutate(Est2 = Estatus,Estatus = Estatus[[1]]) %>% ungroup() %>% add_count(matricula) %>%
  select(matricula,Bimestre,Semana,Estatus) %>% unique() %>% mutate(Semana=if_else(grepl("Semana",Semana),gsub("emana ","",Semana),"Cierre Ciclo")) %>%
  unite(Clave,matricula,Bimestre,Semana)
#####----------Principal----------#####
Base <- leer(c("Histórico Retenciones2","Históricos$")) %>% unite(Clave,Matricula,Bimestre,`Semana Ret`,remove = F) %>% as.data.table()
Base <- left_join(Base,Rep,"Clave")
Base <- mutate(Base,Estatus = if_else(is.na(Estatus),ESTATUS,Estatus),`Semana Ret`=if_else(`Semana Ret`=="S0","S1",`Semana Ret`))

Rep <- leer("//Tut-095/REPORTES/ESTADOS/reglas_negocios.csv") %>% rename("Matricula"=MATRICULA) %>% filter(!duplicated(Matricula))
Base <- left_join(Base,Rep,"Matricula")

Rep <- leer(c("semanas retención","Históricos$"),sheet="Catalogo")
Base <- left_join(Base,Rep,"Estatus")

Rep <- leer(c("semanas retención","Históricos$"),sheet="Solicitudes")
Base <- left_join(Base,Rep,"Homologado")
#Bloque y semana
Base <- mutate(Base,Bloque = if_else(grepl("^B",`Bloque seguimiento`),paste0("B",substring(`Bloque seguimiento`,10,11)),
                                     if_else(grepl("^C",`Bloque seguimiento`),paste0("B",substring(`Bloque seguimiento`,14,15)),"_")),
               Bloque = gsub("0","",Bloque))#,`Semana Ret` = if_else(`Semana Ret`=="S0","S1",`Semana Ret`))
#Bloques faltantes
Rep <- leer("//Tut-095/REPORTES/ESTADOS/bloquesegmae.csv",col_select = c("Matricula"=matricula,B.ingreso)) %>% filter(!duplicated(Matricula)) %>%
  mutate(B.ingreso=substring(B.ingreso,1,2))
Base <- left_join(Base,Rep,"Matricula")
Base <- mutate(Base,Bloque=if_else(Bloque=="_",
                                   if_else(!is.na(B.ingreso),B.ingreso,"B1"),Bloque),
               Bloque=gsub("Q","B",Bloque))
#####----------País----------#####
Base <- mutate(Base,País=if_else(grepl("Colombia",Alianza),"Colombia",
                                 if_else(grepl("Ecuador",Alianza),"Ecuador",
                                         if_else(grepl("Perú",Alianza),"Perú",
                                                 if_else(grepl("USA",Alianza),"USA",
                                                         if_else(Alianza=="UTEL","México","ROLA"))))),
               Modalidad = if_else(grepl("Maestr[íi]a",Modalidad),"Maestría","Master"))
#####----------Conteos----------#####
Base <- count(Base,Bimestre,`Semana Ret`,Bloque,Estatus,Modalidad,Alianza,País,Regla_Negocio,Indicadores,`Estatus Ret`,`Tipo Solicitud`,name = "Total") %>%
  spread(`Estatus Ret`,Total) %>% gather(`Estatus Ret`,Total,11:16) %>% mutate(Total=replace_na(Total,0))
Base <- unite(Base,Clave,Bimestre,`Semana Ret`,Bloque,Modalidad,Alianza,Regla_Negocio,remove=F)
escribir(Base,c("Indicadores Dir.csv","Principales$"))
#####----------Niveles de riesgo (IA Sabana Posgrados)----------#####
Base <- vroom("C:/Users/jsalinba/Documents/Reportes/Bases/Principales/IA sabana Posgrados.csv",",",col_types = cols(.default = "c"),locale = locale(encoding = "LATIN1"),
              col_select = c("Bl"=Bloque.seguimiento,"M"=Modalidad,"B"=Bimestre,"S"=`Semana extracción`,"A"=Campus,"R"=Regla_Negocio,
                             Aprobado,Np,Nuncas,`Por aprobar`,Reprobó,"TC"=`Tipo Campus`,"EB"=Estado.Banner,"SM"=Status.Materia))
Base <- filter(Base,!M %in% c("Doctorado","_"),TC != "Global",EB %in% c("ADMITIDO","MATRICULADO","EGRESADO"),SM=="Activo") %>% select(-TC,-EB) %>%
  mutate(S = if_else(S %in% c("Cierre"),"Cierre Ciclo",S),
         Bl = stri_extract(Bl,regex = "[0-9]",mode = "last"), Bl = if_else(Bl=="0","9",Bl), Bl = if_else(is.na(Bl),"_",paste0("B",Bl)),
         M = if_else(M=="Máster","Master",M),
         A = if_else(A == "UTEL Estados Unidos","UTEL USA",
                     if_else(A == "UTEL Panamá","UTEL Panama",
                             if_else(A %in% c("UTEL Higher","UTEL Student"),"UTEL",A))),
         A = if_else(A %in% c("_",NA),"UTEL",A),
         R = if_else(R %in% c("_",NA),"UTEL",R)) %>%
  unite(Clave,B,S,Bl,M,A,R,remove = F) %>% as.data.table() %>% filter(!S %in% c("S7","S8","S9","S10","SS")) %>%
  mutate_at(8:12,~as.integer(.)) %>% mutate(`Total NR` = Aprobado+Np+Nuncas+`Por aprobar`+Reprobó) %>%
  rename("Bimestre"=B,"Bloque"=Bl,"Semana"=S,"Alianza"=A,"Regla Negocio"=R,"Modalidad"=M) %>%
  .[,.("Aprobado" = sum(Aprobado),
       "Np" = sum(Np),
       "Nuncas" = sum(Nuncas),
       "Por aprobar" = sum(`Por aprobar`),
       "Reprobó" = sum(Reprobó),
       "Total NR" = sum(`Total NR`)),
    by = .(Clave,Bimestre,Bloque,Semana,Alianza,`Regla Negocio`,Modalidad)] %>%
  mutate_all(~as.character(.))
diremail("C:/Users/jsalinba/Documents/Reportes")
fwrite(Base,"C:/Users/jsalinba/Documents/Reportes/Bases/Principales/Indicadores Dir NR.csv",bom = T)
#escribir(Base,c("Indicadores Dir NR.csv","Principales$")) No sirve por alguna razón xd
#####----------Contención y Preventivo----------#####
#México
Base <- leer(c("11vIZuywshSmTO5Ch2_wm3aEJ3KQIPAJSDy7x67t_FWw","gsheet.ID"),sheet="Posgrados May-Jun",
             col_select=c(Matricula,"Bloque"=Bloque.Ingreso,Modalidad,"Tipo CP"=tipo,"Recuperados"=...27))
Rep <- leer(c("11vIZuywshSmTO5Ch2_wm3aEJ3KQIPAJSDy7x67t_FWw","gsheet.ID"),sheet="Recuperados posgradosMay-jun",
            col_select=c(Matricula,"Semana Ret"=`Semana Activo`)) %>% filter(!is.na(Matricula))
Base <- left_join(Base,Rep,"Matricula") %>% mutate(`Semana Ret`=if_else(!is.na(`Semana Ret`),paste0("S",substring(`Semana Ret`,8,8)),NA),
                                                   `Semana Ret`=replace_na(`Semana Ret`,"Por recuperar"),
                                                   Bloque=substring(Bloque,1,2),
                                                   Recuperados=if_else(Recuperados!="Por recuperar","Recuperados","Por recuperar"),
                                                   País="México",
                                                   Modalidad=replace(Modalidad,Modalidad=="Máster","Master"))
#LATAM
#1Yl7bx4R8YsR-W919P49wOp5Kp-TeY2p6vuzvTXNqqgU
Rep <- leer(c("1qW5cLwt64-KF2uoBlA0p_381OdOySRFNex0_YIjfQ98","gsheet.ID"),sheet="BDD_Contenciòn",
            col_select=c(Matricula,"Bloque"=`Bloque Actual`,"Modalidad"=`Lic/Pos`,"Tipo CP"=Tipo,"Recuperados"=`Razón recuperado`,"Semana Ret"=`Semana R`,
                         "País"=Campus,"Hom"=Homologado)) %>%
  filter(Modalidad=="Posgrados") %>% mutate_at(c(5,6),~replace(.,. %in% c(NA,"-"),"Por recuperar")) %>%
  mutate(Bloque=paste0("B",substring(Bloque,10,11)),
         Bloque=gsub("0","",Bloque),
         Recuperados=if_else(Recuperados!="Por recuperar","Recuperados","Por recuperar"),
         `Semana Ret`=if_else(`Semana Ret`!="Por recuperar",gsub("0","",paste0("S",substring(`Semana Ret`,8))),"Por recuperar"),
         Modalidad = if_else(grepl("^mae",Hom,ignore.case = T),"Maestría",
                             if_else(grepl("^mas",Hom,ignore.case = T),"Master","Doctorado")),
         País=replace(País,País=="Rola","ROLA")) %>% select(-Hom)
#Uniendo ambas bases
Base <- rbind(Base,Rep)
#Reglas negocios
Rep <- leer("//Tut-095/REPORTES/ESTADOS/reglas_negocios.csv") %>% rename("Matricula"=MATRICULA) %>% filter(!duplicated(Matricula))
Base <- left_join(Base,Rep,"Matricula")
#Estatus general
Rep <- leer(c("Estatus General Alumnos","SIR$"),col_select=c("Matricula"=MATRICULA,"Estatus"=ESTATUS_CODE,"FE"=FECHA_ESTATUS,"FI"=FECHAINICIO)) %>%
  mutate_at(c("FE","FI"),~dmy(.)) %>% arrange(desc(FE),desc(FI)) %>% select(Matricula,Estatus) %>% filter(!duplicated(Matricula)) %>%
  mutate(Matricula=as.character(as.integer(Matricula)))
Base <- left_join(Base,Rep,"Matricula")
Base <- mutate(Base,Recuperados=if_else(Recuperados=="Por recuperar",Estatus,Recuperados))
#Contando
Base <- count(Base,Bloque,Modalidad,`Tipo CP`,Recuperados,`Semana Ret`,País,Regla_Negocio)
Base <- rename(Base,"Total"=n)

escribir(Base,c("Indicadores Dir CyP.csv","Principales$"))


















# Rep2 <- unificar(c("Históricos/NR$"," 2[123]"),
#                 col_select = c(Matricula,Bimestre,Semana,`Fecha extracción`,Aprobado,Np,Nuncas,"Por aprobar"=Por.aprobar,Reprobó,"Total NR"=Total.general))
# Rep <- Rep2 %>% unite(Clave,1:3) %>% arrange(desc(ymd(`Fecha extracción`))) %>% filter(!duplicated(Clave)) %>% select(-`Fecha extracción`)
# Rep <- left_join(Base,Rep,"Clave") %>% mutate_at(28:33,~as.integer(.)) %>% mutate_at(28:33,~replace_na(.,0)) %>% rename("Reprobó"=Reprobó)





# gsub("Ã¡","á",.)
# gsub("Ã©","é",.)
# gsub("Ã","í",.)
# gsub("Ã³","ó",.)
# gsub("Ãº","ú",.)
# gsub("Ã±","ñ",.)














