library(MultiLEDS); library(dplyr); library(tidyr); library(lubridate); library(hms)
diremail("D:/Trabajo","jsalinba@utel.edu.mx")
#####-----------------Variables principales-----------------#####
fecha <- as.Date("2022-10-01"); tol <- as_hms("00:15:00"); tm <- as_hms("00:50:00")
#####-----------------Tiempos Validación histórico-----------------#####
Rep <- leer(c("Validación Tiempos","Bases$")) %>% separate(Horario,into = c("HSI","HSF"),sep=" a ") %>% separate(`Horario fin`,into = c("HFI","HFF"),sep=" a ")%>%
  mutate_at(c("HSI","HSF","HFI","HFF"),~paste(.,":00",sep="")) %>% mutate_at(c("HSI","HSF","HFI","HFF"),~as_hms(.))%>%
  mutate_at(c("HSI","HFI"),~as_hms(. - tol)) %>% mutate_at(c("HSF","HFF"),~as_hms(. + tol))%>%
  mutate(Días=gsub("Lu","1",Días),Días=gsub("Ma","2",Días),Días=gsub("Mi","3",Días),Días=gsub("Ju","4",Días),Días=gsub("Vi","5",Días),Días=gsub("Sa","6",Días),
         Días=gsub("Do","7",Días))

Base <- leer(c("Histórico Decisiones","SIR$")) %>% filter(USUARIO_DECISION %in% Rep[["Usuario"]])%>%
  mutate(HORA_DES=as_hms(HORA_DES),FECHA_DES=dmy(FECHA_DES)) %>% arrange(HORA_DES)%>%
  left_join(leer(c("Validación Tiempos","Bases$")) %>% select("USUARIO_DECISION"=Usuario,Nombre),by="USUARIO_DECISION")
#####-----------------Tiempos-----------------#####
i<-5
BG <- data.frame()
while (fecha<="2023-01-05") {
  Rep3 <- data.frame()
  for (i in 1:length(Rep[[1]])) {
    Rep2 <- filter(Base,FECHA_DES==fecha,USUARIO_DECISION==Rep$Usuario[i])
    if (length(Rep2[[1]])!=0) {
      #-----Horario semana o fin de semana-----#
      ds <- wday(fecha,week_start = 1) #Día de la semana que se mide
      dst <- stringr::str_split(Rep$Días[i],"-")[[1]] #Días de la semana que se trabaja
      if(ds %in% 1:5){
        hi <- Rep$HSI[i]; hf <- Rep$HSF[i]
      }else{
        hi <- Rep$HFI[i]; hf <- Rep$HFF[i]
      }
      #----------------------------------------#
      #-----Primeros y ultimos tiempos-----#
      Rep2 <- mutate(Rep2,`Primer Decisión`=as_hms(min(HORA_DES)),`Última Decisión`=as_hms(max(HORA_DES)),`Día Laboral`=ds,
                     `PD En Horario`=as_hms(min(filter(Rep2,HORA_DES>=hi,HORA_DES<=hf)$HORA_DES)),`UD En Horario`=as_hms(max(filter(Rep2,HORA_DES>=hi,HORA_DES<=hf)$HORA_DES)),
                     `PD Antes del Horario`=as_hms(min(filter(Rep2,HORA_DES<hi)$HORA_DES)),`UD Antes del Horario`=as_hms(max(filter(Rep2,HORA_DES<hi)$HORA_DES)),
                     `PD Después del Horario`=as_hms(min(filter(Rep2,HORA_DES>hf)$HORA_DES)),`UD Después del Horario`=as_hms(max(filter(Rep2,HORA_DES>hf)$HORA_DES)),
                     `PD Periodo En Horario`=as_hms(`PD En Horario`-(hi+tol)),`UD Periodo En Horario`=as_hms(`UD En Horario`-(hf-tol)),
                     `Periodo Antes del Horario`=as_hms(`UD Antes del Horario`-`PD Antes del Horario`),
                     `Periodo Después del Horario`=as_hms(`UD Después del Horario`-`PD Después del Horario`))%>%
        mutate_at(grep("^PD |^UD |^Periodo|^TT ",names(.)),~if_else(is.infinite(.),as_hms("00:00:00"),.))%>%
        mutate(`TT Fuera de Horario`=as_hms(`Periodo Antes del Horario`+`Periodo Después del Horario`),
               `Horario OK`=if_else(`Día Laboral` %in% dst,
                                    if_else(HORA_DES < hi,"Antes del horario",
                                            if_else(HORA_DES > hf,"Después del horario","En horario")),"No labora"),
               Intervalo="_",Periodo=hms(0),Tiempo="_") %>% add_count(name = "Decisiones")
      #----------------------------------------#
      for (j in 1:length(Rep2[[1]])) {
        #-----Periodos-----#
        if(j==1){#Si es la primer decisión o iteración...
          #Si "Horario OK" no es "En horario", "Intervalo" es "Fuera de horario", de lo contrario es "PD En Horario"
          if (Rep2$`Horario OK`[j]!="En horario") {
            Rep2$Intervalo[j] <- "Fuera de horario"
          }else{
            Rep2$Intervalo[j] <- as.character(Rep2$`PD En Horario`[j])
          }
        }else{#De lo contrario, si no es la primer decisión o iteración...
          #Si "Horario OK" no es "En horario", "Intervalo" es "Fuera de horario"...
          if (Rep2$`Horario OK`[j]!="En horario") {
            Rep2$Intervalo[j] <- "Fuera de horario"
            #Si "Horario OK" de la decisión anterior es "En horario","Periodo" es "00:00:00", de lo contrario resta "HORA_DES" actual menos "HORA_DES" anterior.
            if (Rep2$`Horario OK`[j-1]=="En horario") {
              Rep2$Periodo[j] <- hms(0)
            }else{
              Rep2$Periodo[j] <- as_hms(Rep2$HORA_DES[j] - Rep2$HORA_DES[j-1])
            }
          }else{ #De lo contrario, si "Horario OK" es "En horario"...
            #Si "Horario OK" de la decisión anterior no es "En horario", "Intervalo" es "PD En Horario" y "Periodo" es "00:00:00",
            #de lo contrario escribe en "Periodo" HORA_DES" anterior "a" "HORA_DES" actual, dando un intervalo en texto y resta "HORA_DES" actual menos "HORA_DES" anterior.
            if (Rep2$`Horario OK`[j-1]!="En horario") {
              Rep2$Intervalo[j] <- as.character(Rep2$`PD En Horario`[j])
              Rep2$Periodo[j] <- hms(0)
            }else{
              Rep2$Intervalo[j] <- paste(Rep2$HORA_DES[j-1],"a",Rep2$HORA_DES[j],sep=" ")
              Rep2$Periodo[j] <- as_hms(Rep2$HORA_DES[j] - Rep2$HORA_DES[j-1])
            }
          }
        }
      }
      Rep2 <- mutate(Rep2,Tiempo=if_else(`Horario OK`=="En horario",
                                         if_else(Periodo>=tm,"Muerto","Trabajado"),"Fuera de horario"),
                     `ND En Horario`=sum(Tiempo=="Trabajado"),`ND Fuera de Horario`=sum(Tiempo=="Fuera de horario"),
                     `No. Tiempos Muertos`=as.integer(sum(Tiempo=="Muerto")),
                     `No. Tiempos Muertos`=if_else(`PD Periodo En Horario` > tm & `UD Periodo En Horario` < (tm*-1),`No. Tiempos Muertos`+2L,
                                                   if_else(`PD Periodo En Horario` > tm | `UD Periodo En Horario` < (tm*-1),`No. Tiempos Muertos`+1L,`No. Tiempos Muertos`)))%>%
        mutate(`TT En horario`=as_hms(sum(filter(.,Tiempo=="Trabajado")$Periodo)),`TM Total`=as_hms(sum(filter(.,Tiempo=="Muerto")$Periodo)),
               `TM Total`=if_else(`PD Periodo En Horario` > tm & `UD Periodo En Horario` < (tm*-1),
                                  as_hms(`PD Periodo En Horario`+(`UD Periodo En Horario`*-1)+`TM Total`),
                                  if_else(`PD Periodo En Horario` > tm,as_hms(`PD Periodo En Horario`+`TM Total`),
                                          if_else(`UD Periodo En Horario` < (tm*-1),as_hms((`UD Periodo En Horario`*-1)+`TM Total`),`TM Total`))),
               `TT Trabajado`=as_hms(`TT En horario`+`TT Fuera de Horario`))
    }
    if (length(Rep3)==0) Rep3 <- Rep2 else Rep3 <- rbind(Rep3,Rep2)
  }
  if (length(BG)==0) BG <- Rep3 else BG <- rbind(BG,Rep3)
  fecha <- fecha+1
};remove(Rep2,Base,Rep3)
Rep <- leer(c("Validación Tiempos","Bases$"),sheet="Columnas")
BG <- `colnames<-`(BG,Rep$Nuevo) %>% .[c(Rep$Incluir[!is.na(Rep$Incluir)])]
BG <- mutate_all(BG,~as.character(.))


escribir(BG,c("Tiempos Decisiones completo.csv","Bases$"))




















