library(readxl)

Docs <- read_excel("D:/Cosa/Validación UNIMINUTO.xlsx")
est <- c("VALIDADO","NO RECIBIDO","EN VALIDACION","NO ACEPTADO")
Docs$Tip <- ifelse(Docs$`DOCUMENTO IDENTIFICACIÓN`==est[1]&
                     Docs$EPS==est[1]&
                     Docs$`ACTA GRADO PROFESIONAL`==est[1]&
                     Docs$`HABEAS DATA`==est[1]&
                     Docs$`CONTRATO DE MATRÍCULA`==est[1]&
                     Docs$CERTIFICACIÓN==est[1],"Completos","nada")

descargas <- list.files ("D:/Cosa/Asignación prueba/FLOKZU", pattern = "*.xlsx", full.names = TRUE)
for (i in 1:length(descargas)) {
  FLOK_Aux <- read_excel(descargas[i], skip = 1)
  if(i==1) Base_FLOK <- FLOK_Aux else Base_FLOK <- rbind(Base_FLOK,FLOK_Aux)
}

openxlsx::write.xlsx(Base_FLOK, file = "D:/Cosa/Asignación prueba/Asignación_Analistas/PSBAJA.xlsx",sheetName="nuevos",row.names = F)

cosa <- read_excel("D:/Cosa/Asignación prueba/Asignación_Analistas/PSBAJA.xlsx")