sapply(c("dplyr","tidyr","lubridate","RMySQL","readxl","readr","vroom","crayon","tools","SynerLyst"),require,character.only=T)
#####---------Tipos de mensaje---------#####
tip_mens <- function(fun = "leer", tip = "men"){
  #Para función leer
  if (fun == "leer") {
    if (tip=="men") {
      mens <- paste0("Nada")
      enc <- paste0("\n",strrep("=",nchar(strip_style(mens))),"\n")
      llam <- "message"
    }
    else if (tip == "adv"){
      mens <- paste0(yellow("Hay "),bold$blue(length("muchos")),yellow(" directorios con el patron "),bold$blue("Patrón"),yellow(".\nSe uso el primer elemento: "),
                     bold$blue("Primer elemento"))
      enc <- magenta(paste0("\n",strrep("=",nchar(strip_style(mens))),"\n"))
      llam <- "warning"
    }
    else if (tip == "det"){
      mens <- paste0("El ",bold("directorio")," que especificaste con el patron ",bold$blue("Algo$")," no existe.")
      enc <- magenta(paste0("\n",strrep("=",nchar(strip_style(mens))),"\n"))
      llam <- "stop"
    }
  }
  #Para otros
  else {
    mens <- paste0("Nada")
    enc <- paste0("\n",strrep("=",nchar(strip_style(mens))),"\n")
    llam <- "message"
  }
  eval(call(llam,enc,mens,enc))
}