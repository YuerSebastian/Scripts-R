library(dplyr);library(lubridate);library(tidyr);library(readr); library(googledrive); library(googlesheets4); library(readxl)
#install.packages("D:/Scripts/R/Proyectos/MultiLEDS_0.1.0.tar.gz",repos=NULL,type="source")
#Variables principales
drive_auth("jsalinba@utel.edu.mx"); gs4_auth("jsalinba@utel.edu.mx")
#####----------------------------------------Lectura----------------------------------------#####
leer_arch <- function(arch=c("",""),delim=NULL,...){
  c <- list.dirs("D:/Trabajo")
  if (length(arch)==1){ #Si es una dirección completa o en éste caso, el vector tiene longitud 1...
    x <- stringr::str_extract(arch,"\\..*")
    if (x==".csv") {
      x <- read_csv(arch,locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c"),...)
    }else if (x==".xlsx"){
      x <- read_excel(arch,col_types = "text",...)
    }
  }else{ #Si no, busca la dirección completa por un patrón.
    if (!stringr::str_detect(arch[2],"gsheet")) { #Si no se lee una hoja de google...
      dir <- list.files(c[grep(arch[2],c)],full.names = T) %>% .[grep(arch[1],.)]
      y <- stringr::str_extract(dir,"\\..*")
      if (y==".csv") {#Si es un csv...
        x <- read_csv(dir,locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c"),...)
      }else if (y==".xlsx"){#Si es un xlsx...
        x <- read_excel(dir,col_types = "text",...)
      }else if (y==".txt"){#Si es un txt...
        x <- read_delim(dir,locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c"),delim = delim...)
      }
    }else{#Si es una hoja de google, lee.
      if (stringr::str_detect(arch[2],".ID")) {#Si se está leyendo con un ID...
        x <- range_speedread(arch[1],col_types = cols(.default = "c"),...)
      }else{#Si no, busca por nombre el ID y lo lee, solo funciona para archivos propios no compartidos
        y <- gs4_find(arch[1]); y <- as.character(y$id); x <- range_speedread(y,col_types = cols(.default = "c"),...)
      }
    }
  }
  return(x) #Retorna la base leída.
}
#####----------------------------------------Impresión----------------------------------------#####
impr_arch <- function(base,arch=c("",""),sep = "\t",...){
  c <- list.dirs("D:/Trabajo")
  dir <- paste(c[grep(arch[2],c)],arch[1],sep="/")
  x <- stringr::str_extract(arch[1],"\\..*")
  if (x==".csv") {
    write.csv(base,dir,row.names = F,...)
  }
  else if (x==".txt"){
    write.table(base,dir,row.names = F,sep = sep...)
  }
}








