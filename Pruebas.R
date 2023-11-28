sapply(c("MultiLEDS","dplyr","tidyr","lubridate","SynerLyst"), library,character.only=T)
radix(list(dir="C:/Users/jsalinba/Documents/Reportes"))

source("C:/Users/jsalinba/Documents/Reportes/Scripts/R/Unif_SabNR.R",encoding = "LATIN1")
source("C:/Users/jsalinba/Documents/Reportes/Scripts/R/Aprobacion Mae.R",encoding = "LATIN1")
source("C:/Users/jsalinba/Documents/Reportes/Scripts/R/Comparativo_NR_Pos.R",encoding = "LATIN1")
source("C:/Users/jsalinba/Documents/Reportes/Scripts/R/Materias_Foco_Mae.R",encoding = "LATIN1")





Base <- leer(c("cols.csv","Bases"))




#2021
# 2021-01-04
# 2021-03-01
# 2021-05-03
# 2021-07-05
# 2021-08-30
# 2021-10-25


#2022
# 2022-01-03
# 2022-02-28
# 2022-05-02
# 2022-07-04
# 2022-08-29
# 2022-10-24


#2023
# 2023-01-02
# 2023-02-27
# 2023-05-01
# 2023-07-03
# 2023-08-28
# 2023-10-23

# library(SynerLyst)
# 
# radix(list(dir="C:/Users/jsalinba/Documents/Reportes"))
# 
# "C:/Users/jsalinba/Documents/Reportes/Bases/Hist?ricos/Sabana/IA Sabana Posgrados 2022.csv"
# 
# Base <- leer("C:/Users/jsalinba/Documents/Reportes/Bases/Hist?ricos/Sabana/IA Sabana Posgrados 2022.csv")
# Base <- leer(c("IA Sabana Posgrados 2022.csv","Hist?ricos/Sabana$"))
# 
# 
# 
#install.packages("C:/Users/jsalinba/Documents/Reportes/Scripts/SynerLyst_0.1.0.tar.gz")


install.packages("C:/Users/jsalinba/Documents/Reportes/MultiLEDS_1.2.2.tar.gz")

devtools::install_github("YuerSebastian/MultiLEDS")




install.packages(c("tidyr","readr","readxl","RMySQL"))












