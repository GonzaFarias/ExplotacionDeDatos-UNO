
###############################################
# Tema: Aspectos basicos en R.
# clase: 01
# Fecha: 06/08/2022  
# version actual: 06/08/2022
# Autor: Mendoza, Dante.
###############################################

# Seteamos el entorno de desarrollo, ubicacion de nuestro carpeta
#setwd("C:/Proyectos/R/")
setwd(paste0(getwd(),"/" ))

inicio0 <- Sys.time()
options(scipen=6) #para evitar notacion cientifica
path0  <-  "Logs/" # Carpeta donde se alojaran los archivos Logs.

zz0 <- file(paste(path0, "00_log_", gsub("[:| |-]", "_", inicio0), ".txt", sep = ""), open="wt") # Se define el nombre de archivo y el tipo.
sink(zz0, type = "message")

message("###################################################\n")
message("PARAMETROS: \n")
message("INICIO: ", inicio0, "\n")
message("PATH: ", path0, "\n")
message("PATH_WD: ", getwd(), "\n")
message("###################################################\n")

message("LECTURA DE ARCHIVOS: \n")

#lee script R y bibliotecas
tryCatch(expr = {
  # bibliotecas
  library(dplyr)
  library(data.table)
  library(lubridate)
  message(" -- Bibliotecas cargadas correctamente\n")
},
error = function(e){
  message("\n------------------------------------------------------------------------------------------\n")
  message("Se encontraron errores en la carga de alguna(s) bibliotecas(s):\n")
  message(e)
  stop(".  (Se recomienda detener la ejecucion)\n\n------------------------------------------------------------------------------------------\n")
},
warning = function(e){
  message("\n------------------------------------------------------------------------------------------\n")
  message("Se encontraron warnings en la carga de alguna(s) bibliotecas(s)\n")
  message(e)
  message(".  (Si hay errores mas adelante, esta puede ser la causa)\n")
  message("\n------------------------------------------------------------------------------------------\n")
}
)

# leo los datos externos
tryCatch(expr = {
  library(readr)
  #leo el archivo que contiene datos externos de caracter publico
  datos_externos <- read_delim("Datos/datos_externos.csv", 
                               ";", escape_double = FALSE, col_types = cols(Comuna = col_character()), 
                               locale = locale(decimal_mark = ",", grouping_mark = ""),
                               trim_ws = TRUE)
  
  #View(datos_externos)

  message("\n[LECTURA] 'datos_externos.csv' leido correctamente [registros: ", nrow(datos_externos), "]\n")
},
error = function(e){
  message("\n------------------------------------------------------------------------------------------\n")
  message("Se encontraron errores en la lectura: 'datos_externos.csv':\n")
  message(e)
  stop(".  (Se recomienda detener la ejecucion)\n\n------------------------------------------------------------------------------------------\n")
},
warning = function(e){
  message("\n------------------------------------------------------------------------------------------\n")
  message("Se encontraron warnings en la lectura: 'datos_externos.csv'\n")
  message(e)
  message(".  (Si hay errores mas adelante, esta puede ser la causa)\n")
  message("\n------------------------------------------------------------------------------------------\n")
}
)

fin0 <- Sys.time()
TIEMPO_EJECUCION <- fin0 - inicio0

message("\n# FIN de ejecucion de SCRIPT en ", round(TIEMPO_EJECUCION, 2) ," segundos\n###################################################\n")

sink(type="message")

close(zz0)
