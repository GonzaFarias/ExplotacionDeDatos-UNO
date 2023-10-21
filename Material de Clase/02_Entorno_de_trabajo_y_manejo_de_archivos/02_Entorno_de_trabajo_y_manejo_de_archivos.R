
###############################################
# Tema: Aspectos basicos en R.
# clase: 01
# Fecha: 06/08/2022  
# version actual: 06/08/2022
# Autor: Mendoza, Dante.
###############################################

# Bibliotecas
library(readr) # importar archivos externos.

# Saber ubicacion del entorno de trabajo.
getwd()

# Setear el directorio de trabajo.
# setwd("C:/Proyectos/R/")

# Ver archivos
list.files()

# Ver directorios
list.dirs()

# Seteamos el entorno de desarrollo, ubicacion de nuestro carpeta
setwd(paste0(getwd(),"/" ))
getwd()

# SCRIPTS
# Los scripts son documentos de texto con la extensión de archivo .R, por ejemplo mi_script.R.
# Estos archivos son iguales a cualquier documentos de texto, pero R los puede leer y ejecutar el código que contienen.
source("02_Scripts_externo.R")

##### LEER UN ARCHIVO EXTERNO #####
# Presupuesto de la Administración Pública Nacional 2022.
# Fuente: https://www.datos.gob.ar/dataset/sspre-presupuesto-administracion-publica-nacional-2022
recursos_mensual_2022 <- read_csv("Datos/recursos-mensual-2022.csv")
View(recursos_mensual_2022)

##### DATA FRAME #####
# Los data frames son estructuras de datos de dos dimensiones (rectangulares) que pueden contener datos de diferentes tipos
# ,por lo tanto, son heterogéneas. Una matriz solo almacena un tipo de datos especifico.

# Convertir a data frame
df = as.data.frame(recursos_mensual_2022)
df1 = df

# Para eliminar columnas de un data frame en R,
# simplemente se asigna el valor NULL a la columna que se desea remover.
df1$ejercicio_presupuestario <- NULL

# Una alternativa es si indicamos que columna no queremos mantener.
df1 <- df [,-c(3)]

# Otra forma es indicar solo las columnas que queremos conservar
df1 <- df[, c("impacto_presupuestario_anio", "impacto_presupuestario_mes")] 

##### GUARDAR EN UN ARCHIVO #####
# Coma como separador y punto como separador decimal
write.csv(df, "Salida_recursos_mensual_2022_csv.csv")

# Punto y coma como separador y coma como separador decimal
write.csv2(df, "Salida_recursos_mensual_2022_csv2.csv")
