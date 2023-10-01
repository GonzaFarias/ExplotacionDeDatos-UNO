
###############################################
# Tema: Discretizar variables
# Fecha: 30/09/2023
# Autor: Farias Gonzalo.
###############################################

rm(list = ls()) # Limpiar entorno
options(scipen = 6) # para evitar notacion cientifica.

# Indico los paquetes que voy a utilizar.
library(dplyr) 
library(readr)


##### CARGAMOS LA FUENTE DE DATOS #####
# Leemos el archivo local en formato CSV
country_wise_latest <- read_csv("country_wise_latest.csv")

View(country_wise_latest)
summary(country_wise_latest) # Realizamos un summary para tratar con algun criterio segun los quartiles
							 # para discretizar la cantidad de contagios por país

# Para discretizar
country_wise_latest$Nivel_contagios <- 1 # Agrego columna que contendra el nivel de gravedad

country_wise_latest <- country_wise_latest %>%  
  mutate(Nivel_contagios  = case_when(.$Confirmed <= 1000 ~ "Muy bajo",
                                   .$Confirmed <= 10000 ~ "Bajo",
                                   .$Confirmed <= 80000 ~ "Medio",
                                   .$Confirmed <= 150000 ~ "Alto",
                                   TRUE ~ "Muy Alto"))

View(country_wise_latest) # Visualizamos como quedó la tabla con la nueva columna de nivel de gravedad 
# según cantidad de contagiados
