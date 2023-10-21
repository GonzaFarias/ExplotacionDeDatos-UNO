
###############################################
# Tema: COVID-19 infectados a nivel global.
# clase: 01
# Fecha: 06/08/2022  
# version actual: 06/08/2022
# Autor: Mendoza, Dante.
###############################################

rm(list = ls()) # Limpio el entorno.

# Funcion para comprobar bibliotecas a cargar / verificar
check_packages <- function(packages) {
  if (all(packages %in% rownames(installed.packages()))) {
    TRUE
  } else{
    cat(
      "Instalar los siguientes packages antes de ejecutar el presente script\n",
      packages[!(packages %in% rownames(installed.packages()))],
      "\n"
    )
  }
}

# Indicamos los paquetes que necesitemos.
packages_needed <- c("ggplot2", "ggrepel", "plotly",
                     "lubridate", "htmlwidgets" , "RColorBrewer",
                     "sqldf", "data.table", "readr" )

# Llamo a la funcion check_packages
check_packages(packages_needed)

# Indico los paquetes que voy a utilizar.
library(ggplot2)
library(ggrepel)
library(plotly)
library(lubridate)
library(htmlwidgets)
library(RColorBrewer)
library(sqldf)
library(data.table)
library(readr) # importar archivos externos.
#library(reshape2)

# Seteamos el entorno de desarrollo, ubicacion de nuestro carpeta
#setwd(paste0(getwd(),"/" ))

##### CARGAMOS LA FUENTE DE DATOS #####
# time_series_covid19_confirmed_global.csv
# Fuente original: 
# https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv

# Lestura desde la API web.
#URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
#url_archivo  <- paste(URL,"time_series_covid19_confirmed_global.csv", sep = "")
#COVID_19_h   <- read.csv(url_archivo, sep = ",", header = TRUE)

# Leemos el archivo local en formato CSV
#COVID_19_h <- read_csv("Datos/time_series_covid19_confirmed_global.csv")
COVID_19_h <- read.csv("Datos/time_series_covid19_confirmed_global.csv", sep = ",", header = T)

###### PREPARACION DE LOS DATOS #####

COVID_19_h$Lat  <- NULL 
COVID_19_h$Long <- NULL
COVID_19_h$Province.State <- NULL

#colnames(COVID_19_h)
#head(COVID_19_h, 5) 

library(tidyr)
# pasamos a formato vertical
COVID_19  <- COVID_19_h %>% gather(date, casos, 2:ncol(COVID_19_h))
# COVID_19  <- gather(COVID_19_h, date, casos, 2:ncol(COVID_19_h))

colnames(COVID_19) <- c(  "pais", "date", "confirmados")
class(COVID_19$date)
COVID_19$date <- as.Date(as.character(COVID_19$date), format = "X%m.%d.%y")

# agrupo por fecha 
casos_por_fecha <- COVID_19 %>% group_by(date) %>% summarise(confirmados = sum(confirmados))

# ----------------------------------------------------------------------------------------
# lo anterior  en SQL seria:
# casos_por_fecha <- sqldf( "select  date, sum(confirmados) confirmados
#                           from COVID_19
#                           group by date")
# ----------------------------------------------------------------------------------------

# Ordenamos
#library(tidyverse)
datos <- arrange(casos_por_fecha, (casos_por_fecha$date) )

###### EXPORTAR LA INFORMACION Y VISUALIZAMOS #####
# Genero figura dinamica

g1 <- ggplot(datos ,aes(x = date, y = confirmados/1000000)) +
  
  geom_point( size=1, color="blue") +
  
  ggtitle("COVID_19 - confirmados a nivel mundial") +
  scale_x_date(date_breaks = "21 day", date_labels =  "%d %b %Y") +
  #scale_y_continuous(limits = c(0, 100), breaks = seq(1, 5, 1)) +
  theme(plot.title = element_text(lineheight = 1,face ='bold'))   +
  ylab("cantidad de confirmados en M") +
  xlab("") +
  labs(caption = "\nFuente: The Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# + geom_text(aes(label = round(porDiezMil,1)), position = position_stack(vjust = .5))

g1 <- ggplotly(g1, tooltip = c("confirmados")) %>%
  layout(legend = list(
    orientation = "h",
    x = 0.7,
    y = 1
  )
  )

g1 # Visualizamos el grafico generado.







