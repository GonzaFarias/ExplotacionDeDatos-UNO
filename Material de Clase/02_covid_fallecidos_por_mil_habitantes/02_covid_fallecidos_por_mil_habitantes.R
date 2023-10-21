
###############################################
# Tema: COVID-19 infectados con valores relativos por cada 1.000 habitantes.
# clase: 02
# Fecha: 18/08/2022  
# version actual: 18/08/2022
# Autor: Mendoza, Dante.
###############################################

# NOTA: se solucionó el problema de regionalizacion de China, Canada y Australia.
# como contraparte, se consideraron los territorios de ultramar como pertenecientes a sus metropolis

rm(list = ls()) # Limpiar entorno

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
packages_needed <- c("ggplot2", "ggrepel", "plotly", "sqldf",
                     "lubridate", "htmlwidgets" , "RColorBrewer",
                     "grid", "data.table", "readr" )

# Llamo a la funcion check_packages
check_packages(packages_needed)

# Indico los paquetes que voy a utilizar.
library(ggplot2)
library(ggrepel)
library(plotly)
library(sqldf)
library(lubridate)
library(htmlwidgets)
library(RColorBrewer)
library(grid)
library(data.table)
library(readr) # importar archivos externos.
# library(reshape2)

# Seteamos el entorno de desarrollo, ubicacion de nuestro carpeta
#setwd(paste0(getwd(),"/" ))

##### CARGAMOS LA FUENTE DE DATOS #####
# time_series_covid19_confirmed_global.csv
# Fuente original: 
# https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv

# Lestura desde la API web.
#URL          <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
#url_archivo  <- paste(URL,"time_series_covid19_confirmed_global.csv", sep = "")
#COVID_19_h   <- read.csv(url_archivo, sep = ",", header = T)

# Leemos el archivo local en formato CSV
#COVID_19_h <- read_csv("Datos/time_series_covid19_confirmed_global.csv")
COVID_19_h <- read.csv("Datos/time_series_covid19_confirmed_global.csv", sep = ",", header = T)

###### PREPARACION DE LOS DATOS #####
COVID_19_h$Lat  <- NULL
COVID_19_h$Long <- NULL
COVID_19_h$Province.State <- NULL
colnames(COVID_19_h)

setnames(COVID_19_h , 
         old = c( "Country.Region" ), # Fijarse el nombre de la columna en la fuente.
         #old = c( "Country/Region" ), 
         new = c( "pais"))

# Pasar de formato "anchos" a "largo"
# Tres posibilidades para hacer lo mismo, reshape, melt y gather

library(tidyr)
COVID_19 <- COVID_19_h %>% gather(fecha, casos, 2:ncol(COVID_19_h))
# COVID_19 <-  gather(COVID_19_h, fecha, casos, 2:ncol(COVID_19_h))

# Convertimos el formtao de fecha
COVID_19$fecha <- as.Date(as.character(COVID_19$fecha), format = "X%m.%d.%y")

#  El primer agrupamiento es para sumarizar los casos de Australia, Canada y China que
#  vienen separadas por region. Otros casos son los territorios de ultramar de UK, 
#  Francia, Dinamarca y Holanda que son de muy pocos habitante / casos.
#  nada de lo anterior influye en el ranking  de los 25 peores casos.
#  otra idea seria no contar los casos  de los territorios de Ultra mar por pertenecer
#  a otras regiones.

COVID_19 <- COVID_19 %>% group_by(pais,fecha) %>% summarise(casos = sum(casos))

# El siguiente agrupamiento es para quedarde con el max de la variable acumulativa
casos_por_pais <- COVID_19 %>% group_by(pais) %>% summarise(casos = max(casos))
# sum(casos_por_pais$casos)

# Normalizo nombres de paises para compatibilidad de archivo habitantes que importaremos localmente.
library(tidyverse) 

library(dplyr)
casos_por_pais <- casos_por_pais %>% 
  mutate(pais = str_replace(pais,  "Korea\\, South"      , "South Korea")) %>%
  mutate(pais = str_replace(pais,  "Congo \\(Kinshasa\\)", "Democratic Republic of the Congo"))%>%
  mutate(pais = str_replace(pais,  "Taiwan\\*"           , "Taiwan")) %>%
  mutate(pais = str_replace(pais,  "US"                  , "United States of America")) %>%
  mutate(pais = str_replace(pais,  "Brunei"              , "Brunei Darussalam"))%>%
  mutate(pais = str_replace(pais,  "Cote d'Ivoire"       , "Costa de Marfil"))%>%
  mutate(pais = str_replace(pais,  "Holy See"            , "Vatican City"))%>%
  mutate(pais = str_replace(pais,  "Czechia"             , "Czech Republic"))%>%
  mutate(pais = str_replace(pais,  "Diamond Princess"    , "crucero Diamond Princess"))%>%
  mutate(pais = str_replace(pais,  "MS Zaandam"          , "crucero MS Zaandam"))%>%
  mutate(pais = str_replace(pais,  "Timor-Leste"         , "East Timor"))   

# idea: unir todos los cruceros y unirlo como un nuevo pais

library(readr)
habitantes <- read_delim("Datos/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.csv", 
                         ";", escape_double = FALSE, col_types = cols(cantidad = col_number()), 
                         locale = locale(grouping_mark = "", encoding = "WINDOWS-1252"), 
                         trim_ws = TRUE)

# Se preparan los datos de poblaciones
colnames(habitantes)

habitantes <- select(habitantes, pais, codigo , cantidad )

setnames(habitantes, "cantidad", "cantHabitantes")

datos <- merge(habitantes, casos_por_pais)

# Se Crea variable nueva con la tasa por cada 1.000 o 100.000 habitantes segun conveniencia.
options(scipen = 6) # para evitar notacion cientifica
datos$porMil <- datos$casos * 1000 / datos$cantHabitantes

# Verificar si hay algun pais que no se junta por diferir su nombre o por no ser pais (buques en alta mar)
# quedo afuera Cruise Ship por tratarse de casos controlados asumo que es correcto 
# no tenerlo en cuenta a estos casos especiales.

no_estan <- sqldf("select c.pais
                   from casos_por_pais c 
                   where not exists (select '1'
                                    from datos d
                                    where c.pais = d.pais)")
no_estan

# Ordenamos
datos_t <- arrange(datos, desc(datos$porMil) )

###### EXPORTAR LA INFORMACION Y VISUALIZAMOS #####
# grabar los datos en un archivo .csv
write.csv2(datos, "Datos/02_covid_fallecidos_por_mil_habitantes.csv",  row.names = FALSE, fileEncoding = "UTF-8")

# para graficar los mas importantes
datos <- sqldf( "select *  
                 from datos_t
                 LIMIT 25 ")

# Se genera la figura dinamica

g1 <- ggplot(datos ,aes(x = reorder(pais, porMil) , y = porMil, label = casos ) ) +
  geom_segment(size = 0.08, aes(xend = pais, yend=0))+
  coord_flip() +    # para girar el  grafico
  geom_point( size=2, color="orange") +
  ggtitle("COVID_19 - Confirmados por cada 1.000 habitantes") +
  theme(plot.title = element_text(lineheight = 1,face ='bold'))   +
  ylab("confirmados por cada 1.000 habitantes") +
  xlab("") +
  labs(caption = "\nFuente: The Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = round(porMil,1)), position = position_stack(vjust = .5))

g1 <- ggplotly(g1, tooltip = c("casos")) %>%
  layout(legend = list(
    orientation = "h",
    x = 0.7,
    y = 0
  )
  )

g1 # Mostramos el grafico generado.
