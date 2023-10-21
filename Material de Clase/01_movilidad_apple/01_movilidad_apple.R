
################################################################
# Tema: Movilidad de las personas en la ciudad de Buenos Aires.
# Clase: 02
# Fecha: 18/08/2022  
# version actual: 18/08/2022
# Autor: Mendoza, Dante.
# Fuente Apple: https://covid19.apple.com/mobility
# Fuente Google: https://www.google.com/covid19/mobility/
###############################################################

options(allow_html=TRUE)

# Bibliotecas a importar
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
packages_needed <- c("readr", "ggplot2", "plotly", "tidyverse", "data.table")
check_packages(packages_needed)
library(readr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(data.table) # Para setnames


##### CARGAMOS LA FUENTE DE DATOS #####
#mobility_url <- "https://covid19-static.cdn-apple.com/covid19-mobility-data/2007HotfixDev47/v2/en-us/applemobilitytrends-"
#aux        <- paste(mobility_url,"2020-05-03.csv", sep = "")
#mobility   <- read.csv(aux, sep = ",", header = T)
library(readr)
mobility <- read_csv("Datos/applemobilitytrends-2020-12-10.csv", locale = locale(grouping_mark = ""))


###### PREPARACION DE LOS DATOS #####
mobility$region              <- as.factor(mobility$region)
mobility$transportation_type <- as.factor(mobility$transportation_type)
colnames(mobility)
levels(mobility$region)
levels(mobility$transportation_type)

library(tidyr)
datos_v   <- mobility     %>% gather(fecha, tasa    , 7:ncol(mobility))

datos_v$fecha     <- as.Date(as.character(datos_v$fecha))
colnames(datos_v)

# Si queremos ver otro pais modificamos aqui.
pais = "Buenos Aires"
trans = "walking"
datos <- subset(datos_v, region == pais & transportation_type == trans)


###### EXPORTAR LA INFORMACION Y VISUALIZAMOS #####
g1 <- ggplot(datos, aes(x = fecha, y = tasa) ) +
  geom_line(size = 0.6) +
  geom_line(linetype = "dashed") +
  ggtitle(paste("COVID_19 - Movilidad en ",pais,sep = "")) +
  scale_x_date(date_breaks = "7 day", date_labels =  "%d %b") +
  #scale_y_continuous(limits = c(0, 100), breaks = seq(1, 10, 1)) +
  theme(plot.title = element_text(lineheight = 1,face ='bold'))   +
  ylab("tasa de movilidad") +
  xlab("") +
  labs(caption = "Fuente de los datos: apple.com/covid19/mobility") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

g1 # Visualizamos el grafico.

# Exportamos el archivo.
write.csv2(datos, "Datos/datos_mobility_apple.csv", row.names = FALSE, fileEncoding = "UTF-8")
