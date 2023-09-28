
########################################################
# Tema: Visualización de la información.
# Materia: Explotación de Datos
# Fecha: 26/08/2023

# Autor: Farias Gonzalo	

# Fuente Ministerio de Cultura Argentina:
# https://datos.cultura.gob.ar/dataset/sector-musical
########################################################

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
packages_needed <- c("readr", "ggplot2")
check_packages(packages_needed)
# Cargamos las bibliotecas 
library(ggplot2)
library(readr)

# Cargamos los datos desde el archivo CSV
datos = read_csv("m06_ingresos-globales-ventas-musica-grabada-segmento.csv")

# Exportamos la información para visualizar las ventas
grafico = ggplot(datos, aes(x = indice_tiempo)) +
  geom_line(aes(y = ingresos_ventas_musica_streaming, color = "Streaming")) +
  geom_line(aes(y = ingresos_ventas_musica_descargas, color = "Descargas")) +
  geom_line(aes(y = ingresos_ventas_musica_formato_fisico, color = "Formato Físico")) +
  geom_line(aes(y = ingresos_derechos_comunicacion_publica, color = "Derechos de Comunicación Pública")) +
  geom_line(aes(y = ingresos_contratos_sincronizacion, color = "Contratos de Sincronización")) +
  labs(x = "Años", y = "Ventas en miles de millones de USD", title = "Ingresos globales por venta de música grabada según segmento (2001 - 2020)",
       caption = "Fuente de los datos: https://datos.cultura.gob.ar/dataset/sector-musical", color = "Segmento") +
  scale_x_date(limits = as.Date(c("2001-01-01", "2020-01-01")), date_labels = "%Y", date_breaks = "1 year") +
  scale_color_manual(values = c("Streaming" = "blue", "Descargas" = "green", "Formato Físico" = "red", 
                                "Derechos de Comunicación Pública" = "purple", "Contratos de Sincronización" = "orange")) +
  theme_bw()

grafico # Visualizamos el gráfico.
