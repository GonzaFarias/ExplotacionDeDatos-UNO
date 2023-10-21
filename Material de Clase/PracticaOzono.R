
##############################################################
# Analisis de componentes principales sobre el dataset ozono.#
#                                                            #
# Creado: 10/09/2022                                         #
# Version: 1.0                                               #
# Autor: Mendoza, Dante.                                     #
#                                                            #
##############################################################

# se procede a utilizar el dataset llamado "Ozono", con informacion sobre los vientos y la temperatura desde
# 1998 hasta 2004 inclusive.
# Los autores originales del dataset son:
# Kun Zhang, Department of Computer Science, Xavier University of Lousiana.
# Wei Fan, IBM T.J.Watson Research.
# XiaoJing Yuan, Engineering Technology Department, College of Technology, University of Houston

##### Importamos las biliotecas #####
library(readxl)
library(corrplot)
library(PerformanceAnalytics)
library(psych)
library(rela)
library(dplyr)
library(ggplot2)
library(plotly)

options(scipen = 6) # para evitar notacion cientifica.


##### Lectura y verificacion de la informacion leida #####
# Seteamos el entorno de desarrollo.
#setwd("C:/Users/Alumnos/Documents/EDD")

# Importamos la fuente de datos.
data <- read_excel("ozono.xls", na = "?")

# Se verifica la informacion leida del archivo fuente.
head(data, 5)

ncol(data) # Cantidad de variables.
nrow(data) # Cantidad de casos.
View(data)

##### Preparacion de los datos cargados #####
# Numero de casos con null mediante iteracion con Sapply.
sapply(data, function(x) sum(is.na(x)))

# En este caso se eliminaran los casos en nulos pero siempre dependera de la cantidad y el caso de estudio.
data <- na.omit(data)
# Buscar tecnicas de tratamiento para gran cantidad de datos nulos
# Volvemos a iterar con Sapply para ver si tenemos valores nulos.
sapply(data, function(x) sum(is.na(x)))
nrow(data) # Cantidad de casos actuales, antes teniamos 2534.

# Hacemos un resumen de los datos.
summary(data)


##### Se analiza la informacion cargada #####
# Se ejecuta la matriz de correlaciones.
cor_data <- cor(data[, -1]) # No tenemos en cuenta la columna de fechas.

# Recordemos que en Azul cuando es correlacion positiva y Rojo para correlacion negativa.
# El tamaño de la esfera indica si esta muy correlacionado o si existe poca correlacion.
corrplot(cor_data) 
# Vemos que el grafico de correlacion es practicamente ilegible al tener una gran cantidad
# de variables
#chart.Correlation(data[, -1]) # Graficar correlaciones e histogramas.

# Test de Barlett
# Como el p.value es 0 se rechaza la hipotesis nula que afirma que las variables no
# estan correlacionadas, y se continua con el ACP.
# Puede tambien ser un valor cercano a cero.
cortest.bartlett(cor_data, n= 1847) # n es el tamaño de la muestra.

# Test de KMO
# Este indice Kaiser-Meyer-Olkin (KMO) sirve para comparar los valores
# de correlacion de las variables y sus correlaciones parciales. Si el indice KMO
# es cercano a 1, significa que puede hacerse el analisis de componentes principales.
KMO(data[, -1])

# El test de Barlett y el test de KMO, ambos permiten determinar la correlacion de las variables.
# Averiguar cual de ellos esta enfocado a ser utilizado con muchas ovariables y la otra con pocas.

# Componentes Principales
cp <- prcomp(data[,-1], scale = TRUE) # Escala los numeros de esta matriz de cero a uno.

#Resumen de los Componentes Principales
summary(cp)

# Grafico de Sedimentacion de todas las CP.
plot(cp, 
     type="l", 
     main="CP mas Significativas con Plot",
     col=c("blue4"))
abline(1,0,col=c("brown3")) # Coloco una linea horizontal en el valor uno del eje y.
# 
# Usamos un grafico de barras.
library(factoextra)
fviz_screeplot(cp, 
               addlabels = TRUE, 
               ylim = c(0, 42),
               main="CP mas significativas con Screeplot")



# Biplot
biplot(x = cp, scale = 0, cex = 0.6, col = c("grey", "brown3"))

# Biplot con puntos. Se ven las variables y los casos.
# El grafico entre la Componente Principal 1 y 2, se puede apreciar
# dos grandes agrupamientos de variables, indicando correlacion positiva en
# cada grupo, y que estos grupos estan de forma perpendicular, indicando correlacion nula
biplot(x = cp, scale = 0, cex = 0.6, xlabs=rep(".", nrow(data)),col = c("grey", "brown3"))
