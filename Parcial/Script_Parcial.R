#######################################################################
# Parcial de Explotación de Datos             
# Fecha: 05/11/2023                                                  
#                                                                       
# Autor: Farias Gonzalo	- gonzafarias01@gmail.com                             
#######################################################################

# CARGA DE BIBLIOTECAS
library(dplyr)
library(xtable)
library(ggplot2)
library(corrplot)
library(readxl)
library(leaps)
library(car)
library(lmtest)
library(broom)
library(fmsb)
library(FactoMineR)
library(psych)
library(factoextra)
library(PerformanceAnalytics)
library(plotly)
library(philentropy)
library(viridis)
library(cluster)
library(pheatmap)
library(NbClust)
library(plyr)
library(rpart.plot)
library(caret)
library(tidyverse)
library(rsample)
library(e1071)
library(MASS)
library(ROCR)
library(gplots)
library(pROC)
library(rpart)
library(randomForest)
library(ggpubr)

rm(list=ls()) # Eliminacion de variables del entorno
gc()

options(scipen = 6) # Para evitar notacion cientifica.
# setwd("C:/Users/Alumnos/Documents/EDD/Parcial") # Seteamos entorno

# CARGA DE DATOS
datos <- read_delim("Datos/data.fb.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

# BUSQUEDA DE DATOS NULOS
# Visualizamos un resumen de datos nulos en el conjunto de datos.
# View(summarise_all(datos, funs(sum(is.na(.)))))
datos <- na.omit(datos) # Eliminamos las filas con valores nulos en caso de haber
# No hay datos nulos

# VEMOS LOS DATOS
View(datos)

# CASTEO DE DATOS
# Convertimos todas las columnas a tipo numerico para asegurarnos de que sean interpretables.
attach(datos)
datos <- datos %>% mutate_all(as.numeric)

# ANALISIS EXPLORATORIO DE DATOS
summary(datos)  # Estadasticas descriptivas

# Boxplot
boxplot(x1,
        main = "Boxplot de Datos",
        xlab = "Variable X",
        ylab = "Variable Y",
        col = "lightblue",
        border = "blue",
        horizontal = TRUE)

boxplot(x4,
        main = "Boxplot de Datos",
        xlab = "Variable X",
        ylab = "Variable Y",
        col = "lightblue",
        border = "blue",
        horizontal = TRUE)

boxplot(y,
        main = "Boxplot de Datos",
        xlab = "Variable X",
        ylab = "Variable Y",
        col = "lightblue",
        border = "blue",
        horizontal = TRUE)

boxplot(datos,
        main = "Boxplot de Datos",
        xlab = "Variable X",
        ylab = "Variable Y",
        col = "lightblue",
        border = "blue",
        horizontal = TRUE)
# Podemos ver como en todas las variables hay valores grandemente atipicos
# los boxplot son mayores a la media 0, teniendo maximos muy grandes a comparacion
# de los demas. Principalmente a x2


################################################################################################
# REGRESION PARA PREDECIR Y
set.seed(16) 
parte <- createDataPartition(datos$y, p=0.7, list=FALSE) 

train<-datos[parte,]
test<-datos[-parte,]

# Vemos la correlacion entre valores
corrplot(cor(datos), method = "number", tl.col = "black", tl.cex = 0.8)
# Vemos como hay una gran correlacion entre la mayoria

##  RLM para predecir "Y"
mod_full<-lm(y~.,data=train)
summary(mod_full)  

#aplicamos método de selección automática

# modStep_for <- step(mod_full, direction = "forward",trace=T) 
modStep_back <- step(mod_full, direction = "backward",trace=T)
# modStep_both <- step(mod_full, direction = "both",trace=F)
# summary(modStep_for)
summary(modStep_back)
# summary(modStep_both)
# Nos da formula con todas las variables, quiere decir que todas son necesarias para nuestra
# predicci�n
y_pred2 <- predict(object=modStep_back, newdata=test)
plot(test$y,y_pred2,pch=20,main="predichos backward",  xlab='Y', ylab=expression(hat(y)))
abline(a=0, b=1, col="green")

# Elegir modelo
modelo <- modStep_back
residuos = residuals(modelo)
summary(residuos)
hist(residuos) # Vemos si se da la campana de Gauss

################################################################################        
######################## VERIFICAR SUPUESTOS DE LA RLM ######################### 
par(mfrow=c(2,2))
plot(modelo) 

# 1. Linealidad: Dispersion sin patron alguno de residuos, viendo plots del modelo y residuos
plot(residuos)

# 2. homocedasticidad: verificar la homocedasticidad graficando los residuos estandarizados
# Grafico de Scale-Location
# Boxplot debe tener mediana 0 (de residuos)
boxplot(residuos,
        main = "Boxplot de Datos",
        xlab = "Variable X",
        ylab = "Variable Y",
        col = "lightblue",
        border = "blue",
        horizontal = TRUE)

# 3. Homogeneidad de la varianza: observar el gráfico de residuos vs. valores predichos. (Residuals vs Fitted)
# Si los puntos se distribuyen aleatoriamente alrededor de la línea de referencia, se cumple la suposición de homogeneidad de varianza.


# 4. Independencia de los residuos: usar test de Durbin Watson
# Errores independientes (no correlacionados, es equivalente si hay normalidad)
# Test Durbin Watson
# DW debe ser cercano a 2 y el p-value cercano a 1 para demostrarlo
# un pvalue muy pequenio indica que no hay independencia (están correlacionados)
# La hipotesis nula es que no hay autocorrelacion.
dwtest(modelo)  # Si el pvalor es chico, entonces hay dependencias entre residuos


# 5. Normalidad de los residuos: usar un grafico QQplot 
# Test de normalidad de Shapiro-Wilk (muestras chicas)
# Cuanto más cercano esté W a 1, más probable es que los datos se ajusten a una distribución normal. 
x.test <- shapiro.test(residuos)
print(x.test)


# 6. Multicolinealidad e influyentes:
# MULTICOLINEALIDAD
# VIF: Si es mayor a 10, entonces hay correlacion entre variables
# VIF = 1: NO HAY MULTICOLINEALIDAD
# VIF > 1: MULTICOLINEALIDAD ACEPTABLE
# VIF > 5: MULTICOLINEALIDAD ALTA
vif(modelo) 

#INFLUYENTES:
cooks=cooks.distance(modelo)
plot(cooks.distance(modelo))

# Resumen del modelo
summary(modelo)


## PREDICCION de nuevos datos
nuevo<-data.frame(x1=c(5000),
                  x2=c(7390),
                  x3=c(103),
                  x4=c(101),
                  x5=c(161),
                  x6=c(3001),
                  x7=c(2010),
                  x8=c(254))
#valores a predecir
predict(modelo,nuevo)




