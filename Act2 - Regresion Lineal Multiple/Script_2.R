#######################################################################
# Tema: Regresión Lineal Multiple               
# Materia: Explotación de Datos                                         
# Fecha: 04/11/2023                                                  
#                                                                       
# Autores:                                                              
# Farias Gonzalo	- gonzafarias01@gmail.com     
# Robledo Alan	-	robledoezequiel609@gmail.com   
# Romano Diego	 -	romanodiegoe@gmail.com                                
#
# Fuente: World Happiness Report - Data                            
# https://worldhappiness.report/data/
#######################################################################

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

#para limpieza de memoria:
rm(list=ls())
gc()
# Cargar de datos
datos <- read_excel("felicidad_mundial.xls")
datos <- subset(datos, year == 2014) # Establecemos un solo año para hacer nuestra regresión.
datos <- subset(datos, select = -c(1,2))

# Eliminamos las columnas "País","año" y renombramos las columnas para mayor claridad
colnames(datos)<-c("SatisfaccionVida", "LogPBI", "ApoyoSocial", "EsperanzaVidaSaludableNacer", "LibertadTomarDecisionesVida", "Generosidad", "PercepcionCorrupcion", "AfectoPositivo", "AfectoNegativo", "ConfianzaGobiernoNacional")
View(datos)

# BÚSQUEDA DE DATOS NULOS
# Visualizamos un resumen de datos nulos en el conjunto de datos.
# View(summarise_all(datos, funs(sum(is.na(.)))))
datos <- na.omit(datos) # Eliminamos las filas con valores nulos en caso de haber

# CASTEO DE DATOS
# Convertimos todas las columnas a tipo numérico para asegurarnos de que sean interpretables.
attach(datos)
datos <- datos %>% mutate_all(as.numeric)

################################################################################        
####################### ANÁLISIS EXPLORATORIO DE DATOS ######################### 

summary(datos)  # Estadísticas descriptivas

## Vemos correlaciones y graficamos:
datos_cor <- cor(datos)  # Calculamos y almacenamos las correlaciones
corrplot(datos_cor, method = "number", tl.col = "black", tl.cex = 0.8)
plot(datos)

## Metodos automaticos para seleccion de variables, probamos cual es mejor:
##  "backward" / "forward" / "both"
mod_full<-lm(SatisfaccionVida~.,data=datos)
summary(mod_full)

summary(step(mod_full, direction = "forward",trace=F) )
summary(step(mod_full, direction = "backward",trace=F))
summary(step(mod_full, direction = "both",trace=F))

# Elegimos el modelo
modelo <- step(mod_full, direction = "backward",trace=T) # Optamos por el modelo con backward que nos dió mejores resultados

ggplot(modelo, aes(x=LogPBI + ApoyoSocial + EsperanzaVidaSaludableNacer + 
                     LibertadTomarDecisionesVida + PercepcionCorrupcion + AfectoPositivo + 
                     ConfianzaGobiernoNacional, y=SatisfaccionVida))+ 
  geom_point() +
  geom_smooth(method='lm',se=FALSE, col='green') +
  theme_light()

residuos = residuals(modelo)
summary(residuos)
hist(residuos) # Vemos si se da la campana de Gauss

################################################################################        
######################## VERIFICAR SUPUESTOS DE LA RLM ######################### 
par(mfrow=c(2,2)) # Para ver los 4 plots del modelo juntos
plot(modelo) 
par(mfrow=c(1,1)) # Reestablecemos a la vista normal

# 1. Linealidad: Dispersion sin patron alguno de residuos, viendo plots del modelo y residuos
plot(residuos)

# 2. homocedasticidad: verificar la homocedasticidad graficando los residuos estandarizados
# Grafico de Scale-Location
# Crear un boxplot personalizado debe tener mediana 0 (de residuos)
boxplot(residuos,
        main = "Boxplot de Residuos",
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

# 5. Normalidad de los residuos: usar un gráfico QQplot 
# Test de normalidad de Shapiro-Wilk (muestras chicas)
# Cuanto más cercano esté W a 1, más probable es que los datos se ajusten a una distribución normal. 
# Si el p-valor es mayor que el nivel de significancia (ejemplo 0.05), se asume que los datos siguen una distribución normal.
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
# Si el valor de F es grande y el valor p es chico indica que el modelo de regresión es significativo 
# y explica de manera efectiva la variabilidad en los datos.
# Se busca un alto R cuadrado ajustado y significatividad en las variables.

## PREDICCION de nuevos datos
nuevo <- data.frame(
  LogPBI = 7.65,
  ApoyoSocial = 0.52,
  EsperanzaVidaSaludableNacer = 53.2,
  LibertadTomarDecisionesVida = 0.50,
  Generosidad = 0.10,
  PercepcionCorrupcion = 0.87,
  AfectoPositivo = 0.49,
  AfectoNegativo = 0.37,
  ConfianzaGobiernoNacional = 0.40
)
#valores a predecir
predict(modelo,nuevo)
# Pusimos datos casi identicos a la primera fila de los originales y nos dió como resultado
# un valor similar, el valor original es de 3.13 y la predicción de 3.29
