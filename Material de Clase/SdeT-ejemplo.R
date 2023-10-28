# clase de Series de Tiempo
# objetivos:  
# autor: Silvia Pérez
# fechas: 28/10/2023

rm(list=ls())
gc()
######Cargamos paquetes a utilizar##########
library(tidyverse)
library(lubridate) #para manejar fechas
library(dplyr)
library(forecast)
library(tseries)

# RECORDEMOS funcion para normalizar nombres de columnas
dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+', '_', tolower(names))
  names = make.names(names, unique = TRUE, allow_ = TRUE)
  names = gsub('.', '_', names, fixed = TRUE)
}
# Los datos de ventas de cemento son indicadores de construcción en el país
# fuente: https://datos.gob.ar/dataset/produccion-archivo-2002-2019-indicadores-sectoriales-construccion
#Inversión: despachos de cemento por tipo de envase en formato de series (unidad=tn)
construccion<- read_delim("cemento-series.csv", 
                            ",", escape_double = FALSE, 
                          col_types = cols(tiempo = col_date(format = "%Y-%m-%d")), 
                            locale = locale(decimal_mark = ","), 
                            trim_ws = TRUE)
colnames(construccion) = dbSafeNames(colnames(construccion))
View(construccion)

# #Transformamos los datos en una serie temporal 
construccion.ts<- ts(construccion[, 2:3], start = c(2011,1), frequency = 12) #para verlas juntas
bolsa.ts<- ts(construccion[, 2], 
              start = c(2011,1), #Inicio de la serie como vector c(año, mes)
              end=c(2019, 12), #Fin de la serie
              frequency = 12) #La series es mensual
granel.ts <- ts(construccion[, 3],   start = c(2011,1), frequency = 12)

###PASO 0: graficar la serie 
library(ggfortify) #Requerida para pasar un objeto decomposed.ts a data.frame. Importa método autoplot.
#Graficamos las 2 series de tiempo juntas 
library(scales)
autoplot(construccion.ts) 

###PASO 1: descomponer la serie y graficar componentes
##Aplicamos una descomposición aditiva de la serie (por default hace la aditiva)
componentes<-decompose(bolsa.ts)
str(componentes) 

#Gráfico estándar de la serie descompuesta. NO ggplot2.
plot(componentes)
#Gráficos de la serie de tiempo descompuesta con ggplot2. 

library(hrbrthemes)         #Importa theme_ipsum() y scale_y_comma()
fortify(decompose(bolsa.ts)) %>% #Convierte a la serie de tiempo descompuesta a data.frame             
  rename("Datos"=Data,                        #Aquí cambio los nombres de columna.
         "Estacional"=seasonal, 
         "Tendencia"=trend, 
         "Residuo"=remainder) %>%
  gather(variable, valor, -Index) %>%  #Paso a formato largo con gather(), mantengo Index.
  ggplot(aes(x=Index, y=valor)) +      #Mapeo x y y 
  geom_line() +                               
  facet_grid(variable~., scales="free") +   #especifico los paneles. facet_grid etiquetado horizontal
  labs(title="Ventas de cemento en Argentina", 
       subtitle="Descomposición aditiva", 
       caption="Datos Ministerio Producción", 
       x="tiempo", 
       y="toneladas") +   
  theme_ipsum() + 
  scale_y_comma()

##en los gráficos anteriores se ven que no hay una tendencia global. Hay estacionalidad.

#PROBAR qué efecto tiene quitar la tendencia:
#bolsa.sin.tend<-bolsa.ts-componentes$trend
#autoplot(bolsa.sin.tend)
#diff(bolsa.sin.tend,differences=1)


###PASO 2: análisis de autocorrelaciones
# Observamos los gráficos para estimar parámetros de un modelo de autocorrelación AR(p)
autoplot(acf(bolsa.ts, plot = F))#con False da la autocorrelación, con True la parcial
plot(acf(bolsa.ts, type="correlation", plot=T)) #igual que la anterior, da autocorrelación
plot(acf(bolsa.ts, type="partial", plot=T)) #da la correlación parcial. Se ve mejor si corresponde AR(p) 


###PASO 3: Prueba de raíz unitaria para ver si es necesario hacer diferencias
library(tseries)
kpss.test(bolsa.ts,null = c("Level"))  #H0: la serie es estacionaria en media
kpss.test(bolsa.ts,null = c("Trend")) #H0: la serie es estacionaria en variabilidad

d1.bolsa<-diff(bolsa.ts)
plot(decompose(bolsa.ts))
plot(decompose(d1.bolsa))
autoplot(acf(d1.bolsa, plot = F),main= "función de autocorrelación") 
autoplot(pacf(d1.bolsa, plot = T),main= "función de autocorrelación parcial") 

# forecast tiene forma de buscar la cantidad ótima de diferencias
#necesarias para estacionarizar una serie
ndiffs(bolsa.ts) #estima el n° de diferencias directas
nsdiffs(bolsa.ts)#estima el n° de diferencias en la estacionalidad


###PASO 4: Para aplicar métodos de predicción ARIMA necesitamos convertir la serie en estacionaria
# vamos a hacerlo de modo automático: 
#aplicamos sobre esta el proceso ARIMA fijando los parámetros
modelo1<-arima(bolsa.ts,order=c(1,1,1))
modelo1

#también podemos pedir que ajuste automáticamente los parámetros:
modelo2<-auto.arima(bolsa.ts)
modelo2

###PASO 5:comparamos modelos con AIC (menor es mejor)
#ver en la salida del modelo:
summary(modelo1)
summary(modelo2)

###PASO 6: diagnóstico 
acf(modelo2$residuals)
qqnorm(modelo2$residuals)
qqline(modelo2$residuals)
Box.test(modelo2$residuals, lag = 1, type = c("Ljung-Box"), fitdf = 0) #H0: independencia
#también podemos testear normalidad
jarque.bera.test(modelo2$residuals) # Test de Jarque-Bera H0: hay normalidad

#library(FitAR) ##no está disponible en mi versión!!!
#boxresult-LjungBoxTest (modelo2$residuals,k=2,StartLag=1)
#plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")

####PASO 7:PREDICCIONES
# prediccion de la serie de bolsa: 
predi<-predict(modelo2,n.ahead = 3)
predi1 <- forecast(modelo2, level = c(95), h = 3) #igual pero con biblio forecast
predi1
autoplot(predi1)


