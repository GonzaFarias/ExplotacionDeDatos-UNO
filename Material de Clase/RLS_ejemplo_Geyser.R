# clase de regresión lineal simple
# objetivos:  mostrar ejemplo de ajuste de modelo RLS
# autor: Silvia Pérez
# fechas: 26/8/2023

#fijar directorio 
#setwd("C:/Users/...") #setea el entorno de trabajo. También se hace desde Session->Set working directory
getwd() #para ver en qué entorno estoy

#para limpieza de memoria:
rm(list=ls())
gc()

######Cargamos paquetes a utilizar##########
library(dplyr) #para manejo de datos. Está incluído en tidyverse, igual que readr y ggplot2
library(xtable)      # para reportar tablas en LaTex para informes
#library(fRegression) # para estadisticos especiales de regresion
library(ggplot2)   #para gráficos lindos
#library(data.table) #
library(corrplot)

###leemos los datos
library(readr)
datos <- read_delim("geyser.csv",delim=";",col_names=T,escape_double = FALSE, 
                    locale = locale(decimal_mark = ",", 
                    grouping_mark = ""),trim_ws = TRUE)

View(datos)
#summary(datos)

#### Podemos revisar los nombres de variables
#https://rdrr.io/github/HarryRosen/hrimodules/man/dbSafeNames.html
dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
}

colnames(datos) = dbSafeNames(colnames(datos))

#### cuándo tiene sentido pensar en un modelo RLS?
attach(datos) #para indicar que trabajamos con este dataset
cor(duracion,siguiente_g)
#otra opción:
#cor(datos$duracion,datos$siguiente_g)

#### graficamos los datos 
plot(duracion,siguiente_g,xlab='duracion',ylab='siguienteG',main="Gráfico de dispersión")
#más lindo!
ggplot(datos, aes(x=duracion, y=siguiente_g)) + labs(title="Gráfico de dispersión")+
  geom_point( color = "blue") + theme_light()

##### ajustamos el modelo RLS
colnames(datos) #si no recuerdo los nombres de variables..
modelo_rls<-lm(siguiente_g~duracion,data=datos) #no hace falta poner data si hicimos attach

### graficamos la recta ajustada sobre los datos
ggplot(datos, aes(x=duracion, y=siguiente_g))+ 
  geom_point() +
  geom_smooth(method='lm',se=FALSE, col='green') +
  theme_light()

#### cómo extraer información del modelo?
names(modelo_rls)
modelo_rls$coefficients
dim(datos)
#### Resumen del modelo
summary(modelo_rls)
xtable(summary(modelo_rls))  #  salida en latex (usar para informes!)

#### Significatividad del modelo
## analizar F-statistic o pendiente

#### Analisis de supuestos 
#veamos los residuales:
ei <- siguiente_g- fitted(modelo_rls)
ei2<-residuals(modelo_rls, type='working')
plot(ei,ei2)
# grafico de residuos
plot(ei)
#veo  si los residuos son Normales 
hist(ei) 
summary(ei)

# Test de normalidad de Shapiro-Wilk
ShW.test <- shapiro.test(ei)
print(ShW.test)  # pvalor chico indica NO normal

### Gráficos que vienen implementados para RLS
plot(modelo_rls) #permite obtener 4 gráficos para analizar
#para verlos juntos:
par(mfrow=c(2, 2))
plot(modelo_rls, las=1, col='deepskyblue4')

#### predicción de nuevo caso 
#¿tiene sentido la prediccion en los tres valores pedidos?
x_nuevo<-data.frame(duracion =(c(4.1,3.0,7))) #valores a predecir
predict(modelo_rls,x_nuevo)

predict(modelo_rls,x_nuevo, interval ="confidence")







