# clase de regresión lineal simple
# objetivos:  mostrar ejemplo de Anscombe sobre la importancia de gráficos
# autor: Silvia Pérez
# fechas: 26/8/2023

#fijar directorio 
#setwd("C:/Users/...") #setea el entorno de trabajo. También se hace desde Session->Set working directory
getwd() #para ver en qué entorno estoy

#para limpieza de memoria:
rm(list=ls())
gc()

### paquetes a usar
library(gridExtra)
library(ggplot2)

#### leemos los datos
data("anscombe") #está en el paquete "datasets" base
attach(anscombe)
head(anscombe) #son 4 conjuntos de puntos (x_i,y_i), i=1...4

#ajustamos una regresion
mod1<-lm(y1 ~ x1, data = anscombe) # modelo 1
mod1

####graficamos todos los conjuntos con la recta

g1<- ggplot(anscombe, aes(x = x1, y = y1))+ 
  geom_point() +
  geom_smooth(method='lm',se=FALSE, col='red')

g2<- ggplot(anscombe, aes(x = x2, y = y2))+ 
  geom_point() +
  geom_smooth(method='lm',se=FALSE, col='red')

g3<- ggplot(anscombe, aes(x = x3, y = y3))+ 
  geom_point() +
  geom_smooth(method='lm',se=FALSE, col='red')

g4<- ggplot(anscombe, aes(x = x4, y = y4))+ 
  geom_point() +
  geom_smooth(method='lm',se=FALSE, col='red')

grid.arrange(g1, g2, g3, g4, top='Cuarteto de Anscombe')
