# clase de arboles de regresión
# objetivos:  mostrar ejemplo 
# autor: Silvia Pérez
# fechas: 14/10/2023

#para limpieza de memoria:
rm(list=ls())
gc()

######Cargamos paquetes a utilizar##########
library(dplyr) #para manejo de datos. Está incluído en tidyverse, igual que readr y ggplot2
library(ggplot2)   #para gráficos lindos
library(readxl)
library(leaps) #  para subsets 
library(rpart)
library(rpart.plot)
library(corrplot)
###leemos los datos
autos<- read_xls("ejemploMontgomery.xls") # Datos de la tabla B3 del libro Montgomery
autos<-na.omit(autos) #quito los datos con valores faltantes
View(autos)

#nombramos mejor las variables involucradas
names(autos) <- c("rend","desplazamiento","HP",
                  "torque","compresion","eje","carburador","vel.transmision",
                  "longitud","ancho","peso","tipo.transmision")
summary(autos)
#Ajusto un arbol
mod1 <- rpart(rend ~ ., data=autos) #si "y" es continua se asume method="anova"
mod1 
summary(mod1)
prp(mod1) #es una forma más básica de graficar el árbol
rpart.plot(mod1)

#interpretar la salida anterior: 
#Si una nueva observación tiene ancho= 70 y  HP= 100 entonces predice rend=20
#Si una nueva observación tiene ancho= 60 y  HP= 150 entonces predice rend=28

#construyo un árbol sólo con las variables que aparecieron
mod2 <- rpart(rend ~ ancho + HP, data=autos)
rpart.plot(mod2)
#de esta manera puedo tener la siguiente representación
with(autos, plot(x=HP, y=ancho))
abline(h=66, lty='dashed', col='blue')
segments(x0=144, y0=66, x1=144, y1=82, lty='dashed', col='blue')
text(x=120, y=63, labels='y=28', col=4)
text(x=90, y=73, labels='y=20', col=4)
text(x=190, y=73, labels='y=16', col=4)

#para ver predicciones sobre un nuevo conjunto de datos
nuevos_datos <- data.frame(HP=c(100, 150), ancho=c(70, 60))
predict(object=mod2, newdata=nuevos_datos)

#podemos ver los valores predichos de todo el dataset
y_pred <- predict(object=mod1, newdata=autos)
plot(autos$rend,y_pred,pch=20,  xlab='rend', ylab=expression(hat(rend)))
abline(a=0, b=1, col="green")

cor(y_pred, autos$rend)
mean((autos$rend-y_pred)^2)  #ECM para el árbol queda = 11.8106


### REGRESION LINEAL MULTIPLE #####

correlaciones<-cor(autos) #requiere corrplot. Las variables deben ser numéricas.
corrplot(correlaciones, method="color",tl.col="black",tl.cex=0.8)

## ajustamos modelos de regresión múltiple para predecir "rendimiento"
mod_full<-lm(rend~.,data=autos)
summary(mod_full)  #no hay vamriables significativas... MULTICOLINEALIDAD!
## VIF
library (car)
vif(mod_full)

#aplicamos método de selección automática
modStep_back <- step(mod_full, direction = "backward",trace=T)
summary(modStep_back)

y_pred2 <- predict(object=modStep_back, newdata=autos)
plot(autos$rend,y_pred2,pch=20,main="predichos con step",  xlab='rend', ylab=expression(hat(rend)))
abline(a=0, b=1, col="green")

#hay que ver los supuestos pero...
#la regresión da mejor predicción y muestra las variables más significativas.
#una forma de compararlos: error cuadrático medio:
mean((autos$rend-y_pred)^2)  #ECM para el árbol queda = 11.8106

mean((autos$rend-y_pred2)^2)  #ECM para la regresión Step queda = 7.460802

#  









