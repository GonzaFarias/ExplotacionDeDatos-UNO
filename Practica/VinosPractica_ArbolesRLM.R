###############################################
# Tema: Arboles de decisión con clasificación y predicción
# Fecha: 14/10/2023
# Autor: Farias Gonzalo.
###############################################

##### Importamos las biliotecas #####
library(dplyr) #para manejo de datos. Está incluído en tidyverse, igual que readr y ggplot2
library(ggplot2)   #para gráficos lindos
library(readxl)
library(leaps) #  para subsets 
library(rpart)
library(caret)
library(rpart.plot)
library(corrplot)
library (car)

rm(list=ls())
gc()

##### Lectura y verificacion de la informacion leida #####
vinos<- read_xls("Vinos.xls")
vinos<-na.omit(vinos) #quito los datos con valores faltantes
View(vinos)

set.seed(63) 
parte <- createDataPartition(vinos$Clase, p=0.7, list=FALSE)  #requiere caret
vinosTrain<-vinos[parte,]
vinosTest<-vinos[-parte,]
################################################################################################
#1) HACER REGRESION PARA PREDECIR ALCOHOL
correlaciones<-cor(vinos) 
corrplot(correlaciones, method="color",tl.col="black",tl.cex=0.8)

##  RLM para predecir "Alcohol"
mod_full<-lm(Alcohol~.,data=vinosTrain)
summary(mod_full)  
## VIF
vif(mod_full)

#aplicamos método de selección automática
modStep_back <- step(mod_full, direction = "backward",trace=T)
summary(modStep_back)

y_pred2 <- predict(object=modStep_back, newdata=vinosTest)
plot(vinosTest$Alcohol,y_pred2,pch=20,main="predichos backward",  xlab='Alcohol', ylab=expression(hat(Alcohol)))
abline(a=0, b=1, col="green")

################################################################################################
#2) HACER ARBOLES Y COMPARAR
# Ajusto un árbol para predecir la variable "Alcohol"
mod1 <- rpart(Alcohol ~ ., data = vinosTrain)
mod1
summary(mod1)
rpart.plot(mod1)

# Construyo un árbol solo con las variables que aparecieron
mod2 <- rpart(Alcohol ~ Clase + x9 + x12 + x3, data = vinosTrain)
rpart.plot(mod2)

# Podemos ver los valores predichos de todo el dataset de prueba
y_pred <- predict(object = mod1, newdata = vinosTest)
plot(vinosTest$Alcohol, y_pred, pch = 20, xlab = 'Alcohol', ylab = expression(hat(Alcohol)))
abline(a = 0, b = 1, col = "green")

cor(y_pred, vinosTest$Alcohol)
mean((vinosTest$Alcohol - y_pred)^2)  # Error Cuadrático Medio (ECM) para el árbol = 0.3273394 
mean((vinosTest$Alcohol - y_pred2)^2)  # Error Cuadrático Medio (ECM) para la regresión backward = 0.2861433

################################################################################################
#3) HACER ARBOLES PARA PREDECIR Clase
tr_fit <- rpart(Clase ~., data = vinosTrain, method="class") # Indicamos que deseamos un arbol de clasificacion.
tr_fit # Nuestro arbol obtenido.
rpart.plot(tr_fit) # Graficamos el arbol.

#Importancia de las variables.
qplot(x = names(tr_fit$variable.importance), y=tr_fit$variable.importance,
      xlab="Variable", ylab="Importancia", main="rpart - Importancia de las variables") 

# Calcular predicciones del modelo en el conjunto de prueba
tr_pred1 <- predict(tr_fit, vinosTest, type = "class")
table(Predicha = tr_pred1, Real = vinosTest$Clase)

# Crear la matriz de confusión
confusionMatrix(
  as.factor(tr_pred1),
  as.factor(vinosTest$Clase),
  positive = "Yes"
)

# Sensibilidad: VP / (FN + VP)
# especificidad: VN / (FP + VN)
# Porcentaje de acierto: VN + VP / (VN + VP + FN + FP)
# Expresado de otra forma: Sumatoria de la diagonal principal / Sumatoria total por cada fila

