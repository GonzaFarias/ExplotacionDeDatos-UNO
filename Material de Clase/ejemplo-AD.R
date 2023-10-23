# clase de Análisis Discriminante Lineal
# objetivos:  mostrar ejemplo 
# autor: Silvia Pérez
# fechas: 21/10/2023

######Cargamos paquetes a utilizar##########
library(MASS)
library(dplyr) #para manejo de datos. Está incluído en tidyverse, igual que readr y ggplot2
library(ggplot2)   #para gráficos lindos
library(readxl)
library(caret) #para usar createDataPartition
library(reshape2)
library(knitr)

###leemos los datos
vinos<- read_xls("Vinos.xls")
vinos<-na.omit(vinos) #quito los datos con valores faltantes
View(vinos)
vinos$Clase<-as.factor(vinos$Clase)
str(vinos)
#attach(vinos)

#veamos que las variables cumplen las condiciones para ADL
##Ver que son numericas
pairs(x = vinos[, -1],
      col = c("firebrick", "green3")[vinos$Clase], pch = 19)

### Contraste de normalidad Shapiro-Wilk para cada variable en cada Clase
datos_tidy <- melt(vinos, value.name = "valor") 
kable(datos_tidy %>% group_by(Clase, variable) %>% summarise(p_value_Shapiro.test = shapiro.test(valor)$p.value))

#se ve que algunas variables no cumplen la condicion de normalidad. 
#nos quedamos con Alcohol,x2,x3,x6,x9:x12
vinos2<-subset(vinos,select=c(Clase,Alcohol,x2,x3,x6,x9:x12)) 
View(vinos2)
datos_tidy2 <- melt(vinos2, value.name = "valor")
kable(datos_tidy2 %>% group_by(Clase, variable) %>% summarise(p_value_Shapiro.test = shapiro.test(valor)$p.value))

#analicemos si hay outliers multivariados
library(MVN)
outliers <- mvn(data = vinos2[,-1], mvnTest = "hz", multivariateOutlierMethod = "quan")

##Separemos en conjuntos de train/test
set.seed(63) 
parte <- createDataPartition(vinos2$Clase, p=0.7, list=FALSE)  #requiere caret
vinosTrain<-vinos2[parte,]
vinosTest<-vinos2[-parte,]

#Ajustemos modelo discriminante
mod_lda <- lda(formula = Clase ~ .,
                  data = vinosTrain)
mod_lda
#se ve que se obtuvieron 2 funciones discriminantes LD1 y LD2
#los coeficientes muestran como construir cada una a partir de las variables orig
#LD1=-0.447*Alcohol-0.0607x2+0.13x3...
#Proporción de traza: Muestra el porcentaje de separación 
#logrado por cada función discriminante lineal.


#Obtenemos las predicciones para el conjunto de test
predi<-predict(object = mod_lda, newdata = vinosTest)
confusionMatrix(predi$class,vinosTest$Clase)

#visualicemos la particion usando
library(klaR)
partimat(Clase ~Alcohol+x6+x12,
         data = vinosTest, method = "lda", prec = 200,
         image.colors = c("darkgoldenrod1", "snow2", "skyblue2"),
         col.mean = "firebrick")

#otra visualización NECESITA R más actual!!
#library(ggord)
#ggord(mod_lda , data = vinosTest$Clase)


