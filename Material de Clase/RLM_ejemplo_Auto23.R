# clase de regresión lineal simple
# objetivos:  mostrar ejemplo de ajuste de modelo RLS
# autor: Silvia P?rez
# fechas: 2/9/2023
#para limpieza de memoria:
rm(list=ls())
gc()

######Cargamos paquetes a utilizar##########
library(dplyr) #para manejo de datos. Esta incluido en tidyverse, igual que readr y ggplot2
library(xtable)      # para reportar tablas en LaTex para informes
library(ggplot2)   #para gráficos lindos
library(corrplot)
library(readxl)
library(leaps) #  para subsets 

###leemos los datos
autos<- read_xlsx("Auto23.xlsx")
View(autos)
str(autos) #estructura. Muestra el tipo de variable
## vamos a sacar el ID para quedar con todas las variables a analizar
autos<-autos[,-1] #Se quita la primer columna, ya que no nos interesa el name
View(autos)
summary(autos)

## Veo si hay datos faltantes
any(is.na(autos$horsepower))
which(is.na(autos$horsepower))
autos<-na.omit(autos) #quito los datos con valores faltantes
## autos<-autos[!is.na(autos$horsepower),] #lo mismo de antes, para cuando ya identificamos en qué columna están los NA

## renombramos variables
nombres<-c("mpg","disp","potencia","peso","aceleracion")
names(autos)<-nombres

### analizamos un poco los datos: descriptivos
summary(autos)

## Vemos correlaciones y graficamos:
correlaciones<-cor(autos) #requiere corrplot. Las variables deben ser numéricas.
correlaciones
corrplot(correlaciones, method="number",tl.col="black",tl.cex=0.8)
corrplot(correlaciones, method="color",tl.col="black",tl.cex=0.8)
#la corr entre mpg y weight es -0.83

plot(autos)

## Si quiero ajustar modelo RLS, cuál parece más conveniente?
attach(autos)

modRLS <- lm(mpg~peso,data=autos)
summary(modRLS)

## ajustamos modelos de regresion multiple para predecir mpg
mod_full<-lm(mpg~.,data=autos)
summary(mod_full)

mod_potencia <- lm(mpg~potencia, data = autos)
summary(mod_potencia)

mod2 <- lm(mpg~peso+potencia, data = autos)
summary(mod2)
#xtable(summary(mod_full) ) #  para sacar en latex

##comparamos modelos
anova(modRLS,mod_full) #compara si las sumas de cuadrados son signif diferentes

## proponemos "a ojo" otros modelos
# mod2<-

### podemos definir nuevas variables para modelo cuadrático:
plot(mpg,peso)
peso2<-peso*peso
mod_C<-lm(mpg~peso+peso2,data=autos)
summary(mod_C)
print(mod_C)
anova(mod_C)
par(mfrow=c(2,2))
plot(mod_C)

print(xtable(mod_C),digits=4)

## Metodos automaticos para seleccion de variables:
##  "backward" / "forward" / "both"

modStep_for <- step(mod_full, direction = "forward",trace=T) 
modStep_back <- step(mod_full, direction = "backward",trace=T)
modStep_both <- step(mod_full, direction = "both",trace=F)
summary(modStep_for)
summary(modStep_back)
summary(modStep_both)


#Metodo de todas las regresiones

#FUNCIONAMIENTO: es otra forma de buscar modelos, le paso la formula con todas las variables
#  y el algoritmo muestra la cantidad de modelos con su propia cantidad de variables que
#  cumplen una bondad minima para ser considerados.
library(leaps) #  para lm y probar todas las combinaciones 
               #  buscar regsubsets para ver que otros parametros se pueden manejar
todos_RLM <- regsubsets(mpg~.
                             , data = autos
                             , nbest = 2    #  cuantos  mejores modelos quiero por cada cantidad de variables
                             , nvmax = 2  # max tamaño del modelo
                             , force.in = NULL, force.out = NULL
                             , method = "exhaustive")
summary(todos_RLM)
plot(todos_RLM, scale = "adjr2", main = "R^2 ajustado")

#### otra forma de usar regsubsets ####
colnames(autos)
variables <- c("disp","potencia","peso","aceleracion")

mpg <- "mpg"  #  variable a predecir

f <- as.formula(paste(mpg,paste(variables, collapse = " + "),sep = " ~ "))
print(f) #  aqui vemos la fórmula propuesta para todas las variables

regsubsets.out <- regsubsets(f
                             , data = autos
                             , nbest = 2    #  cuantos  mejores modelos quiero por cada cantidad de variables
                             , nvmax = 2  # max tama?o del modelo
                             , force.in = NULL, force.out = NULL
                             , method = "exhaustive")
summary(regsubsets.out)
plot(regsubsets.out, scale = "adjr2", main = "R^2 ajustado")

### Analizamos supuestos estudiando residuos###
residuos = residuals(mod_C)
summary(mod_C)
boxplot(residuos, col = "blue",horizontal=TRUE,ylim = c(-15,20),main="Box-plot de residuos")
#residuos alrededor de 0: ver en el boxplot
#homogeneidad de varianza: ver en residuos vs predichos
#normalidad de residuos: ver en QQplot
plot(mod_C)

# Test de normalidad de Shapiro-Wilk (muestras chicas)
x.test <- shapiro.test(residuos)
print(x.test)
# Test de normalidad de Kolmogorov-Smirnov (muestras grandes)
#library(nortest)
#lillie.test(residuos)

#errores independientes (no correlacionados, es equivalente si hay normalidad)
# Test Durbin Watson
# DW debe ser cercano a 2 y el p-value cercano a 1 para demostrarlo
# un pvalue muy pequenio indica que no hay independencia (están correlacionados)
# La hipotesis nula es que no hay autocorrelacion.
library(lmtest)
dwtest(mod_full)  # Si el pvalor es chico, entonces hay dependencias entre residuos

### Analizamos multicolinealidad e influyentes:
## VIF
library (car)
vif(mod_C)
vif(mod_full)
vif(modStep_back)

library(fmsb)
#INFLUYENTES:
cooks=cooks.distance(mod_C)
plot(cooks.distance(mod_C))

#otra forma:
library(broom)
model.diag.metrics <- augment(mod3)
names(model.diag.metrics)
which(model.diag.metrics$.cooksd>0.025) 

## PREDICCION de nuevos datos
nuevo<-data.frame(disp=c(130,152,305),
                  potencia=c(51,49,200),
                  peso=c(2444,3100,4800),
                  aceleracion=c(11,16,14))
#valores a predecir
predict(mod_full,nuevo)



## EXTRAS: viendo regresiones simples
library(GGally)
mod_full <- mod_full
# un resumen rapido:
graf.interactivo <- GGally::print_if_interactive
pm <- ggnostic(mod_full)
X11() #abre un gráfico interactivo en otra pantalla
graf.interactivo(pm)

