################################################################################################
# Tema: Arboles de decisión con clasificación (Random Forest y Rpart)
# Fecha: 21/10/2023
#
# Autores:                                                              
# Farias Gonzalo	- gonzafarias01@gmail.com                             
# Romano Diego	 -	romanodiegoe@gmail.com                                
# Robledo Alan	-	robledoezequiel609@gmail.com     
#
# Fuente para RPart: 
# Kaggle - Breast Cancer Dataset 
# https://www.kaggle.com/datasets/yasserh/breast-cancer-dataset
#
# Fuente para Random Forest:
# BA Data - Test de Alerta sobre un noviazgo violento
# https://data.buenosaires.gob.ar/dataset/test-alerta-sobre-noviazgo-violento
#
# Test - Señales de alerta en el noviazgo
# https://ash.buenosaires.gob.ar/desarrollohumanoyhabitat/mujer/senales-de-alerta-en-el-noviazgo
################################################################################################

##### Importamos las biliotecas #####
library(plyr)  
library(rpart.plot) 
library(caret)
library(tidyverse) 
library(rsample)
library(e1071) 
library(ggplot2)
library(dplyr)
library(corrplot)
library(MASS)
library(e1071)
library(ROCR)
library(gplots)
library(pROC)
library(rpart)
library(randomForest)
library(ggpubr)
library(readxl)

rm(list=ls())
gc()

options(scipen = 6) # para evitar notacion cientifica.

# setwd(paste0(getwd(),"/" )) # Setamos el entorno

#######################################################################################################  
################################################ RPART ################################################ 
####################################################################################################### 

##### Lectura y verificacion de la informacion leida #####
datosDiagnostico<- read_csv("breast-cancer.csv")
nrow(datosDiagnostico) # Cantidad de casos.

# ELIMINACIÓN DE DATOS NULOS
datosDiagnostico<-na.omit(datosDiagnostico) # quitamos los datos con valores faltantes en caso de haber
datosDiagnostico <- subset(datosDiagnostico, select = -c(1)) # eliminamos la columna ID
View(datosDiagnostico)

# Cambio de nombres para un mejor entendimiento.
colnames(datosDiagnostico)<- c(
  "diagnostico",
  "radio_promedio",
  "textura_promedio",
  "perimetro_promedio",
  "area_promedio",
  "suavidad_promedio",
  "compacidad_promedio",
  "concavidad_promedio",
  "puntos_concavos_promedio",
  "simetria_promedio",
  "dimension_fractal_promedio",
  "radio_error",
  "textura_error",
  "perimetro_error",
  "area_error",
  "suavidad_error",
  "compacidad_error",
  "concavidad_error",
  "puntos_concavos_error",
  "simetria_error",
  "dimension_fractal_error",
  "peor_radio",
  "peor_textura",
  "peor_perimetro",
  "peor_area",
  "peor_suavidad",
  "peor_compacidad",
  "peor_concavidad",
  "peor_puntos_concavos",
  "peor_simetria",
  "peor_dimension_fractal"
)

# Normalización de la varible categorica diagnostico
datosDiagnostico$diagnostico <- as.factor(mapvalues(datosDiagnostico$diagnostico,
                                                    from=c("M","B"),
                                                    to=c("Maligno", "Benigno")))
table(datosDiagnostico$diagnostico)

##### Información #####
# El cáncer de mama es el cáncer más común entre las mujeres en el mundo. 
# Representa el 25% de todos los casos de cáncer y afectó a más de 2,1 millones de personas solo en 2015. 
# Comienza cuando las células del seno comienzan a crecer sin control. 
# Estas células generalmente forman tumores que pueden verse mediante rayos X o palparse como bultos en el área del seno.
# Los principales desafíos para su detección es cómo clasificar los tumores en malignos (cancerosos) o benignos (no cancerosos).


##### Modelado - Arbol de decisión con RPart #####
set.seed(1) 
datosDiagnostico$diagnostico <- as.factor(datosDiagnostico$diagnostico)

parte <- createDataPartition(datosDiagnostico$diagnostico, p=0.75, list=FALSE)  
datosTrainDiagnostico<-datosDiagnostico[parte,]
datosTestDiagnostico<-datosDiagnostico[-parte,]

tr_fit <- rpart(diagnostico ~., data = datosTrainDiagnostico, method="class") # Indicamos que deseamos un arbol de clasificación.
tr_fit # Nuestro arbol obtenido.
rpart.plot(tr_fit) # Graficamos el arbol. Cabe aclarar que si el nodo dice "Benigno" representa "Maligno" y viceversa, esto se debe al orden alfabetico de niveles
# El nodo raiz indica que del 100% de la muestra el 37% es maligno.
# Si el peor perimetro es < 106, tenemos 61%, de este porcentaje el 5% es maligno
# Si es falso tenemos el 39%, de este porcentaje el 87% es benigno.
# y así continua el árbol...

# Calculo de predicciones del modelo en el conjunto de prueba
tr_pred <- predict(tr_fit, datosTestDiagnostico, type = "class")

# Matriz de confusión
confusionMatrix(tr_pred,datosTestDiagnostico$diagnostico)

##### Visualizacion #####
# Importancia de las variables.
qplot(x = names(tr_fit$variable.importance), y=tr_fit$variable.importance,
      xlab="Variable", ylab="Importancia", main="rpart - Importancia de las variables") 

## Distribución de diagnósticos 
ggplot(datosDiagnostico, aes(x = diagnostico, fill = diagnostico)) +
  geom_bar() +
  labs(title = "Distribución de diagnósticos",
       x = "Diagnóstico",
       y = "Cantidad") +  theme_gray() +   scale_fill_manual(values = c("Maligno" = "red", "Benigno" = "blue"))

## Porcentaje de Diagnósticos
resumen_diagnostico <- datosDiagnostico %>%
  group_by(diagnostico) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)
ggplot(resumen_diagnostico, aes(x = "", y = Percentage, fill = diagnostico)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Porcentaje de Diagnósticos",
       y = "Porcentaje",
       x = "") +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_fill_manual(values = c("Maligno" = "red", "Benigno" = "blue"))
# Vemos una gran cantidad de diagnosticos malignos, más de la mitad de los benignos

#######################################################################################################  
######################################### RANDOM FOREST ############################################### 
####################################################################################################### 

##### Lectura y verificacion de la informacion leida #####
datosViolencia<- read_xlsx("resultados-del-test.xlsx")
nrow(datosViolencia) # Cantidad de casos.

# BÚSQUEDA DE DATOS NULOS
# Visualizamos un resumen de datosViolencia nulos en el conjunto de datosViolencia.
# View(summarise_all(datosViolencia, funs(sum(is.na(.)))))
# datosViolencia<-na.omit(datosViolencia) #quito los datosViolencia con valores faltantes en caso de haber
datosViolencia <- subset(datosViolencia, select = -c(1, 2, 5)) # Eliminamos fecha, hora y barrio
View(datosViolencia)

# Normalización de los valores en las columnas pregunta_1 a pregunta_13
datosViolencia <- datosViolencia %>%
  mutate(across(starts_with("pregunta_"), ~case_when(
    . == "A" ~ "Siempre",
    . == "B" ~ "A_veces",
    . == "C" ~ "Nunca",
    TRUE ~ as.character(.)
  )))

# Cambio de nombres para un mejor entendimiento.
colnames(datosViolencia)[3:ncol(datosViolencia)] <- c(
  "controla",
  "revisa_celular",
  "acusa_infiel",
  "culpabiliza",
  "molesta_independencia",
  "sentir_perdidas",
  "normas_apariencia",
  "menosprecia_opinion",
  "critica",
  "amenaza_para_obedecer",
  "arrepentimiento_postviolencia",
  "cuestiona_exs",
  "sentir_presion_sexual"
)

##### Información del cuestionario #####
# controla                        Si la pareja controla a la victima en sus acciones
# revisa_celular                  Si la pareja revisa el celular a la victima
# acusa_infiel                    Si la pareja acusa de infiel a la victima
# culpabiliza                     Si la pareja culpa a la victima de la tensión en el ambiente
# molesta_independencia           Si a la pareja le molesta la independencia de la victima
# sentir_perdidas                 Si la victima siente perdidas ya sea de amigos o familia por culpa de su pareja
# normas_apariencia               Si la pareja aplica normas de apariencia como la vestimenta a la victima
# menosprecia_opinion             Si la pareja menosprecia la opinion de la victima
# critica                         Si la pareja critica lo que hace la victima
# amenaza_para_obedecer           Si la pareja amenaza a la victima para que esta la obedezca
# arrepentimiento_postviolencia   Si la pareja siente arrepentimiento luego de aplicar violencia
# cuestiona_exs                   Si la pareja cuestiona a los exparejas de la victima o indaga acerca de ellas
# sentir_presion_sexual           Si la victima siente presión sexual para continuar con la pareja

# Hacemos una variable categorica para tener un rango de edad y asi posteriormente visualizar.
datosViolencia <- subset(datosViolencia, !(edad >= 0 & edad < 5)) 
# Convierto la variable edad a categorica, para clasificar rangos de edades
# en base a los intervalos dados por la OMS 
# Ref: https://www.paho.org/es/noticias/9-5-2012-recomendaciones-mundiales-sobre-actividad-fisica-para-salud
#
# Los intervalos son:
# 5 a 17   <- Joven
# 18 a 64   <- Adulto
# 64 en adelante  <- Adulto mayor
datosViolencia$grupo_edad <- cut(datosViolencia$edad, c(5, 18, 65, Inf), labels = c("Joven", "Adulto", "Adulto Mayor"), left=F)

# Creamos variable categorica de nivel de violencia
datosViolencia <- datosViolencia %>% 
  mutate(siempres =( controla == "Siempre") +
           (revisa_celular == "Siempre") +
           (acusa_infiel == "Siempre") + 
           (culpabiliza == "Siempre") +
           (molesta_independencia == "Siempre")  +
           (sentir_perdidas == "Siempre")  +
           (normas_apariencia == "Siempre")  +
           (menosprecia_opinion == "Siempre")  +
           (critica == "Siempre")  +
           (amenaza_para_obedecer  == "Siempre") +
           (arrepentimiento_postviolencia == "Siempre")  +
           (cuestiona_exs == "Siempre")  +
           (sentir_presion_sexual  == "Siempre")) %>% 
  mutate(nivel_violencia = case_when(
    siempres > 9 ~ "Extremo",
    siempres > 5 & siempres <=9 ~ "Alto",
    siempres < 6 & siempres >2 ~ "Moderado",
    siempres < 3 ~ "Bajo"
  ))
datosViolencia$siempres <- NULL
datosViolencia$nivel_violencia <- as.factor(datosViolencia$nivel_violencia)

View(datosViolencia)

##### Modelado - Arbol de decisión con RandomForest #####
set.seed(16) 
datosViolencia$nivel_violencia <- as.factor(datosViolencia$nivel_violencia)

datosTrainViolencia <- sample_frac(datosViolencia, .8) # se utiliza para seleccionar n filas aleatorias de un marco de datos, el numero indica el tamaño de la muestra.
datosTestViolencia <- setdiff(datosViolencia, datosTrainViolencia) # Utiliza el 20% restante.

qplot(x=datosTestViolencia$nivel_violencia, xlab="Nivel de Violencia", ylab="Número de muestras")

##### MODELO #####
# Genero el modelo con todas las variables
modeloRforest <- randomForest(nivel_violencia ~ ., data=datosTrainViolencia, ntree=400, mtry=2)
modeloRforest
prediccionRforest <- predict(modeloRforest, newdata=datosTestViolencia, type="class")
# Error OOB de 5.31% 

# Grafico del error OOB, donde se observa que a partir de 200 arboles el error se estabiliza
qplot(y=modeloRforest$err.rate[,1], main="randomForest, Error out-of-bag, 400 arboles, 2 variables", 
      ylab="Error OOB", xlab="Cantidad de arboles")

# Crear la matriz de confusión
confusionMatrix(prediccionRforest, datosTestViolencia$nivel_violencia)
# Exactitud del 0.9392%

##### Visualizacion #####
# Grafico de la importancia de las variables
varImpPlot(modeloRforest, main = "Importancia de las variables en el modelo Random Forest")
# La que mayor importancia tiene es "acusa infiel"

porcentajes <- datosViolencia %>%
  group_by(grupo_edad, nivel_violencia) %>%
  summarise(count = n()) %>%
  group_by(grupo_edad) %>%
  mutate(percentage = (count / sum(count)) * 100)

# Porcentaje de parejas Violentas, por edad de víctimas
ggplot(porcentajes, aes(x = grupo_edad, y = percentage, fill = nivel_violencia)) +
  geom_col(position = "fill") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_fill(vjust = 0.5)) +
  labs(
    x = "Rango de Edad Victima",
    y = "Porcentaje",
    fill = "Nivel de Violencia"
  ) +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Parejas Violentas, por grupo de edad de la víctima")
# Vemos que quienes tienen mayor porcentaje de parejas violentas son los adultos mayores, sin embargo
sum(datosViolencia$grupo_edad == "Adulto Mayor") # son tan solo 6 muestras de este grupo de edad.
# En cambio tenemos una gran muestra de jovenes y adultos, donde hay un gran porcentaje 
# de parejas con alerta y violencia.


# Nivel de Violencia por Género de la Víctima
ggplot(datosViolencia, aes(x = genero, fill = nivel_violencia)) +
  geom_bar() + geom_text(stat = "count", aes(label = stat(count)), position = position_stack(vjust = 0.5)) +
  labs(
    x = "Género de la Víctima",
    y = "Cantidad por Nivel",
    fill = "Nivel de Violencia"
  ) +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Nivel de Violencia por Género de la Víctima") +
  theme_minimal()
sum(datosViolencia$genero == "Otro") # La muestra de personas con género otro es de tan solo 28
# Vemos que las mujeres son el género que mayor cantidad de reportes tienen de parejas
# violentas, asimismo las que más completan la encuesta.


#######################################################################################################  
############################################ RANGER ###################################################
####################################################################################################### 
library(ranger)
# Como extra probamos como quedan nuestras pruebas con Ranger (contraccion de Random Forest Generator)

# Ranger es una implementacion rapida de bosques aleatorios para R, 
# se destaca por su velocidad y su capacidad para manejar conjuntos de datos grandes. 
# Usamos los parametros por defecto.

## Modelo Ranger aplicado al diagnostico de Detección de Cáncer de Mama.
modeloRangerDiagnostico <- ranger(formula=diagnostico ~ ., data=datosTrainDiagnostico)
modeloRangerDiagnostico
prediccionRangerDiagnostico <- predict(modeloRangerDiagnostico, data=datosTestDiagnostico)
confusionMatrix(prediccionRangerDiagnostico$predictions, datosTestDiagnostico$diagnostico)
# Lo más notorio es el cambio del test Mcnemar's, el valor de Kappa y Accuracy son casi identicos. 

## Modelo Ranger aplicado al Nivel de Violencia en Parejas.
modeloRangerViolencia <- ranger(formula=nivel_violencia ~ ., data=datosTrainViolencia)
modeloRangerViolencia
prediccionRangerViolencia <- predict(modeloRangerViolencia, data=datosTestViolencia)
confusionMatrix(prediccionRangerViolencia$predictions, datosTestViolencia$nivel_violencia)
# El valor de Kappa y Accuracy son casi identicos. 
