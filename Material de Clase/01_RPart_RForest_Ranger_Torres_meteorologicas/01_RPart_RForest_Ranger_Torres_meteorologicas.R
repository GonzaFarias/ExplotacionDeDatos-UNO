
##############################################################
# Arboles de desicion.                                       #
#                                                            #
# Creado: 07/10/2023                                         #
# Version: 1.0                                               #
# Autor: Mendoza, Dante.                                     #
# Temas: RPart - RForest - Ranger - Visualizacion.           #
#                                                            #
##############################################################

# Se procede a utilizar para este ejemplo de arboles de desicion un set de datos provisto por
# el sitio web Buenos Aires Data y consta de mediciones de parametros meteorologicos, 
# tomadas por diversas torres de medicion inteligentes ubicadas en la Ciudad Autonoma de Buenos Aires.
# Se propone analizar y clasificar el indice de radiacion ultravioleta ("indice UV") 
# en base a datos meteorologicos tomados por 16 torres de medicion inteligentes ubicadas en
# distintos puntos de CABA. Como primera medida realizaremos un analisis exploratorio del conjunto de datos, 
# haciendo uso de diversas clases de graficos y funciones provistas por el lenguaje R para tales fines.
# Luego trabajaremos el conjunto de datos, eliminando variables sin importancia para el analisis,
# eliminando registros nulos, y generando una nueva variable que nos ayudara posteriormente a
# clasicar los registros. A continuacion construiremos varios tipos de modelos de arbol de decision, 
# comenzando por un modelo sencillo y adentrandonos posteriormente en los bosques aleatorios.

##### Importamos las biliotecas #####
library(readr)
library(plotly)
library(dplyr)
# Para separar dataset en entrenamiento y testing.
library(caret)
library(ggplot2)


##### Lectura y verificacion de la informacion leida #####
# Seteamos el entorno de desarrollo.
#setwd(paste0(getwd(),"/" ))


informacion_meteorologica_2011 <- read_csv("01_RPart_RForest_Ranger_Torres_meteorologicas/Datos/informacion-meteorologica-2011.csv", 
                                           col_types = cols(FECHA = col_skip(), 
                                                            HORA = col_skip(), ESTACION = col_number()))

##### Informacion de las variables #####
# FECHA         Fecha en que se tomo la medicion
# HORA          Hora en que se tomo la medicion
# ESTACION      Identificador de la torre de monitoreo que tomo la medicion
# VV (M/S)      Velocidad del viento, en ms2
# DV            Direccion del viento
# TEMP C        Temperatura ambiente en grados Celsius
# HR PORC       Porcentaje de humedad ambiente
# PRESS MBAR    Presion atmosferica en milibares
# PLUV MM       Precipitacion en milimetros
# RAD SOL W/M2  Radiacion solar en W/m2 (watt por metro cuadrado)
# UV UVINDEX    El indice de radiacion ultravioleta


informacion_torres <- read_csv("01_RPart_RForest_Ranger_Torres_meteorologicas/Datos/torres-de-monitoreo-inteligente-tmi.csv")

# En el resumen para los datos se observa que la columna HR_PORC tiene un valor null
summary(informacion_meteorologica_2011)

# Otra forma de buscar valores nulos
sapply(informacion_meteorologica_2011, function(x) sum(is.na(x)))

# Se elimina el valor nulo de la variable HR_PORC
informacion_meteorologica_2011 <- informacion_meteorologica_2011[!is.na(informacion_meteorologica_2011$HR_PORC), ]


##### Normalizacion y analisis #####
# Grafico dinamico de boxplot para identificacion de outliers
grafico.boxplot <- plot_ly(y=informacion_meteorologica_2011$`VV_(M/S)`, type="box", name="Velocidad del viento (m/s)") %>%
  add_trace(y=informacion_meteorologica_2011$DV, type="box", name="Direccion del viento") %>%
  add_trace(y=informacion_meteorologica_2011$TEMP_C, type="box", name="Temperatura en Celsius") %>%
  add_trace(y=informacion_meteorologica_2011$HR_PORC, type="box", name="Porcentaje de humedad") %>%
  add_trace(y=informacion_meteorologica_2011$PRESS_MBAR, type="box", name="Presion (mbar)") %>%
  add_trace(y=informacion_meteorologica_2011$PLUV_MM, type="box", name="Precipitacion (mm)") %>%
  add_trace(y=informacion_meteorologica_2011$`RAD_SOL_W/M2`, type="box", name="Radiacion solar (w/m2)")
grafico.boxplot


# Convierto la variable continua UV_UVINDEX (indice de radiacion UV) a categoria, para clasificar cada
# registro en un intervalo determinado, acorde a dicho valor. 
# Ref: https://www.tiempo.com/ram/270282/que-es-el-indice-de-radiacion-ultravioleta-uv/
#
# Los intervalos en cuestion son:
# menos de 3    <- bajo
# entre 3 y 5   <- moderado
# entre 6 y 7   <- alto
# entre 8 y 10  <- muy alto
# mas de 10     <- extremo
uv.intervalos <- cut(informacion_meteorologica_2011$UV_UVINDEX, c(-Inf, 2, 3, 5, 6, 7, 8, 10, Inf), left=F)

# Histograma de los intervalos
# Podemos apreciar una gran cantidad de casos con bajo indice de radicacion UV, lo cual seria logico,
# cuando menos para el area de estudip que es CABA.
qplot(uv.intervalos, ylab="Observaciones", xlab="Intervalos") #Se ve un gran desbalanceo de casos, hay diferentes tecnicas para hacer balanceo en arboles

# Creo una variable nueva que corresponde al intervalo de indice UV al que pertenece cada registro
informacion_meteorologica_2011_clasificada <- mutate(informacion_meteorologica_2011, IntervaloUV = uv.intervalos)

# A cada intervalo asigno un valor numerico discreto, Por lo tanto, se
# mejoran los intervalos con la creacion de una categoria asociada a cada uno de ellos;
# Intervalo     Valor numerico      Categoria
# (-Inf, 2]     0                   Indice UV bajo
# (2, 3]        0                   Indice UV bajo
# (3, 5]        1                   Indice UV medio
# (5, 6]        1                   Indice UV medio
# (6, 7]        2                   Indice UV alto
# (7, 8]        2                   Indice UV alto
# (8, 10]       3                   Indice UV muy alto
# (10, +Inf)    4                   Indice UV extremo

levels(informacion_meteorologica_2011_clasificada$IntervaloUV) <- c(0, 0, 1, 1, 2, 2, 3, 4)

# Cantidad de registros por nivel
summary(informacion_meteorologica_2011_clasificada$IntervaloUV)

# Remuevo variable continua UV_UVINDEX (Por que provocaria una clasificacion perfecta) del dataset 
# y la columna ESTACION ya que no aporta nada al modelo
informacion_meteorologica_2011_clasificada <- select(informacion_meteorologica_2011_clasificada, -UV_UVINDEX, -ESTACION)


# Normalizo el nombre de las variables, pues algunas contienen caracteres ilegales.
# el caracter / (barra) se encontraba presente en algunos casos.
names(informacion_meteorologica_2011_clasificada) <- make.names(names(informacion_meteorologica_2011_clasificada))

# Separo en 80/20 para entrenamiento y prueba (Train - Testing)
set.seed(876) # Semilla aleatoria que me permite recrear el experimento.
datos.entrenamiento1 <- sample_frac(informacion_meteorologica_2011_clasificada, .8) # se utiliza para seleccionar n filas aleatorias de un marco de datos, el numero indica el tamaño de la muestra.
datos.test1 <- setdiff(informacion_meteorologica_2011_clasificada, datos.entrenamiento1) # Utiliza el 20% restante.
summary(datos.entrenamiento1$IntervaloUV)
qplot(x=datos.entrenamiento1$IntervaloUV, xlab="Intervalo UV", ylab="Número de muestras")
summary(datos.test1$IntervaloUV)
qplot(x=datos.test1$IntervaloUV, xlab="Intervalo UV", ylab="Número de muestras")

# La desigual distribucion de las muestras podria provocar inconvenientes al momento de efectuar
# las predicciones: logicamente, para aquellas categorias con mayor cantidad de muestras, la prediccion
# seria bastante mas certera.

view(informacion_meteorologica_2011_clasificada)
##############################
# RPART   para ver como queda el arbol de manera grafica
##############################
library(rpart)
library(rpart.plot)

# Genero un modelo con rpart
modelo.rpart <- rpart(formula=IntervaloUV ~ ., data=datos.entrenamiento1)
rpart.plot(modelo.rpart, tweak = 1.3) # PROBAR LUEGO
# En cada nodo se observa la clase predicha, la probabilidad predicha para cada
# clase, y el porcentaje de muestras que corresponden a cada nodo.

#Importancia de las variables.
qplot(x = names(modelo.rpart$variable.importance), y=modelo.rpart$variable.importance,
      xlab="Variable", ylab="Importancia", main="rpart - Importancia de las variables") 

# Prediccion del primer modelo.
prediccion.rpart <- predict(modelo.rpart, newdata=datos.test1, type="class")
confusionMatrix(prediccion.rpart, datos.test1$IntervaloUV)

# La matriz de confusion proporciona informacion importante: principalmente la cantidad de predicciones
# correctas por clase, asi como tambien los falsos positivos y los falsos negativos. Por otra parte,
# y tal como era de esperarse, si bien la exactitud del modelo es alta (del 85,91 %), es deficiente para
# predecir, particularmente, las clases 3 y 4, debido a la baja cantidad de muestras para estas clases.

# Sensibilidad: VP / (FN + VP)
# especificidad: VN / (FP + VN)
# Porcentaje de acierto: VN + VP / (VN + VP + FN + FP)
# Expresado de otra forma: Sumatoria de la diagonal principal / Sumatoria total por cada fila
#                          3718 / 4328 = 0.8591

# La sensibilidad se explica como la capacidad del modelo de ser sensible a casos positivos: es
# la proporcion de casos positivos identificados de forma correcta. Por su parte, la especificidad del
# modelo se define como la proporcion de casos negativos identificados de forma correcta. 
# Ambas metricas dan una idea de la capacidad del modelo de distinguir entre casos positivos y negativos.


##############################
# RANDOM FOREST
##############################
# Implementacion de RandomForest estandar
library(randomForest)

# Aclaraciones sobre los parametros posibles;
# modelo.forest <- randomForest(tipo ~ .
#                             , data=entrenamiento[,-1] #a) Conjunto entrenamiento con el cual se formara el modelo para luego ponerse a prueba con el conjunto de test. Mayormente el train es 70% y el test un 30%.
#                             , ntree=700               #b) Cantidad de arboles.
#                             , mtry=2                  #c) Cantidad de variables por nodo.
#                             , replace= T              #d) Si tiene reemplazo o no. (TRUE - FALSE)
#                             , importance=T            #e) Si se toma en cuenta los residuos OOB
#                             , class = NULL            #f) Por defecto podemos usar "class"  de clasificacion o regresion, ver documentacion.
#                             )

##### MODELO 1 #####
# Genero el primer modelo con todas las variables y config por defecto
# Error OOB de aproximadamente 11.59%
modelo.rf1 <- randomForest(IntervaloUV ~ ., data=datos.entrenamiento1)
modelo.rf1
# Se puede observar que si bien el error del modelo es bajo (11,59 %),
# el error por clase, especificamente para las clases 2 y 3, es muy alto. Esto probablemente se deba a
# la baja cantidad de muestras para dichas clases.
# Se estudia entonces el error OOB (out-of-bag error): es el error promedio para cada observacion
# usando predicciones de los arboles que no contienen a dicha observacion.

prediccion.rf1 <- predict(modelo.rf1, newdata=datos.test1, type="class")
confusionMatrix(prediccion.rf1, datos.test1$IntervaloUV)

# Grafico del error OOB, donde se observa que a partir de 200 arboles el error se estabiliza
qplot(y=modelo.rf1$err.rate[,1], main="randomForest, Error out-of-bag, 500 arboles, 2 variables", 
      ylab="Error OOB", xlab="Cantidad de arboles")


##### MODELO 2 #####
# Genero el segundo modelo con todas las variables pero usando solo 200 arboles (la ejecucion es notablemente mas rapida)
# Sin embargo el error OOB aumenta poco (11.81%)
modelo.rf2 <- randomForest(IntervaloUV ~ ., data=datos.entrenamiento1, ntree=200)
modelo.rf2
prediccion.rf2 <- predict(modelo.rf2, newdata=datos.test1, type="class")
confusionMatrix(prediccion.rf2, datos.test1$IntervaloUV)
# Grafico del error OOB, donde se observa lo mismo que en el modelo.rf1
qplot(y=modelo.rf2$err.rate[,1], main="randomForest, Error out-of-bag, 200 arboles, 2 variables", 
      ylab="Error OOB", xlab="Cantidad de arboles")


##### MODELO 3 #####
# Genero tercer modelo con 800 arboles y 3 variables
# El error bajo muy ligeramente (11.58%) respecto del modelo.rf1
modelo.rf3 <- randomForest(IntervaloUV ~ ., data=datos.entrenamiento1, ntree=800, mtry=3)
modelo.rf3
prediccion.rf3 <- predict(modelo.rf3, newdata=datos.test1, type="class")
confusionMatrix(prediccion.rf3, datos.test1$IntervaloUV)
# Grafico del error OOB
qplot(y=modelo.rf3$err.rate[,1], main="randomForest, Error out-of-bag, 800 arboles, 3 variables", 
      ylab="Error OOB", xlab="Cantidad de arboles")

##### MODELO 4#####
# Genero cuarto modelo con 100 arboles y 2 variables
# El error bajo muy ligeramente (11.58%) respecto del modelo.rf1
modelo.rf4 <- randomForest(IntervaloUV ~ ., data=datos.entrenamiento1, ntree=100, mtry=2)
modelo.rf4
prediccion.rf4 <- predict(modelo.rf4, newdata=datos.test1, type="class")
confusionMatrix(prediccion.rf4, datos.test1$IntervaloUV)
# Grafico del error OOB
qplot(y=modelo.rf4$err.rate[,1], main="randomForest, Error out-of-bag, 100 arboles, 2 variables", 
      ylab="Error OOB", xlab="Cantidad de arboles")

##############################
# RANGER (contraccion de RANdom forest GENerator)
##############################
# Implementacion rapida de RandomForest
library(ranger)

# Ranger es una implementacion rapida de bosques aleatorios para R, 
# util cuando la cantidad de variables es grande.

# Pruebo con implementacion ranger, parametros por defecto (500 arboles, 2 variables)
# Error OOB de aproximadamente 11.49%, es decir, un poco menos que en la implementacion estandar, 
# con parametros por defecto, de randomForest
# Ojo porque el error varia entre ejecucion y ejecucion.
modelo.ranger <- ranger(formula=IntervaloUV ~ ., data=datos.entrenamiento1)
modelo.ranger
prediccion.ranger <- predict(modelo.ranger, data=datos.test1)
confusionMatrix(prediccion.ranger$predictions, datos.test1$IntervaloUV)

# Se observan unos resultados similares a los anteriores, la ganancia se ve reflejada en los tiempos
# de ejecucion que son menores.
