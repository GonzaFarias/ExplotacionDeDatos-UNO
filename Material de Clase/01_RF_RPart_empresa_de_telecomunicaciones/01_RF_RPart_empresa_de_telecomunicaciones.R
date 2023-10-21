
##############################################################
# Arboles de desicion del dataset Churn.                     #
#                                                            #
# Creado: 06/10/2023                                         #
# Version: 1.0                                               #
# Autor: Mendoza, Dante.                                     #
#                                                            #
##############################################################

# Se procede a utilizar para este ejemplo de arboles de desicion el set de datos "Churn".
# Este archivo contiene informacion de clientes que dejaron de utilizar el servicio de una empresa de telecomunicaciones.
# La finalidad es ver que clientes son potenciales a dejar de utilizar el servicio.
# La fuente de este archivo se encuentra en el siguiente enlace: 
# https://www.kaggle.com/blastchar/telco-customer-churn

##### Importamos las biliotecas #####
library(plyr)  
library(rpart.plot) 
library(caret)
library(gridExtra) 
library(tidyverse) 
library(rsample)
library(e1071) 
library(GGally)
library(data.table)
library(DT)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
#library(rms)
library(MASS)
library(e1071)
library(ROCR)
library(gplots)
library(pROC)
library(rpart)
library(randomForest)
library(ggpubr)


##### Lectura y verificacion de la informacion leida #####
# Seteamos el entorno de desarrollo.
# setwd("D:/Universidad/Explotacion de Datos/6 - Arboles")

churn <- read_csv("01_RF_RPart_empresa_de_telecomunicaciones/Datos/WA_Fn-UseC_-Telco-Customer-Churn.csv")
View(churn)

nrow(churn) # Cantidad de casos.

# Numero de casos con null mediante iteracion con Sapply.
sapply(churn, function(x) sum(is.na(x)))

# Vemos que porcentaje representa esos valores nulos del total.
sum(is.na(churn$TotalCharges))/nrow(churn)

churn_clean <- churn[complete.cases(churn), ] # complete.cases() devuelve un vector con los valores completos y que sean no nulos.
sapply(churn_clean, function(x) sum(is.na(x))) # Verificamos si hay valores nulos.

# Normalizamos algunos campos.
churn_clean$SeniorCitizen <- as.factor(mapvalues(churn_clean$SeniorCitizen,
                                                 from=c("0","1"),
                                                 to=c("No", "Si")))

churn_clean$MultipleLines <- as.factor(mapvalues(churn_clean$MultipleLines, 
                                                 from=c("No phone service"),
                                                 to=c("No")))

churn_clean$OnlineSecurity <- as.factor(mapvalues(churn_clean$OnlineSecurity, 
                                                 from=c("No internet service"),
                                                 to=c("No")))

churn_clean$OnlineBackup <- as.factor(mapvalues(churn_clean$OnlineBackup, 
                                                 from=c("No internet service"),
                                                 to=c("No")))

churn_clean$DeviceProtection <- as.factor(mapvalues(churn_clean$DeviceProtection, 
                                                 from=c("No internet service"),
                                                 to=c("No")))

churn_clean$TechSupport <- as.factor(mapvalues(churn_clean$TechSupport, 
                                                 from=c("No internet service"),
                                                 to=c("No")))

churn_clean$StreamingTV <- as.factor(mapvalues(churn_clean$StreamingTV, 
                                               from=c("No internet service"),
                                               to=c("No")))

churn_clean$StreamingMovies <- as.factor(mapvalues(churn_clean$StreamingMovies, 
                                               from=c("No internet service"),
                                               to=c("No")))

# Se cambia los nombres de las columnas para un mejor entendimiento.
names(churn_clean) <- c("ID_Cliente",
                        "Genero", 
                        "Jubilado/a", 
                        "Casado/a", 
                        "Hijos", 
                        "Meses_en_la_compania", 
                        "Servicio_telefonico", 
                        "Multiples_lineas", 
                        "Internet",
                        "Seguridad_online",
                        "Backup_online",
                        "Proteccion_de_dispositivo",
                        "Soporte_tecnico",
                        "TV_Streaming",
                        "Peliculas_Streaming",
                        "Tipo_contrato",
                        "Facturacion_electronica",
                        "Metodo_de_pago",
                        "Cargo_mensual",
                        "Cargo_total",
                        "Cancelacion_del_servicio"
                        )

churn_clean$ID_Cliente <- NULL # Ponemos en null ya que no sera de utilidad, una forma alternativa de omitir esa columna.

# Vemos como quedo la informacion normalizada.
View(churn_clean)


##### Modelado - Arbol de desicion #####
set.seed(56) # Numero inicial del cual comenzara a generar una secuencia aleatoria.
split_train_test <- createDataPartition(churn_clean$Cancelacion_del_servicio, p=0.7, list=FALSE) #p = porcentaje de los datos a usar, list = Si el resultado devolvera una lista o una matriz. 
dtrain<- churn_clean[split_train_test,] #Se suele usar 70% en entrenamiento
dtest<-  churn_clean[-split_train_test,] # y 30% en test, dependiendo del caso.

# Removemos Cargo_total.
dtrain <- dtrain[,-19] 
dtest <- dtest[,-19]

tr_fit <- rpart(Cancelacion_del_servicio ~., data = dtrain, method="class") # Indicamos que deseamos un arbol de clasificacion, tambien podemos armar un arbol de regresion.
tr_fit # Nuestro arbol obtenido.
rpart.plot(tr_fit) # Graficamos el arbol.

# La variable "Tipo_contrato" es la mas importante, aquellos que esten por uno o dos años son menos propensos
# a abandonar el servicio, los que pagan mes a mes tienen mas chances de darse de baja.
# Los clientes con internet DSL tambien tienen menos chances que pidan la baja del servicio.
# Aquellos clientes con mas de quince meses tienen mas tendencia a permanecer en el servicio.
# El nodo raiz en la parte superior, nos muestra que los clientes que no renovaron el servicio del 100%
# de la muestra, representan un 27%. Bajando vemos que se pregunta si el tipo de contrato es de uno o dos años.
# En caso afirmativo vamos al nodo terminal izquierdo que nos dice que del 45% de la muestra tenemos,
# solo un 7% no renovara el servicio. Por el lado derecho tenemos aquellos clientes que tienen un contrato mensual.
# Entonces, tenemos que un 42% de estos clientes no renovara el servicio, siendo estos un 55% de la muestra restante. 
# Cada nodo está coloreado de acuerdo a la categoría mayoritaria entre los datos que agrupa. 
# Esta es la categoría que ha predicho el modelo para ese grupo.

# Matriz de confusion
tr_prob1 <- predict(tr_fit, dtest) # Compara el resultado del arbol con el conjunto de prueba.
tr_pred1 <- ifelse(tr_prob1[,2] > 0.5,"Yes","No") # Separa los que fueron clasificados por el si o no si supera el 50%
table(Prediccion = tr_pred1, Cancelacion_del_servicio = dtest$Cancelacion_del_servicio)

# Presicion de la matriz de confusion
#tr_prob2 <- predict(tr_fit, dtrain) # Compara el resultado del arbol con el conjunto de entrenamiento.
#tr_pred2 <- ifelse(tr_prob2[,2] > 0.5,"Yes","No") # Separa los que fueron clasificados por el si o no si supera el 50%
#tr_tab1 <- table(Prediccion = tr_pred2, Actual = dtrain$Cancelacion_del_servicio) # Lo usaremos luego.
#tr_tab2 <- table(Prediccion = tr_pred1, Actual = dtest$Cancelacion_del_servicio) # Lo usaremos luego.

# Train
#confusionMatrix(
#  as.factor(tr_pred2),
#  as.factor(dtrain$Cancelacion_del_servicio),
#  positive = "Yes" 
#)

# Test
confusionMatrix(
  as.factor(tr_pred1),
  as.factor(dtest$Cancelacion_del_servicio),
  positive = "Yes" 
)
#kappa, mcnermars, intervalo de confianza como tarea

#tr_acc <- sum(diag(tr_tab2))/sum(tr_tab2)
#tr_acc # 79% de presicion.

# Sensibilidad: VP / (FN + VP)
# especificidad: VN / (FP + VN)
# Porcentaje de acierto: VN + VP / (VN + VP + FN + FP)
# Expresado de otra forma: Sumatoria de la diagonal principal / Sumatoria total por cada fila

# La sensibilidad se explica como la capacidad del modelo de ser sensible a casos positivos: es
# la proporcion de casos positivos identificados de forma correcta. Por su parte, la especificidad del
# modelo se define como la proporcion de casos negativos identificados de forma correcta. 
# Ambas metricas dan una idea de la capacidad del modelo de distinguir entre casos positivos y negativos.

##### Visualizacion #####
# "Cancelacion del servicio segun tipo de contrato"
figura_1 <- ggplot(churn_clean, aes(x = Tipo_contrato, fill = Cancelacion_del_servicio)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) +
  labs(title="Cancelacion del servicio segun tipo de contrato")

figura_1
# Los clientes que pagan mes a mes, tienen tendencia a cancelar el servicio.
# Los clientes con contratos por año, son menos propensos a cancelar el servicio.

# "Cancelacion del servicio segun tipo de internet contratado"
figura_2 <- ggplot(churn_clean, aes(x = Internet, fill = Cancelacion_del_servicio)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) +
  labs(title="Cancelacion del servicio segun tipo de internet contratado")

figura_2
# Podemos notar que los clientes que contratan internet son mas propensos a darse de baja, 
# en especial los de fibra optica.

