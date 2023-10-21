# experimento 2: uniendo horizontalmente los 6 archivos
library(randomForest) # para  el Modelo RF
library(caret)  
library(corrplot)  
library(C50)          # para  el Modelo C50
library(rpart)        # para el Modelo rpart
library(rpart.plot)
library(party)
library(RCurl)
library(doParallel)  # para la concurrencia

#######**********************************************************************
# inicio randomFores en paralelo
#Dos formas de ejecutar RF: secuencial y paralelo

#http://sehablanalytics.cl/2014/12/random-forests-consejos/

cores <- 4 #nro de nucleos a usar en modo paralelo
registerDoParallel(cores = cores)

#---------------------------------------------------
#funcion paralela usando n nucleos del procesador
par <- function(unModelo, cantArboles, cantVars){
  foreach(ntree.iter = rep(ceiling(cantArboles / cores), cores), # divido la cantidad total de arboles que tengo que generar por la cantidad de nucleos
          .combine = combine, .packages = "randomForest") %dopar% { #uno los bosques creados por cada nucleo
            randomForest( clase ~ .
                          , data= unModelo       # datos para entrenar 
                          , ntree= ntree.iter    # cantidad de arboles
                          , mtry=cantVars        # cantidad de variables
                          , replace = T            # muestras con reemplazo
                          , importance=T        # para poder mostrar la importancia de cada var
                          , class = NULL
            )
          }
}
setwd('C:/gus/CATEDRAS/2016_letras/ptoyecto_2016/2017_trazos/LBP_segmentos/')

########################################################## Lectura
# el input son 6 archivos .xlsx con los segmentos
library(xtable) # para latex
library(readxl)
S2 <- read_excel("S2.xlsx", col_names = FALSE)
S3 <- read_excel("S3.xlsx", col_names = FALSE)
S4 <- read_excel("S4.xlsx", col_names = FALSE)
S5 <- read_excel("S5.xlsx", col_names = FALSE)
S6 <- read_excel("S6.xlsx", col_names = FALSE)
S8 <- read_excel("S8.xlsx", col_names = FALSE)
# ------------------------------------
# renombro las columnas:
library(plyr) # para renombrar

for(i in 1:256){
  names(S2)[i]= paste0('S2_',i)
}
for(i in 1:256){
  names(S3)[i]= paste0('S3_',i)
}
for(i in 1:256){
  names(S4)[i]= paste0('S4_',i)
}
for(i in 1:256){
  names(S5)[i]= paste0('S5_',i)
}
for(i in 1:256){
  names(S6)[i]= paste0('S6_',i)
}
for(i in 1:256){
  names(S8)[i]= paste0('S8_',i)
}

# -----------------------------------------------
# uno los trazos en uno solo archivo 
S = cbind(S2 ,S3 ,S4,S5,S6,S8) 

# para agregar la col clase:
# 1ro creo un vector
clase = vector(mode='character', length=nrow(S))
# agrego la col clase en el primer lugar:
S = data.frame(clase, S)
S$clase <- c(rep(1,10), rep(2,10),rep(3,10),rep(4,10),rep(5,10),rep(6,10), rep(7,10),rep(8,10),rep(9,10),rep(10,10)
             ,rep(11,10), rep(12,10),rep(13,10),rep(14,10),rep(15,10),rep(16,10), rep(17,10),rep(18,10),rep(19,10),rep(20,10)
             ,rep(21,10), rep(22,10),rep(23,10),rep(24,10),rep(25,10),rep(26,10), rep(27,10),rep(28,10),rep(29,10),rep(30,10)
             ,rep(31,10), rep(32,10),rep(33,10),rep(34,10),rep(35,10),rep(36,10), rep(37,10),rep(38,10),rep(39,10),rep(40,10)
             ,rep(41,10), rep(42,10),rep(43,10),rep(44,10),rep(45,10),rep(46,10), rep(47,10),rep(48,10),rep(49,10),rep(50,10))

## convierto  campo a factor:
S$clase     = as.factor(S$clase)
write.csv2 (S, 'S.csv' ,fileEncoding= "UTF-8", row.names=FALSE)

# obtengo las muestras de entrenamiento y para el test:
##  xx% para el conjunto de entrenamiento:
smp_size <- floor(0.80 * nrow(S))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(S)), size = smp_size)
entrenamiento <- S[train_ind, ]
test          <- S[-train_ind, ]
write.csv2 (entrenamiento, 'entrenamiento_exp_2_union_trazos.csv',fileEncoding= "UTF-8", row.names=FALSE)
write.csv2 (test, 'test_exp_2_union_trazos.csv' ,fileEncoding= "UTF-8",row.names=FALSE)

#----------------------fin de preparacion de los datos ------------------

# Grafico del error OOB dependiendo de la cantidad de var
X11()
tuneRF(x = entrenamiento,   # data set de entrenamiento
       y = entrenamiento$clase,  # variable a predecir
       mtryStart  = 1,     # cantidad de variables inicial
       stepFactor = 2,     # incremento de variables
       ntreeTry   = 400,    # cantidad arboles a ejecutar en cada iteracion
       improve    = 0.01    # mejora minina del OOB para seguir iteraciones
) # esto dio como resultado que entre 4 y 8 var se decremente el OOB = 12 y 11%
# **********************

# el ciclo es para ajustar la cantidad de variables y arboles
vAciertos_RF=c(0)
for(j in 2: 4){
  gc()
  modelo.forest <- par(entrenamiento, 400,j)
  # Crea prediccion y Matriz de confusion:
  prediccion <- predict(modelo.forest, test, type='class'); # Matriz de Confusion
  mc <- with(test,table(prediccion, test$clase))
  
  #Calculo el % de  aciertos totales
  aciertos_RF <- sum(diag(mc)) / sum(mc) * 100
  cat("\nCantidad de variables:",j,"\n")
  cat("\nCorrectamente clasificados:",round(aciertos_RF,4),"%\n\n")
  vAciertos_RF[j] = aciertos_RF
}
vAciertos_RF

############################################################################
# el ciclo es para ver la robustez del resultado
vAciertos_RF=c(0)
corridas = 50
qvar = 3
for (qvar in 41:43) {
  cat( 'variables: ', qvar,' ; cant. de corridas: ', corridas,  "\n")
for(j in 1: 5){
  gc()
  train_ind <- sample(seq_len(nrow(S)), size = smp_size)
  # conjuntos de entrenamiento y prueba:
  entrenamiento <- S[train_ind, ]
  test          <- S[-train_ind, ]
  modelo.forest <- par(entrenamiento, 600,qvar)  
  prediccion <- predict(modelo.forest, test, type='class') 
  mc <- with(test,table(prediccion, test$clase))  # Matriz de Confusion
  
  #Calculo el % de  aciertos totales
  aciertos <- sum(diag(mc)) / sum(mc) * 100
  vAciertos_RF[j] = aciertos
}

cat('promedio: ',mean(vAciertos_RF),"% ")
cat( ' ; min: ', min(vAciertos_RF))
cat( ' ; max: ', max(vAciertos_RF), "\n")
}

# aciertos con 80% 20%: ( tomando 400 arboles)
# promedio 93.74%   ( promedios de 50 corridas;  min 85 ,  max: 99 )   con 9 variables
# promedio 94.36%   ( promedios de 50 corridas;  min 89 ,  max: 99 )   con 8 variables
# promedio 93.5%    ( promedios de 50 corridas;  min 88 ,  max: 99 )   con 7 variables
# promedio 94.43%   ( promedios de 100 corridas; min 87 ,  max: 100 )
# promedio 94.18%   ( promedios de 50 corridas;  min 85 ,  max: 100 )  con 16 variables
# promedio 93.3%    ( promedios de 50 corridas;  min 86 ,  max: 99 )   con 6 variables
# ver todos los resulyados en resultados_exp2.txt




# para ver la importancia de las variables: ( uso el resultado de la ultima corrida)
X11(10)
importancia = data.frame(importance(modelo.forest))
library(reshape)
importancia <- sort_df(importancia, vars = 'MeanDecreaseGini')
varImpPlot(modelo.forest, sort = T, n.var = 18 , main = 'Top 18 importancia de las variables')
# fin de ver la importancia de las var
round(importance(modelo.forest), 2 )

#                                            fin forestRandom
#######**********************************************************************
# C5.0
library(C50)
vAciertos_c50=c(0)
for(j in 1: 2){
  gc()
  train_ind <- sample(seq_len(nrow(S)), size = smp_size)
  # conjuntos de entrenamiento y prueba:
  entrenamiento <- S[train_ind, ]
  test          <- S[-train_ind, ]
modelo.c50 <- C5.0(clase ~ ., data=entrenamiento)
prediccion <- predict(modelo.c50, test, type="class")
mc <- with(test,table(prediccion, test$clase))  # Matriz de Confusion
cat("** Arbol de clasificacion C5.0\n")
# Aciertos en %
acierto_c50 <- sum(diag(mc)) / sum(mc) * 100
#cat("\n numero de corrida:",j,"\n",round(acierto_c50,2),"%\n\n\n")
vAciertos_c50[j] = acierto_c50
}
cat('promedio: ',mean(vAciertos_c50),"% ")
cat( ' ; min: ', min(vAciertos_c50))
cat( ' ; max: ', max(vAciertos_c50), "\n")

# aciertos con 80-20%:    67%  ( promedios de 20 corridas; min 57 ,  max: 76 )
#   -----------fin C5.0


# SVM   
library(e1071)
vAciertos_svm=c(0)
for(j in 1: 50){
  gc()
  train_ind <- sample(seq_len(nrow(S)), size = smp_size)
  # conjuntos de entrenamiento y prueba:
  entrenamiento <- S[train_ind, ]
  test          <- S[-train_ind, ]
modelo.svm <- svm(clase ~ ., data=entrenamiento)
pred <- predict(modelo.svm, test, type="class")
# Matriz de confusion
mc <- table(pred,test$clase, dnn = c("Asignado","Real"))
#cat("** SVM\n")
# Aciertos en %
acierto_svm <- sum(diag(mc)) / sum(mc) * 100
# cat("\n numero de corrida:",j,"\n",round(acierto_svm,2),"%\n\n\n")
vAciertos_svm[j] = acierto_svm
}
# vAciertos_svm # veo los resultados todos juntos
cat('promedio: ',mean(vAciertos_svm),"% ")
cat( ' ; min: ', min(vAciertos_svm))
cat( ' ; max: ', max(vAciertos_svm), "\n")
# aciertos con 80-20: 91.65% ( promedio de 20 corridas ; min es 83 y max es 97)

#--------------------------- Fin SVM
# AdaBoost - Adaptative Boosting
#  esto  tarde mucho ....ver  de correrlo en otro momento
library(adabag)
modelo.ad <- boosting(clase ~., data=entrenamiento)
pred <- predict(modelo.ad, test, type="class")
#Matriz de confusi?n
#mc <- mc[order(rownames(mc)), order(colnames(mc))]
mc <- table(pred,test$clase, dnn = c("Asignado","Real"))
#mc <- table(pred, test[, pred$clase], dnn = c("Asignado","Real"))

cat("** Adaptative Boosting: boosting\n")
print(mc)
# Aciertos en %
aciertos_ada <- sum(diag(mc)) / sum(mc) * 100
cat("\nCorrectamente clasificados:",round(aciertos_ada,2),"%\n\n\n")
# 71% de aciertos

##########################################################################
# Clasificador bayesiano ingenuo (Naive Bayes classifier): naiveBayes

modelo.nb  <- naiveBayes(  clase ~., data=entrenamiento)
pred <- predict(modelo.nb, test, type="class")
pred
# Matriz de confusión
mc <- table(pred, test$clase, dnn = c("Asignado","Real"))

# Ordenar tabla alfabéticamente
# Por algún motivo a veces sale desordenada
mc <- mc[order(rownames(mc)),order(colnames(mc))]
cat("** Clasificador bayesiano ingenuo: naiveBayes\n"); print(mc)
aciertos10 <- sum(diag(mc)) / sum(mc) * 100
cat("\nCorrectamente clasificados:",round(aciertos10,2),"%\n\n\n")

# 92% aciertos
#################################
# Modelo rp  - modelo de arbol: usando rpart
# PASO 1:   Crea Arbol de Decision
# ---------------------------------------------------------------------------
library(rpart)
library(rpart.plot)
ModeloArbol<-rpart(clase ~ .,data=entrenamiento, parms=list(split="information"))
print(ModeloArbol)
X11(14)
rpart.plot(ModeloArbol) ##  aqui tengo el resultado graficamente en entrenamiento
X11() # otra forma de graficarlo
rpart.plot(ModeloArbol, type=1, extra=100,cex = .7,
           box.col=c("gray99", "gray88")[ModeloArbol$frame$yval])
# ---------------------------------------------------------
library(nnet)
vAciertos_nn=c(0)
for(j in 1: 20){
  gc()
  train_ind <- sample(seq_len(nrow(S)), size = smp_size)
  entrenamiento <- S[train_ind, ]
  test          <- S[-train_ind, ]
  parametros <- train(clase ~ ., data=entrenamiento, method="nnet", trace=F)
  size <- parametros$bestTune$size
  decay <- parametros$bestTune$decay
  
  modelo.nnet <- nnet(clase ~ .,trace= F, data=entrenamiento, size=size, decay=decay)
  pred.nnet <- predict(modelo.nnet, test, type="class")
  
  mc <- table(pred.nnet,test$clase, dnn = c("Asignado","Real"))
  mc <- mc[order(rownames(mc)),order(colnames(mc))]
#  cat("** Red neuronal: nnet\n") ; print(mc)
  aciertos <- sum(diag(mc)) / sum(mc) * 100
#  cat("\nro de corrida:",j,"\n")
#  cat("\nCorrectamente clasificados:",round(aciertos,2),"%\n\n\n")
  vAciertos_nn[j] = aciertos
}
cat('promedio: ',mean(vAciertos_nn),"% ")
cat( ' ; min: ', min(vAciertos_nn))
cat( ' ; max: ', max(vAciertos_nn), "\n")
pred.nnet

