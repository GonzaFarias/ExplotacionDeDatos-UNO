# Supervised Learning - Modelado de Arboles de Decisión

Este repositorio contiene el código y los datos utilizados para realizar un análisis de datos y modelado con arboles de decisión en dos contextos diferentes: detección de cáncer de mama (utilizando rpart) y evaluación de relaciones violentas en parejas (utilizando Random Forest).

## Contexto - Detección de Cáncer de Mama

### Fuente de Datos

Para el análisis de detección de cáncer de mama, se utilizó un conjunto de datos proveniente de Kaggle, específicamente el "Breast Cancer Dataset". Este conjunto de datos contiene información relevante para la clasificación de tumores mamarios en benignos y malignos.

**https://www.kaggle.com/datasets/yasserh/breast-cancer-dataset**
### Modelo rpart

Se aplicó el algoritmo de árboles de decisión utilizando la biblioteca `rpart` en R. El objetivo era construir un modelo capaz de clasificar los tumores en benignos o malignos basado en diversas características. El proceso involucró la preprocesamiento de los datos, la construcción del modelo y la evaluación de su rendimiento utilizando matrices de confusión.

## Contexto - Evaluación de Relaciones Violentas en Parejas

### Fuente de Datos

Para el análisis de evaluación de relaciones violentas en parejas, se utilizó un conjunto de datos de la Ciudad de Buenos Aires. Este conjunto de datos se centra en encuestas de relaciones de pareja y busca evaluar la presencia de violencia en estas relaciones.

**https://data.buenosaires.gob.ar/dataset/test-alerta-sobre-noviazgo-violento**
### Modelo Random Forest

Se aplicó el algoritmo Random Forest para clasificar la gravedad de las relaciones violentas en parejas. El proceso involucró el preprocesamiento de datos, la construcción del modelo de Random Forest y la evaluación de su rendimiento. También se analizó la importancia de las variables en el modelo.


Este repositorio proporciona información detallada sobre los pasos seguidos en cada uno de los análisis y permite a otros usuarios explorar y comprender cómo se aplicaron los árboles de decisión en diferentes contextos de análisis de datos.

