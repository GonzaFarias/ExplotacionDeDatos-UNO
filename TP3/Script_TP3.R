#########################################################################
# Tema: Analisis de Componentes Principales y Clustering                #   
# Materia: Explotación de Datos                                         #
# Fecha: 26/09/2023                                                     #
#                                                                       #
# Autor: Farias Gonzalo	                                                #
#                          												#
# Fuente: Kaggle - Argentina provincial data                            #
# https://www.kaggle.com/datasets/kingabzpro/argentina-provincial-data  #
#########################################################################

# CARGA DE BIBLIOTECAS
# Importamos las bibliotecas necesarias para el análisis.
library(readr)              # Para leer archivos de datos
library(dplyr)              # Para manipulación de datos
library(FactoMineR)         # Para el Análisis de Componentes Principales (ACP)
library(psych)              # Para estadísticas descriptivas
library(factoextra)         # Para visualización del ACP
library(corrplot)           # Para visualización de correlaciones
library(PerformanceAnalytics) # Para el índice de silueta
library(ggplot2)            # Para visualizaciones
library(plotly)             # Para crear gráficos interactivos
library(philentropy)        # Para algunas funciones adicionales

options(scipen = 6) # para evitar notacion cientifica.

# LIMPIEZA DEL ENTORNO
rm(list=ls())                # Limpiamos las variables en el entorno de trabajo
gc()                         # Recogemos la basura de la memoria

# Establecemos el entorno de trabajo
# setwd("C:/Users/...") 

# CARGA DE DATOS
# Cargamos los datos desde un archivo CSV
datos <- read_csv("D:/Universidad/Explotacion de Datos/argentina.csv")

# Eliminamos la columna "provincia" y renombramos las columnas para mayor claridad
datos <- subset(datos, select = -c(1))
colnames(datos) <- c("pbi", "analfabetismo", "pobreza", "infraestructura_deficiente", "abandono_escolar", "falta_atencion_medica", "mortalidad_infantil", "poblacion", "cines_por_cada_habitante", "medicos_por_cada_habitante")
View(datos)
# BÚSQUEDA DE DATOS NULOS
# Visualizamos un resumen de datos nulos en el conjunto de datos.
View(summarise_all(datos, funs(sum(is.na(.)))))
datos <- na.omit(datos) # Eliminamos las filas con valores nulos

# CASTEO DE DATOS
# Convertimos todas las columnas a tipo numérico para asegurarnos de que sean interpretables.
attach(datos)
datos <- datos %>% mutate_all(as.numeric)

# ANÁLISIS EXPLORATORIO DE DATOS
summary(datos)  # Estadísticas descriptivas
str(datos)      # Estructura del conjunto de datos

# BOXPLOT DE LOS DATOS CUANTITATIVOS
# Creamos un boxplot para visualizar la distribución de los datos cuantitativos.
pbi_boxplot <- plot_ly(y = ~datos$pbi , type = "box")
pbi_boxplot
rm(pbi_boxplot)  # Limpiamos la variable utilizada para el gráfico

############################### ANALISIS DE COMPONENTES PRINCIPALES ###################################


# CORRELACIONES PARA JUSTIFICAR EL ACP
cor(datos)               # Matriz de correlación
corrplot(cor(datos))     # Gráfico de dispersión de correlaciones

datos_cor <- cor(datos)  # Calculamos y almacenamos las correlaciones
datos_cor

# Visualización con índice de correlación para cada atributo
corrplot(datos_cor, method = "number", tl.col = "black", tl.cex = 0.8)

# ES RELEVANTE APLICAR ACP?
# Utilizamos el test de Bartlett y el índice KMO para evaluar si es adecuado el ACP.
cortest.bartlett(cor(datos), n = 22)  # Test de Bartlett
KMO(cor(datos))                       # Índice KMO

# ANÁLISIS DE COMPONENTES PRINCIPALES (ACP)
cp <- prcomp(datos, scale = TRUE)  # Realizamos el ACP
summary(cp)                        # Resumen de los resultados del ACP
names(cp)                          # Nombres de las componentes

# Los elementos center y scale almacenan la media y desviación típica 
cp$center  
cp$scale   
cp$sdev    

# rotation contiene el valor de los autovalores para cada componente (eigenvector)
cp$rotation  

# x almacena los autovectores
cp$x 

# GRÁFICO DE SEDIMENTACIÓN DE LAS COMPONENTES
# Visualizamos el gráfico de sedimentación de las componentes.
plot(cp, 
     type = "l", 
     main = "Gráfico de sedimentación",
     col = c("blue4"))
abline(0.7, 0, col = c("brown3"))

# SCREEPLOT PARA DECIDIR CUÁNTAS COMPONENTES UTILIZAR
fviz_screeplot(cp, addlabels = TRUE, ylim = c(0, 60),
               main = "CP más significativas con Screeplot")

# AJUSTE DE LA TÉCNICA
scree(cor(datos), pc = TRUE)

# BIPLÓT
# Realizamos un biplot para visualizar las variables y las observaciones en el espacio de componentes principales.
biplot(x = cp, scale = 0, cex = 0.6, col = c("blue4", "brown3"))


####################################### CLUSTER ##########################################

getDistMethods()  # Métodos de distancia disponibles

# ESCALADO DE DATOS
datos_esc <- scale(datos)
mat_dist <- dist(x = datos_esc, method = "euclidean")  # Cálculo de distancias

# CLUSTER JERÁRQUICO
hc_euclidea_average  <- hclust(d = mat_dist, method = "average")
cor(x = mat_dist, cophenetic(hc_euclidea_average))

# DENDROGRAMA
# Visualizamos dendrograma de clustering jerarquico.
fviz_dend(x = hc_euclidea_average, k = 3, cex = 0.6) + 
  geom_hline(yintercept = 5.5, linetype = "dashed") +
  labs(title = "Clustering jerárquico",
       subtitle = "Distancia euclidea, Linkaje average, k=3")

# ASIGNACIÓN DE GRUPOS
cutree(hc_euclidea_average, k = 3)  # Asignación de grupos

# Visualización de grupos en el plano de las 3 primeras componentes
fviz_cluster(object = list(data = datos, cluster = cutree(hc_euclidea_average, k = 3)),
             ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE,
             labelsize = 8)  +
  labs(title = "Clustering jerárquico + Proyección PCA",
       subtitle = "Distancia euclídea, Linkage avg, K=3") +
  theme_bw() +
  theme(legend.position = "bottom")

# CLUSTER JERÁRQUICO DIVISIVO
library(cluster)
hc_diana <- diana(x = mat_dist, diss = TRUE, stand = FALSE)

fviz_dend(x = hc_diana, cex = 0.5) +
  labs(title = "Clustering divisivo",
       subtitle = "Distancia euclídea")


# MAPA DE CALOR
library(viridis) # Paquete viridis para la paleta de color
colores <- viridis(254) # single
heatmap(x = datos_esc, scale = "none",col = hcl.colors(50), cexRow = 0.7) 

library(pheatmap)
kn <- 3  # Número de grupos (ajustar según sea necesario)
pheatmap(mat = datos_esc, scale = "none", clustering_distance_rows = "manhattan",
         clustering_distance_cols = "euclidean", clustering_method = "ward.D2",
         cutree_rows = kn, fontsize = 8)

# CLUSTER NO JERÁRQUICO - KMEANS
km_clusters_2 <- kmeans(x = mat_dist, centers = 3, nstart = 50)

# NÚMERO ÓPTIMO DE CLUSTERS (usando índice de silueta)
fviz_nbclust(x = datos_esc, FUNcluster = kmeans, method = "silhouette", k.max = 11) +
  labs(title = "Número óptimo de clusters", diss = mat_dist)

set.seed(101)  # Establecemos una semilla para reproducibilidad
fviz_cluster(object = km_clusters_2, data = datos, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means con k=3") +
  theme_bw() +
  theme(legend.position = "none")

# ANÁLISIS DE SILUETA
library(NbClust)
km_clusters <- eclust(x = datos_esc, FUNcluster = "kmeans", k = 3, seed = 123,
                      hc_metric = "manhattan", nstart = 50, graph = FALSE)

fviz_silhouette(sil.obj = km_clusters, print.summary = TRUE, palette = "jco",
                ggtheme = theme_classic()) 

# ADICIONAL
# Supongamos que km_clusters_2 contiene los resultados del clustering K-means
# y datos contiene los datos originales

# Crear un nuevo dataframe que incluya las dos primeras componentes principales y la asignación de clusters
df <- data.frame(PC1 = cp$x[, 1], PC2 = cp$x[, 2], Cluster = km_clusters_2$cluster)

# Crear el diagrama de dispersión
ggplot(df, aes(x = PC1, y = PC2, color = factor(Cluster))) +
  geom_point(size = 3) +
  labs(title = "Diagrama de Dispersión de Componentes Principales",
       x = "Componente Principal 1",
       y = "Componente Principal 2") +
  scale_color_discrete(name = "Cluster") +
  theme_minimal()

