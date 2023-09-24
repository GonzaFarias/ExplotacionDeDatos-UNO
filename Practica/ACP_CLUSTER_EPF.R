############################### CARGA DE BIBLIOTECAS ################################
library(readr)
library(readxl)
library(dplyr)
library(FactoMineR)
library(psych)
library(factoextra)
library(corrplot)
library(PerformanceAnalytics)
library(rela)
library(ggplot2)
library(plotly)
################################# CARGA DE DATOS ####################################

epf <- read_excel("D:/Universidad/Explotacion de Datos/Practica/epf.xls")

colnames(epf)<-c("Provincia","alimentacion","vestido_calzado","vivienda","mobiliario_domestico",
                   "gastos_sanitarios","transporte","ensenanza_cultura",
                   "turismo","otros_gastos")


#------------------------------------------------------------------------------------
#Analisis Exploratorio
summary(epf)
str(epf)
#------------------------------------------------------------------------------------
#Casteo de datos
attach(epf)
epf$alimentacion <- suppressWarnings(as.numeric(epf$alimentacion))
epf$vestido_calzado <- suppressWarnings(as.numeric(epf$vestido_calzado))
epf$vivienda <- suppressWarnings(as.numeric(epf$vivienda))
epf$mobiliario_domestico <- suppressWarnings(as.numeric(epf$mobiliario_domestico))
epf$gastos_sanitarios <- suppressWarnings(as.numeric(epf$gastos_sanitarios))
epf$transporte <- suppressWarnings(as.numeric(epf$transporte))
epf$ensenanza_cultura <- suppressWarnings(as.numeric(epf$ensenanza_cultura))
epf$turismo <- suppressWarnings(as.numeric(epf$turismo))
epf$otros_gastos <- suppressWarnings(as.numeric(epf$otros_gastos))
#------------------------------------------------------------------------------------
#Busqueda de datos nulos
View(summarise_all(epf, funs(sum(is.na(.)))))
#------------------------------------------------------------------------------------
#BOXPLOT DE LOS DATOS CUANTITATIVOS:
epf_gastos_sanitarios_boxplot <- plot_ly(y = ~epf$gastos_sanitarios, type = "box")
epf_gastos_sanitarios_boxplot
rm(epf_gastos_sanitarios_boxplot)

epf_gastos_sanitarios_boxplot <- boxplot(epf$gastos_sanitarios, col="skyblue", frame.plot=F)
epf_gastos_sanitarios_boxplot$out

epf_vivienda_boxplot <- boxplot(epf$vivienda, col="skyblue", frame.plot=F)
epf_vivienda_boxplot$out
#------------------------------------------------------------------------------------
#Correlaciones para justificar el ACP (argumentar)
cor(epf[, -1]) # matriz de correlacion
corrplot(cor(epf[, -1])) # scatter plot de correlaciones

epf_cor <- cor(epf[,-1]) #requiere corrplot. Las variables deben ser numéricas.
epf_cor

#Visualizacion con indice de correlacion para cada atributo
corrplot(epf_cor, method="number",tl.col="black",tl.cex=0.8)

#------------------------------------------------------------------------------------
#Es relevante aplicar ACP?: Se comprueba mediante un test de Barlett
#N = Cantidad registros
#
# La prueba de esfericidad de Bartlett prueba
#H0: no hay correlaciones (esfericidad) por lo que si pvalor chico entonces está habilitado ACP
cortest.bartlett(cor(epf[, -1]),n=51) #n=tamaño de muestra
#------------------------------------------------------------------------------------
#KMO  #Kaiser-Meyer-Olkin analiza los autovalores de la matriz de covarianzas
#sirve para comparar los valores de correlacion de las variables y sus correlaciones parciales
#si es cercano a 1, tiene sentido el analisis de componentes principales.
KMO(cor(epf[, -1]))

#------------------------------------------------------------------------------------
### ACP usando Rbase
# La función prcomp() calcula automáticamente el valor de las 
# componentes principales para cada observación 

cp <- prcomp(epf[,-1], scale = TRUE) # Analizo los componentes principales.
# Por defecto, prcomp() centra las variables para que tengan media cero
# si se quiere además que su desviación estándar sea de uno, hay que indicar scale = TRUE.
summary(cp) #Obtenemos el porcentaje de explicacion de los ACP *****
names(cp)
# Los elementos center y scale almacenados en el objeto pca contienen la media y desviación típica 
# de las variables previa estandarización (en la escala original).
cp$center  
cp$scale   
cp$sdev    
# rotation contiene el valor de los autovalores para cada componente (eigenvector). 
# El número máximo de componentes principales se corresponde con el mínimo(n-1,p),
# que en este caso es min(24,9)= 9.
cp$rotation  # **********
cp$x #autovectores ***************

#------------------------------------------------------------------------------------
# Grafico de Sedimentacion de las componentes
plot(cp, 
     type="l", 
     main="Gráfico de sedimentación",
     col=c("blue4"))
abline(0.7,0,col=c("brown3")) # linea horizontal en 1 del eje y.

# Usamos un grafico de barras.
library(factoextra)
fviz_screeplot(cp, 
               addlabels = TRUE, 
               ylim = c(0, 80),
               main="CP mas significativas con Screeplot")

#para graficar autovalores ordenados (gráfico de sedimentación)
#fviz_screeplot(cp, addlabels = TRUE, ylim = c(0, 60))

#Ajuste de la tecnica:
#  con cuantas CP nos quedamos???
scree(cor(epf[, -1]),pc=TRUE)
#------------------------------------------------------------------------------------
# para graficar el biplot:
biplot(x = cp, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

# Biplot con puntos. Se ven las variables y los casos.
# El grafico entre la Componente Principal 1 y 2, se puede apreciar
# dos grandes agrupamientos de variables, indicando correlacion positiva en
# cada grupo, y que estos grupos estan de forma perpendicular, indicando correlacion nula
biplot(x = cp, scale = 0, cex = 0.6, xlabs=rep(".", nrow(data)),col = c("grey", "brown3"))
#------------------------------------------------------------------------------------

# para ver el biplot
fviz_pca_biplot(cp,
                repel = TRUE, #para que no superponga texto 
                title = "ACP - Biplot")

#------------------------------------------------------------------------------------
###OTRA FORMA DE CORRER ACP: 
library(FactoMineR)
acp <- PCA(epf[, -1],graph= F)
summary(acp)


######extras########
#varias formas de ver correlaciones de a pares:
pairs(epf[, -1])

library(psych)
chart.Correlation(datos[, -1]) # para graficar correlaciones e histogramas

library(PerformanceAnalytics)
chart.Correlation(datos[, -1], histogram=T, pch=20)


################### CLUSTER ########################

epf <- epf[,-1] #quito provincia
View(epf)

library(philentropy)
getDistMethods()
#Escalado de datos ## Probar otras distancias y linkajes 
datos_esc  <- scale(epf)
mat_dist <- dist(x = datos_esc, method = "euclidean")

####################  CLUSTER JERARQUICO ###################  
hc_euclidea_completo <- hclust(d = mat_dist, method = "complete")
hc_euclidea_average  <- hclust(d = mat_dist, method = "average")


cor(x = mat_dist, cophenetic(hc_euclidea_completo)) #se quiere que sea cercana a 1
cor(x = mat_dist, cophenetic(hc_euclidea_average))

# Dendograma con distancia euclidea y linkaje average

fviz_dend(x = hc_euclidea_average, k = 2, cex = 0.6) + 
  geom_hline(yintercept = 5.5, linetype = "dashed") +
  labs(title = "Clustering jer?rquico",
       subtitle = "Distancia eucl?dea, Linkaje average, k=2")

# Dendrograma con distancia euclidea y linkaje completo
fviz_dend(x = hc_euclidea_completo, k = 2, cex = 0.6) + 
  geom_hline(yintercept = 5.5, linetype = "dashed") +
  labs(title = "Clustering jer?rquico",
       subtitle = "Distancia eucl?dea, Linkaje completo, k=2")


# Grupo asignado por cada caso
cutree(hc_euclidea_average, k = 2)
# Grafico de los grupos anteriores sobre el plano de las 2 primeras componentes
fviz_cluster(object = list(data=epf, cluster=cutree(hc_euclidea_average, k=2)),
             ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE,
             labelsize = 8)  +
  labs(title = "Clustering jer?rquico + Proyecci?n PCA",
       subtitle = "Distancia eucl?dea, Lincage avg, K=2") +
  theme_bw() +
  theme(legend.position = "bottom")

#para hacer cluster jerarquico divisivo:
library(cluster)
hc_diana <- diana(x = mat_dist, diss = TRUE, stand = FALSE)

fviz_dend(x = hc_diana, cex = 0.5) +
  labs(title = "Clustering divisivo",
       subtitle = "Distancia eucl?dea")

library(viridis) # Paquete viridis para la paleta de color
colores <- viridis(254)
# single
heatmap(x = datos_esc, scale = "none",col = hcl.colors(50),
        cexRow = 0.7)

# Mapa de calor
library(pheatmap)
kn <- 2  # modificar 
pheatmap(mat = datos_esc, scale = "none", clustering_distance_rows = "manhattan",
         clustering_distance_cols = "euclidean", clustering_method = "ward.D2", # <-- 
         cutree_rows = kn, fontsize = 8)

####################  CLUSTER no jerarquico - KMEANS ################### 
km_clusters_2 <- kmeans(x = mat_dist, centers = 2, nstart = 50)
# Verificamos numero optimo de clusters, serian 2
fviz_nbclust(x = datos_esc , FUNcluster = kmeans, method = "silhouette", k.max = 11) +
  labs(title = "Numero optimo de clusters", diss = mat_dist)


set.seed(101) # 

fviz_cluster(object = km_clusters_2, data = epf, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means con k=2") +
  theme_bw() +
  theme(legend.position = "none")


library(NbClust)
km_clusters <- eclust(x = datos_esc, FUNcluster = "kmeans", k = 2, seed = 123,
                      hc_metric = "manhattan", nstart = 50, graph = FALSE)

fviz_silhouette(sil.obj = km_clusters, print.summary = TRUE, palette = "jco",
                ggtheme = theme_classic()) 
