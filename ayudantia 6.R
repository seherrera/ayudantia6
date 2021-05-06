# Ayudantia 6

## Importar Librerias

library(tidyverse)
library(cluster)
library(factoextra)
library(janitor)


## Cargar Datos:

setwd("C:/Users/sebah/Desktop/U/mineria de datos/proyecto N1/Ayudantia_DataMining01_2021/Ayudantia 6")

data <- read.csv("Spotify_Songs.csv")

summary(data)
head(data)


# Pre Procesamiento de los Datos

## Limpieza Datos:

Para este dataset el proceso de limpieza de datos sera un poco mas extensa por lo que debemos ir por partes

- Primero verificar la existencia de valores NA o faltantes
```{r limpieza na}
# Para las observaciones que tengan datos faltantes, le asignamos el valor NA para eliminarlos en el siguiente paso
data[data == ""] <- NA

# Verificamos donde hay valores NAs
data %>% 
  summarise_all(funs(sum(is.na(.))))

# De existir eliminamos todas las observaciones que presenten estos datos
data_pre <- data %>% 
  filter(!(is.na(track_name)|is.na(track_artist)|is.na(track_album_name)|is.na(duration_ms)))

# Corroboramos que no queden datos NA
data_pre %>% 
  summarise_all(funs(sum(is.na(.))))


 Segundo filtrar y remover datos duplicados

data_pre <- data_pre[!duplicated(data_pre$track_id),]



- Tercero verificar la existencia de errores en los datos de las observaciones

# Al explorar la base de datos podemos darnos cuenta de que hay varias observaciones que tiene mal ingresado los datos
# Por lo que tomaremos la columna track_popularity (como sabemos que es un valor numerico entre 0-100) y transformares esa columna
# de factor a numerico, por lo que todas las observaciones que no sean numeros se ingresara NA por defecto

data_pre$track_popularity <- as.numeric(as.character(data_pre$track_popularity))

# Como generamos nuevos valores NA dentro de nuestra BBDD, debemos volver a ejecutar el paso uno de la limpieza de datos

data_pre <- data_pre %>% 
  filter(!(is.na(track_popularity)))

# Eliminamos el patron <U que aparece en algunas observaciones en track_name y track_artist

data_pre <- data_pre[!grepl("<U",data_pre$track_name),]
data_pre <- data_pre[!grepl("<U",data_pre$track_artist),]

# Ahora corroboraremos si existen canciones que esten duplicadas
data_pre %>% count(duplicated(data_pre$track_name))

# Como existen canciones repetidas realizamos la consulta para obtener los valores distintos, pero este hecho obvia que hayan canciones con el mismo nombre pero de distinto autos  
data_pre %>% distinct(track_name, .keep_all = TRUE, )

# Por lo que creamos una variables que almacene si existe duplicidad en la cacion y/o en el artista
data_pre$duplicate <- duplicated(data_pre[,c("track_name", "track_artist")])

# Generamos un sub data frame que almacenara solo los valores que haya obtenido el valor TRUE a la consulta anterior y los ordenamos por track popularity
data_dupli <- data_pre %>% 
  filter(data_pre$duplicate == TRUE) %>% 
  arrange("track_name", "track_popularity", desc(track_popularity))

# Seleciono las filas que sean distintas, borro todas las canciones que se repiten y me quedo con la mayor track popularity
data_dupli <- data_dupli %>% 
  distinct(track_name, track_artist, .keep_all = TRUE)

# Elimino de mi data pre procesada los datos que dieron positivo a la duplicidad, para que al momento de re insertar los datos sobrevivieron a la limpieza de duplicidad no se genere la duplicidad que se estaba evitando
data_pre <- data_pre[!(data_pre$duplicate == TRUE),]

# Junto la data pre procesada con los datos que sobrevivieron a la limpieza de duplicidad
data_pre <- rbind(data_pre, data_dupli)

# Elimino la columna que me indicaba duplicidad ya que no sera util mas adelante
data_pre$duplicate <- NULL



Una vez limpiados los datos, el siguiente paso en el pre procesamiento será escalar los datos pero antes debemos revisar los datos por si hay que transformar alguna variable

## Revisar Estructura Datos

# Transformamos cada variables al tipo de variable que sale en el archivo .txt con la descripcion de cada una

data_pre$track_id <- as.character(data_pre$track_id)
data_pre$track_name <- as.character(data_pre$track_name)
data_pre$track_artist <- as.character(data_pre$track_artist)
data_pre$track_album_id <- as.character(data_pre$track_album_id)
data_pre$track_album_name <-  as.character(data_pre$track_album_name)
data_pre$playlist_name <- as.character(data_pre$playlist_name)
data_pre$playlist_id <- as.character(data_pre$playlist_id)
data_pre$playlist_genre <- as.character(data_pre$playlist_genre)
data_pre$playlist_subgenre <- as.character(data_pre$playlist_subgenre)

data_pre$danceability <- as.double(as.character(data_pre$danceability))
data_pre$energy <- as.double(as.character(data_pre$energy))
data_pre$key <- as.double(as.character(data_pre$key))
data_pre$loudness <- as.double(as.character(data_pre$loudness))
data_pre$mode <- as.double(as.character(data_pre$mode))
data_pre$speechiness <- as.double(as.character(data_pre$speechiness)) 
data_pre$acousticness <- as.double(as.character(data_pre$acousticness))
data_pre$instrumentalness <- as.double(as.character(data_pre$instrumentalness))
data_pre$liveness <- as.double(as.character(data_pre$liveness))
data_pre$valence <- as.double(as.character(data_pre$valence))
data_pre$tempo <- as.double(as.character(data_pre$tempo))
data_pre$duration_ms <- as.double(as.character(data_pre$duration_ms))

# transformacion de milisegundos a minutos
#data_pre <- data_pre %>% mutate(duration_min = data_pre$duration_ms/60000)

# Character
data_char <- c("track_id", "track_name", "track_artist", "track_album_id", "track_album_name", "playlist_name", "playlist_id", "playlist_genre", "playlist_subgenre")

# Double
data_dou <- c("track_popularity","danceability", "energy", "key", "loudness", "mode", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo", "duration_ms")

# Volvemos a borrar los datos que puedan haber quedado como NA con el cambio de tipo de variable
data_pre <- data_pre %>% 
  filter(!(is.na(key)|is.na(danceability)))

summary(data_pre)

str(data_pre)



## Separo Datos

datanum <- data_pre %>% 
  select(data_dou)

datachar <- data_pre %>% 
  select(data_char)



## Escalar Datos

data_sca <- sapply(datanum, scale)

#min_max_norm <- function(x) {
#    return((x - mean(x))/(max(x) - min(x)))    
#  }

#div_norm <- function(y) {
#    y/100
#  }

#des_norm <- function(z) {
#    return((z+min(z))*(max(z) - min(z)))
#  }

#data_scalmin <- min_max_norm(datanum)

```

# Procesamiento de los Datos

## Clustering Jerarquico

# Matriz de Distancias

#Distancia Euclideana
d = dist(data_sca, method = "euclidean")

#Distancia Manhattan
d1 = dist(data_sca, method = "manhattan")

#Distancia Minkowski
d2 = dist(data_sca, method = "minkowski")

hist(d, main = "Histograma Distancia Euclideana")
hist(d1, main = "Histograma Distancia Manhattan")
hist(d2, main = "Histograma Distancia Minkowski")

## Clustering Aglomerativo

Utilizando la funcion de R base hclust, aplicamos hierarchical clustering, a partir de la matriz de distancias d, y utilizamos el criterio complete linkage

- Complete Model

# Fijamos una seed para que cada vez que ejecutemos el codigo obtengamos los mismos valores y no vayan cambiado cada vez que ejecutamos el script
set.seed(369)

model_complete <- hclust(d, method = "complete")

summary(model_complete)


- Ward Model

set.seed(369)

model_ward <- hclust(d, method = "ward.D")

summary(model_ward)


- Otra forma de hacer clustering jerarquico con la funcion agnes

#model_comag <- agnes(d, method = "complete")

#model_comag$ac



#model_wardag <- agnes(d, method = "ward.D")

#model_wardag$ac


- Comparacion de los coeficientes de aglomeracion para cada metodo

#models <- c("single", "complete", "average", "ward")
#names(models) <- c("single", "complete", "average", "ward")

models <- c("complete", "ward")
names(models) <- c("complete", "ward")

agcoef <- function(x) {
  agnes(data_sca, method = x)$ac
}

#sapply(models, agcoef)



Generamos un dendrograma para visualizar la jerarquia. La libreria 'ggdendro' permite hacer estos diagramas en una sintaxis equivalente a ggplot. 



library("ggdendro")

ggdendrogram(model_complete, rotate = TRUE, theme_dendro = TRUE) 

## Corte

# Determinamos un valor para h lo que nos entregara un valor distinto de k para cada h que escogamos, tambien podemos definir el k desde un inicio
groups <- cutree(model_complete, h = 9)

# Se imprimen los tamaños de cada cluster
table(groups)

# Generamos una nueva columna para almacenar a que cluster pertenece cada observacion (tanto en data_pre y datanum)
data_pre$clust <- as.factor(groups)
datanum$clust <- as.factor(groups)

# Graficamos las observaciones agrupadas por su cluster
fviz_cluster(list(data = data_sca, cluster = groups))

## Caracteristicas de los clusters encontrados

datanum$clust <- as.numeric(as.character(datanum$clust))

# Generamos una tabla que almacenara los valores promedios para cada uno de los clusters encontrados lo que nos permitira caracterizar a cada uno de ellos
infoclusters <- aggregate(datanum, by=list(cluster=datanum$clust), mean)

# Borramos la columna clust ya que se repite esa informacion en la tabla generada
infoclusters$clust <- NULL

# Transformamos el tiempo de la cancion a minutos
infoclusters <- infoclusters %>% mutate(duration_min = infoclusters$duration_ms/60000)

# Borramos la columna de la duracion en milisegundoss
infoclusters$duration_ms <- NULL

infoclusters


## Filtremos por clusters con mas datos

# 1er Cluster con mas datos
data_c1 <- data_pre %>% 
  filter(data_pre$clust == 1)

# 2do Cluster con mas datos
data_c2 <- data_pre %>% 
  filter(data_pre$clust == 4)

# 3er Cluster con mas datos
data_c3 <- data_pre %>% 
  filter(data_pre$clust == 2)


## Tomemos a c2

# Borramos la columna clust para escalar la datanum de c2
data_c2$clust <- NULL

# Selecciono las variables numericas, se escalan las variables y se almacenan los datos en una tabla
datanumc2 <- data_c2 %>% 
  select(data_dou) %>% 
  scale() %>% 
  as_tibble()



#Ahora a C2 le aplicaremos un clustering divisivo

## Clustering Divisivo

# Generamos un modelo divisvo mediante la funcion diana de clusters
modelo_div <- diana(datanumc2)

# Le pedimos el coeficiente de divisivilidad al modelo
modelo_div$dc

# Graficamos nuestro dendrograma divisivo
pltree(modelo_div, cex = 0.8, hang = -1.5, main = "Dendrogram of diana")


## Cantidad Clusters

# Para el caso divisivo le entregaremos el numero de clusters con los que queremos agrupar nuestros datos
groupsc2 <- cutree(modelo_div, k = 10)

# Se imprimen los tamaños de cada cluster
table(groupsc2)

# Generamos una nueva columna para almacenar a que cluster pertenece cada observacion de data_c2
data_c2$clust <- as.factor(groupsc2)

# Graficamos las observaciones agrupadas por su cluster
fviz_cluster(list(data = datanumc2, cluster = groupsc2))

# Generamos una nueva columna para almacenar a que cluster pertenece cada observacion de datanumc2
datanumc2$clust <- as.factor(groupsc2)


## Caracteristicas Clusters encontrados

datanumc2$clust <- as.numeric(as.character(datanumc2$clust))

# Generamos una tabla que almacenara los valores promedios para cada uno de los clusters encontrados lo que nos permitira caracterizar a cada uno de ellos
infoclustersc2 <- aggregate(datanumc2, by=list(cluster=datanumc2$clust), mean)

# Borramos la columna clust ya que se repite esa informacion en la tabla generada
infoclustersc2$clust <- NULL

# Transformamos el tiempo de la cancion a minutos
infoclustersc2 <- infoclustersc2 %>% mutate(duration_min = infoclustersc2$duration_ms/60000)

# Borramos la columna de la duracion en milisegundoss
infoclustersc2$duration_ms <- NULL

infoclustersc2

