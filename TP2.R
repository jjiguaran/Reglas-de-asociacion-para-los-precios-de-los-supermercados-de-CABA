# TP 02
# se define el working environment
setwd('/home/juan/Documents/Academico/Maestria/Data mining/tp2')
# se inicia con las transformaciones de precios
library(jsonlite)
precios <- file("precios.json")
precios <- stream_in(precios)
precios <- precios[,c(-1,-5)]
reshaped <-reshape(precios, timevar = 'medicion', idvar = c('producto','sucursal'), direction = 'wide')
remove(precios)
reshaped <- reshaped[,c(1,2,7,12,8,11,9,3,4,6,5,10)]
write.csv(reshaped, 'reshaped.csv', row.names = F)
data <- read.csv('reshaped.csv')
nas <- which(is.na(data), arr.ind = T)
for (i in 1:nrow(nas)) {
  if ( is.numeric(data[nas[i,1],nas[i,2]-1]) &  is.numeric(data[nas[i,1],nas[i,2]+1])) {
    data[nas[i,1],nas[i,2]] <- (data[nas[i,1],nas[i,2]-1] + data[nas[i,1],nas[i,2]+1])/2
  }
}
remove(nas)
data$periodo1 <- rowMeans(data[c('precio.1', 'precio.2', 'precio.3')])
data$periodo2 <- rowMeans(data[c('precio.4', 'precio.5')])
data$periodo3 <- rowMeans(data[c('precio.6', 'precio.7')])
data$periodo4 <- rowMeans(data[c('precio.8', 'precio.9', 'precio.10')])
data$medio <- rowMeans(data[c('periodo1', 'periodo2', 'periodo3','periodo4')])
data <- data[complete.cases(data[ , 13:17]),]
data$variacion1  <- (data[,14] - data[,13])/data[,13]
data$variacion2  <- (data[,15] - data[,14])/data[,14]
data$variacion3  <- (data[,16] - data[,15])/data[,15]
data$variacion.total  <- (data[,16] - data[,13])/data[,13]
write.csv(data, 'precios_2.0.csv', row.names = F)
data <- read.csv('precios_2.0.csv')
discret <- function(valor){
  nombres <- c('Disminucion fuerte', 'Disminucion media', 'Disminucion leve', 'Mantiene', 'Aumento leve', 'Aumento medio', 'Aumento fuerte')
  limites <- c(-0.05,-0.02,-0.005,0.005,0.05,0.1)
  return(nombres[sum(valor > limites)+1])
}
data$variacion1.discret <-sapply(data[,18], discret)
data$variacion2.discret <-sapply(data[,19], discret)
data$variacion3.discret <-sapply(data[,20], discret)
data$variacion.total.discret <-sapply(data[,21], discret)
library(dplyr)
medias <- data %>%
  group_by(producto) %>%
  summarise_at(vars(periodo1,periodo2,periodo3,periodo4,medio), mean)
dataset<-merge(data, medias, by.x = "producto",  by.y = "producto")
dataset$relativo1 <- (dataset$periodo1.x-dataset$periodo1.y)/dataset$periodo1.y
dataset$relativo2 <- (dataset$periodo2.x-dataset$periodo2.y)/dataset$periodo2.y
dataset$relativo3 <- (dataset$periodo3.x-dataset$periodo3.y)/dataset$periodo3.y
dataset$relativo4 <- (dataset$periodo4.x-dataset$periodo4.y)/dataset$periodo4.y
dataset$relativo.medio <- (dataset$medio.x-dataset$medio.y)/dataset$medio.y
discret2 <- function(valor){
  nombres <- c('Muy barato', 'Medianamente barato', 'levemente barato', 'Medio', 'Levemente caro', 'Medio caro', 'Muy caro')
  limites <- c(-0.1,-0.05,-0.01,0.01,0.05,0.1)
  return(nombres[sum(valor > limites)+1])
}
dataset$relativo1.discret <-sapply(dataset[,31], discret2)
dataset$relativo2.discret <-sapply(dataset[,32], discret2)
dataset$relativo3.discret <-sapply(dataset[,33], discret2)
dataset$relativo4.discret <-sapply(dataset[,34], discret2)
dataset$relativo.medio.discret <-sapply(dataset[,35], discret2)
write.csv(dataset, 'precios_completos.csv', row.names = F)
datos <- dataset[,c(1,2,22,23,24,25,36,37,38,39,40)]
write.csv(datos, 'precios_transformados.csv', row.names = F)
# se prosige con las transformaciones de pproductos
productos <- file("productos.json")
productos <- stream_in(productos)
library(tm)
library(stringi)
library(stringr)
productos <- productos[,c(-1)]
productos2 <- productos[,c(-4)]
productos2<- data.frame(lapply(productos2,tolower))
productos2 <- data.frame(lapply(productos2, as.character), stringsAsFactors=FALSE)
productos2<- data.frame(lapply(productos2,removeNumbers))
productos2 <- data.frame(lapply(productos2, as.character), stringsAsFactors=FALSE)
productos2<- data.frame(lapply(productos2,removePunctuation))
productos2$nombre <- stri_trans_general(productos2$nombre, "Latin-ASCII")
productos2$marca <- stri_trans_general(productos2$marca, "Latin-ASCII")
productos2$marca <- stri_trans_general(productos2$marca, "Latin-ASCII")
productos2 <- data.frame(lapply(productos2, as.character), stringsAsFactors=FALSE)
productos2$marca <- stri_replace_all(productos2$marca, fixed=" ", "")
productos2$presentacion <- stri_replace_all(productos2$presentacion, fixed=" ", "")
write.csv(productos2, 'productos_2.0.csv', row.names = F)
productos2 <- read.csv('productos_2.0.csv')
productos2 <- data.frame(lapply(productos2, as.character), stringsAsFactors=FALSE)
present <- unique(productos2$presentacion)
productos2$nombre <- removeWords(productos2$nombre, present)
marc <- unique(productos2$marca)
productos2$nombre <- removeWords(productos2$nombre, marc)
productos2$nombre <- removeWords(productos2$nombre, stopwords(kind = 'es'))
library(arules)
corp <- Corpus(VectorSource(productos2$nombre))
matr <-TermDocumentMatrix(corp)
lista <- vector()
for (i in 1:70) {
  freq <- findFreqTerms(matr, i)
  lista <- c(lista,length(freq))
}
freq <- findFreqTerms(matr, 12)
bool <- productos2[,-3]
for (i in freq) {
  termino <-str_detect(productos2$nombre, i)
  termino[termino == T] = 'S'
  termino[termino != 'S'] = NA
  bool[paste0("termino_", i)] <- termino
}
bool$marca = NULL
write.csv(bool, 'productos_transformados.csv', row.names = F)
# se procede con el procesamiento de las sucursales
library(rgeos)
library(sp)
library('raster')
sucursales <- file("sucursales.json")
sucursales <- stream_in(sucursales)
cord_sucursales <- SpatialPoints(sucursales[,c("lng", "lat")])
barrios <- read.csv('barrios.csv')
cord_barrios <- sapply(barrios[,1], readWKT)
cord_barrios <- do.call(bind, cord_barrios)
ubicacion <-over(cord_sucursales,cord_barrios)
nombres <- ubicacion
for (i in 1:length(nombres)) {
  nombres[i] <- as.character(barrios$barrio[ubicacion[i]])
}
sucursales$barrio <- nombres
write.csv(sucursales, 'sucursales_transformados.csv', row.names = F)
