## Ejecutar el código de las líneas 2 y 3 únicamente si no se han instalado antes las librerías.
install.packages("tidyverse")
install.packages("ggplot2")

## Cargamos las librerías y obtenemos los datos:
library(tidyverse)
library(ggplot2)
url <- "https://raw.githubusercontent.com/Libro-GEOFOREST/Capitulo1-Sensores/Exercise-1/calibracion_TDR.csv"
datos <- read.csv(url)
## Resumen de los datos
head(datos)
