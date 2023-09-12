## Cargamos las librerías y obtenemos los datos:
library(tidyverse)
library(ggplot2)
url <- "https://raw.githubusercontent.com/Libro-GEOFOREST/Capitulo1-Sensores/Ejemplo-2/fitdata_TDR.csv"
datos <- read.csv(url)
