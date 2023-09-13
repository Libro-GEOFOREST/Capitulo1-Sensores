## Cargamos las librerías y obtenemos los datos:
library(tidyverse)
library(ggplot2)
url <- "https://raw.githubusercontent.com/Libro-GEOFOREST/Capitulo1-Sensores/Ejemplo-2/fitdata-TDR.csv"
datos <- read.csv(url)
fit <- as.data.frame(datos$fit)
## Creamos una tabla de datos con las diferencias entre los pasos de voltaje
dr_df <- as.data.frame(cbind(datos[-1,2],apply(fit,2,function(x) diff(x)/head(x,-1))))
## Cambiar nombres para reconocer las variables
colnames(dr_df) <- c("volt", "deltatita") 
## Representación gráfica
plot(dr_df$volt, dr_df$deltatita, xlab="mV",ylab=expression(delta))
## Resolucion
min(abs(dr_df$deltatita))
