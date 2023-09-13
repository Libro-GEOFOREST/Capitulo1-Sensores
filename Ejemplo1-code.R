########## GEOFOREST - Cap. 1 SENSORES / Ejemplo 1 ############################
############ Francisco Jose Ruiz Gomez ########################################

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
## Representacion grafica
plot(datos$V, datos$tita, xlab="mV",ylab=expression(theta))
## Rango:
max(datos$V)
min(datos$V)
## Margen
titamax <- max(datos$tita)
titamin <- min(datos$tita)
titamax-titamin
## Ajuste de la curva de calibrado
modelo <- lm(1/tita ~ V + I(V^2) + I(V^3) ,data=datos)
summary(modelo)
## Extracción de los parámetros del modelo y obtencion de datos predichos
a_param <- modelo$coefficients[[1]]
b_param <- modelo$coefficients[[2]]
c_param <- modelo$coefficients[[3]]
d_param <- modelo$coefficients[[4]]
fitData <- tibble(x=min(datos$V):max(datos$V),fit = 1/(a_param+(b_param*x)+(c_param*(x^2))+(d_param*(x^3))))
## Representación de la curva característica
ggplot()+
  geom_line(data = fitData, aes(x=x, y=fit))+
  xlab("mV") + ylab(expression(theta))+
  theme_classic()

