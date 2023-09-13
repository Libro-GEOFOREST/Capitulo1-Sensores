########## GEOFOREST - Cap. 1 SENSORES / Ejemplo 3 ############################
############ Francisco Jose Ruiz Gomez ########################################

## Cargamos las librerías y obtenemos los datos:
library(tidyverse)
library(ggplot2)
url <- "https://raw.githubusercontent.com/Libro-GEOFOREST/Capitulo1-Sensores/Ejemplo-3/calibracion_TDR.csv"
datos <- read.csv(url)

## Enfrentamos los datos esperados a los datos reales para las 117 observaciones
## Antes necesitamos nuevamente los parametros del modelo
modelo <- lm(1/tita ~ V + I(V^2) + I(V^3) ,data=datos)

## Extracción de los parámetros del modelo y obtencion de datos predichos
a_param <- modelo$coefficients[[1]]
b_param <- modelo$coefficients[[2]]
c_param <- modelo$coefficients[[3]]
d_param <- modelo$coefficients[[4]]
fitData <- tibble(x=datos$V,fit = 1/(a_param+(b_param*x)+(c_param*(x^2))+(d_param*(x^3))))

## Pareamos los datos esperados con los reales
datos$esperados <- fitData$fit

ggplot(data = datos, aes(x = tita, y = esperados))+
  geom_point()+
  stat_smooth(method = "lm", se = FALSE)+
  labs(title = expression(theta), x = "Valores observados", y = "Valores ajustados")+
  theme_classic()
