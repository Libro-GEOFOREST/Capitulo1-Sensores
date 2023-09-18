#Antonio Cachinero Vivar#
#UCO-ERSAF#

rm(list=ls())
list_pkgs <- c("readxl","ggplot2","lubridate","devtools","dplyr","caret","tidyverse","ggarrange")
new_pkgs <- list_pkgs[!(list_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) install.packages(new_pkgs)

sapply(list_pkgs, require, character.only=TRUE)

##################
data <- read.csv("ejercicio_dendrometro.csv",sep=";", header = TRUE)

  df1 <- data.frame(data$Timestamp, data$individuo, data$incremento, data$temperatura)
  colnames(df1) <- c("Timestamp", "individuo", "incremento","temperatura")


  p1 <- ggplot(data = df1, aes(Timestamp, incremento, color = individuo)) + 
    geom_point(aes(group = "whatever"), size = 0.4) + 
    geom_line(aes(group = "whatever")) + facet_grid(facets = data$individuo ~ 
                                                                 ., margins = FALSE) + labs(y=expression("Incremento"~(mm)), x=expression("")) + 
    scale_x_datetime(minor_breaks = ("1 day")) + theme(legend.position = "none") + 
    theme(strip.text.y = element_text(angle = 0, hjust = 0)) + 
    ylim(min(incremento, na.rm = T), max(incremento, na.rm = T))
  p1
  
  p2 <- ggplot(data = df1, aes(Timestamp, temperatura, color = individuo)) + 
    geom_point(aes(group = "whatever"), size = 0.4) + 
    geom_line(aes(group = "whatever")) + facet_grid(facets = data$individuo ~ 
                                                      ., margins = FALSE) + labs(y=expression("Temperatura"~(ºC)), x=expression("")) + 
    scale_x_datetime(minor_breaks = ("1 day")) + theme(legend.position = "none") + 
    theme(strip.text.y = element_text(angle = 0, hjust = 0)) + 
    ylim(min(temperatura, na.rm = T), max(temperatura, na.rm = T))
  p2

  ggarrange(p1,p2, ncol=2, common.legend = TRUE, legend="none")
 
  factor <- 0.7 
  p3 <- ggplot(data = df1, aes(Timestamp, incremento, color = individuo)) + 
    geom_point(aes(group = "whatever"), size = 0.4) + 
    geom_line(aes(group = "whatever")) + facet_grid(facets = data$individuo ~ 
                                                      ., margins = FALSE) + labs(y=expression("Incremento"~(mm)), x=expression("")) + 
    geom_line(mapping=aes( y=temperatura/factor, group = "whatever"),color="darkgreen", lwd=0.2)+
    scale_x_datetime(minor_breaks = ("1 day")) + theme(legend.position = "none") + 
    scale_y_continuous(sec.axis=sec_axis(trans= ~.*factor,name= "Temperatura (ºC)"))+
    theme(strip.text.y = element_text(angle = 0, hjust = 0)) 
  p3
