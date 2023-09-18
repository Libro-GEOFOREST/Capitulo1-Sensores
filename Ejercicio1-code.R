########## GEOFOREST - Cap. 1 SENSORES / Ejemplo 1 ############################
############ Antonio Cachinero Vivar ########################################

#cargando librerías
rm(list=ls())
list_pkgs <- c("readxl", "ttalkR", "ggplot2","lubridate","devtools","dplyr","caret","tidyverse")
new_pkgs <- list_pkgs[!(list_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) install.packages(new_pkgs)

sapply(list_pkgs, require, character.only=TRUE)

#cargando datos 

data <- read.csv("ejercicio_flujodesavia.csv",sep=";", header = TRUE)

#procesado datos crudos de los sensores 
mydata_4D <- data
id_col <- data$TT_ID
id_col_ind <- data.frame(unique(id_col), 1:length(unique(id_col)))
colnames(id_col_ind) <- c("TT_ID", "ID")
mydata_4D$id_col_ind <- mydata_4D$TT_ID
for (i in 1:length(id_col_ind$ID)) {
  mydata_4D$id_col_ind <- replace(mydata_4D$id_col_ind, 
                                  mydata_4D$id_col_ind == id_col_ind$TT_ID[i], id_col_ind$ID[i])
}

Tref_0C <-  mydata_4D$Tref_0/10

Tref_0C[Tref_0C < -20] <- NA
Tref_0C[Tref_0C > 50] <- NA

ID <- unique(mydata_4D$TT_ID)

for (j in 1:length(ID)) {
  ts <- Tref_0C[mydata_4D$TT_ID == ID[j]]
  if (length(ts) < 11) {
    (next)()
  }
  ts_filt <- savitzkyGolay(ts, 0, 1, 11)
  Tref_0C[mydata_4D$TT_ID == ID[j]] <- ts_filt[1:length(ts)]
}


Tref_1C <- mydata_4D$Tref_1/10

Tref_1C[Tref_1C < -20] <- NA
Tref_1C[Tref_1C > 50] <- NA


ID <- unique(mydata_4D$TT_ID)
for (j in 1:length(ID)) {
  ts <- Tref_1C[mydata_4D$TT_ID == ID[j]]
  if (length(ts) < 11) {
    (next)()
  }
  ts_filt <- savitzkyGolay(ts, 0, 1, 11)
  Tref_1C[mydata_4D$TT_ID == ID[j]] <- ts_filt[1:length(ts)]
}


Theat_0C <- mydata_4D$Theat_0/10


Theat_0C[Theat_0C < -20] <- NA
Theat_0C[Theat_0C > 50] <- NA
ID <- unique(mydata_4D$TT_ID)
for (j in 1:length(ID)) {
  ts <- Theat_0C[mydata_4D$TT_ID == ID[j]]
  if (length(ts) < 11) {
    (next)()
  }
  ts_filt <- savitzkyGolay(ts, 0, 1, 11)
  Theat_0C[mydata_4D$TT_ID == ID[j]] <- ts_filt[1:length(ts)]
}

Theat_1C <-  mydata_4D$Theat_1/10

Theat_1C[Theat_1C < -20] <- NA
Theat_1C[Theat_1C > 50] <- NA
ID <- unique(mydata_4D$TT_ID)
for (j in 1:length(ID)) {
  ts <- Theat_1C[mydata_4D$TT_ID == ID[j]]
  if (length(ts) < 11) {
    (next)()
  }
  ts_filt <- savitzkyGolay(ts, 0, 1, 11)
  Theat_1C[mydata_4D$TT_ID == ID[j]] <- ts_filt[1:length(ts)]
}

dTon <- Theat_1C - Tref_1C

dToff <- Theat_0C - Tref_0C

dTmax <- (dTon - dToff)

ID <- unique(mydata_4D$TT_ID)

for (j in 1:(length(ID))) {
  df <- data.frame(dTmax[mydata_4D$TT_ID == ID[j]], mydata_4D$SDate[mydata_4D$TT_ID == 
                                                                      ID[j]])
  colnames(df) <- c("dTmax", "Date")
  if (length(na.omit(df$dTmax)) < 11) {
    (next)()
  }
  
  dTmax_day_ID <- aggregate(dTmax ~ Date, df, max, symplify = F, 
                            na.action = na.omit)
  daily_Tmax <- rep(NA, length(df$dTmax))
  
  df <- cbind(df, daily_Tmax)
  for (i in 1:length(df$daily_Tmax)) {
    df$daily_Tmax[i] <- max(df$dTmax[df$Date == df$Date[i]])
  }
  mydata_4D$daily_Tmax[mydata_4D$TT_ID == ID[j]] <- df$daily_Tmax
}


Fd <- 118.99 * ((mydata_4D$daily_Tmax - (dTon - dToff))/(dTon - dToff))^1.231


Fd[Fd > 1000] <- NA

ID <- unique(mydata_4D$TT_ID)
for (j in 1:(length(ID))) {
  ts <- Fd[mydata_4D$TT_ID == ID[j]]
  if (length(ts) < 11) {
    (next)()
  }
  ts_filt <- baytrends::fillMissing(ts, span = 24, Dates = NULL, 
                                    max.fill = 12)
  Fd[mydata_4D$TT_ID == ID[j]] <- ts_filt[1:length(ts)]
}
  
  mydata_4D$Timestamp <- as.POSIXct(as.character(mydata_4D$Timestamp))
  
#obtención de datos de flujo de savia y representación gráfica
  df1 <- data.frame(mydata_4D$Timestamp, Fd, mydata_4D$TT_ID)
  colnames(df1) <- c("Timestamp", "Fd", "id_col_ind")
  
  p1 <- ggplot(data = df1, aes(Timestamp, Fd*0.036, color = id_col_ind)) + 
    geom_point(aes(group = "whatever"), size = 0.4, na.rm = T) + 
    geom_line(aes(group = "whatever"), na.rm = T) + facet_grid(facets = mydata_4D$TT_ID ~ 
                                                                 ., margins = FALSE) + labs(y=expression("Fd"~(dm^3~dm^-2~h^-1)), x=expression("")) + 
    scale_x_datetime(minor_breaks = ("1 day")) + theme(legend.position = "none") + 
    theme(strip.text.y = element_text(angle = 0, hjust = 0)) + 
    ylim(0, max(Fd*0.036, na.rm = T))
  p1

