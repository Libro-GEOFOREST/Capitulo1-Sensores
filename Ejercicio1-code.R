########## GEOFOREST - Cap. 1 SENSORES / Ejemplo 1 ############################
############ Antonio Cachinero Vivar ########################################

rm(list=ls())
list_pkgs <- c("readxl", "ttalkR", "prospectr", "ggplot2","lubridate","devtools","writexl","dplyr","gridExtra","caret","ggpubr","signal","scales","reshape2","knitr","rmarkdown","tidyverse")
new_pkgs <- list_pkgs[!(list_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) install.packages(new_pkgs)

sapply(list_pkgs, require, character.only=TRUE)


#########  GETTING SAPFLOW   #########

data <- read_xlsx("C:/Users/Dell Latitude/OneDrive - Universidad de Córdoba/Dendrolatlab/Acachinero/Libro_GEOFOREST/Capitulo_I/cap_I_sensoriz_ejerc_1/ejercicio_sapflow.xlsx")


#write_xlsx(data,"C:/Users/Dell Latitude/OneDrive - Universidad de Córdoba/Dendrolatlab/Acachinero/Libro_GEOFOREST/Capitulo_I/cap_I_sensoriz_ejerc_1/ejercicio_sapflow.xlsx")

data <- data[data$SDate >= "2023-07-01",] #eliminate data before 2023-03-30 eg: 1970-01-01
data <- data[data$SDate <= "2023-07-08",] 



View(data)
head(data)

mydata_4D <-data

####SAPFLOW
ttGranier
function (mydata_4D, plot_label) 
{
  mydata_4D <- data
  id_col <- data$TT_ID
  id_col_ind <- data.frame(unique(id_col), 1:length(unique(id_col)))
  colnames(id_col_ind) <- c("TT_ID", "ID")
  mydata_4D$id_col_ind <- mydata_4D$TT_ID
  for (i in 1:length(id_col_ind$ID)) {
    mydata_4D$id_col_ind <- replace(mydata_4D$id_col_ind, 
                                    mydata_4D$id_col_ind == id_col_ind$TT_ID[i], id_col_ind$ID[i])
  }
  
  # Tref_0C <- (127.6 - 0.006045 * mydata_4D$Tref_0 + 1.26e-07 * 
  #   mydata_4D$Tref_0^2 - 1.15e-12 * mydata_4D$Tref_0^3)/10
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
  
  # Tref_1C <- (127.6 - 0.006045 * mydata_4D$Tref_1 + 1.26e-07 * 
  #   mydata_4D$Tref_1^2 - 1.15e-12 * mydata_4D$Tref_1^3)
  
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
  
  # Theat_0C <- (127.6 - 0.006045 * mydata_4D$Theat_0 + 1.26e-07 * 
  #   mydata_4D$Theat_0^2 - 1.15e-12 * mydata_4D$Theat_0^3)/10
  
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
  
  # Theat_1C <- (127.6 - 0.006045 * mydata_4D$Theat_1 + 1.26e-07 * 
  #   mydata_4D$Theat_1^2 - 1.15e-12 * mydata_4D$Theat_1^3)
  
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
  
  # Fd <- 118.99 * (mydata_4D$daily_Tmax)^1.231
  
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
  
  
  df1 <- data.frame(mydata_4D$Timestamp, Fd, mydata_4D$TT_ID)
  colnames(df1) <- c("Timestamp", "Fd", "id_col_ind")
  
  summary(df1)
  
  # Outlier removal 
  out <- df1
  
  df1$Fd <- ifelse(df1$Fd < quantile(df1$Fd, probs=c(.05), na.rm = TRUE), NaN, df1$Fd)
  
  Q <- quantile(df1$Fd, probs=c(.75), na.rm = TRUE)
  iqr <- IQR(df1$Fd, na.rm=TRUE)
  
  up <-  Q[1]+1.5*iqr # Upper Range  
  df1$Fd <- ifelse(df1$Fd > up, NaN, df1$Fd)
  summary(df1)
#View(df1)

  
  p <- ggplot(data = df1, aes(Timestamp, Fd*0.036, color = id_col_ind)) + 
    geom_point(aes(group = "whatever"), size = 0.4, na.rm = T) + 
    geom_line(aes(group = "whatever"), na.rm = T) + facet_grid(facets = mydata_4D$TT_ID ~ 
                                                                 ., margins = FALSE) + labs(x = "Timestamp", y = "sap flow density (l dm-2 h-1)") + 
    scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) + 
    scale_x_datetime(minor_breaks = ("1 day")) + theme(legend.position = "none") + 
    theme(strip.text.y = element_text(angle = 0, hjust = 0)) + 
    ylim(0, 1)
  p
  
  
  if (plot_label == "all_in_one") {
    p <- ggplot(data = df1, aes(Timestamp, Fd*0.036)) + geom_point(aes(colour = id_col_ind), 
                                                                   size = 0.2, na.rm = T) + scale_color_gradientn(colours = hcl.colors(30, 
                                                                                                                                       palette = "viridis")) + labs(x = "Timestamp", y = "sap flow density (l dm-2 h-1)") + 
      scale_x_datetime(minor_breaks = ("1 week")) + theme(legend.position = "none") + 
      ylim(0, 1)
    print(p)
  }
  if (plot_label == "split") {
    p <- ggplot(data = df1, aes(Timestamp, Fd*0.036, color = id_col_ind)) + 
      geom_point(aes(group = "whatever"), size = 0.4, na.rm = T) + 
      geom_line(aes(group = "whatever"), na.rm = T) + facet_grid(facets = mydata_4D$TT_ID ~ 
                                                                   ., margins = FALSE) + labs(x = "Timestamp", y = "sap flow density (l dm-2 h-1)") + 
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) + 
      scale_x_datetime(minor_breaks = ("1 week")) + theme(legend.position = "none") + 
      theme(strip.text.y = element_text(angle = 0, hjust = 0)) + 
      ylim(0, 1)
    print(p)
    p_ttGranier <<- p
  }
  if (plot_label == "none") {
  }

  df_ttGranier <- data.frame(mydata_4D$Timestamp, Fd, mydata_4D$TT_ID)
  colnames(df_ttGranier) <- c("Timestamp", "Fd", "TT_ID")
  .GlobalEnv$df_ttGranier <- df_ttGranier
}

ttGranier(data, "split")
  
  #mydata_4D$Timestamp <- as.POSIXct(paste(mydata_4D$SDate, mydata_4D$STime), tz="GMT", format="%d.%m.%y %H:%M:%S")
  #mydata_4D$Timestamp <- as.POSIXct(as.character(mydata_4D$Timestamp))
  
  df1 <- data.frame(mydata_4D$Timestamp, Fd, mydata_4D$TT_ID)
  colnames(df1) <- c("Timestamp", "Fd", "TT_ID")
  
  #write_xlsx(df1,"sapflow_rabanales_2.xlsx")
  
  summary(df1)
  
  # # Outlier removal 
  # out <- df1
  # 
  # df1$Fd <- ifelse(df1$Fd < quantile(df1$Fd, probs=c(.05), na.rm = TRUE), NaN, df1$Fd)
  # 
  # Q <- quantile(df1$Fd, probs=c(.75), na.rm = TRUE)
  # iqr <- IQR(df1$Fd, na.rm=TRUE)
  # 
  # up <-  Q[1]+1.5*iqr # Upper Range  
  # df1$Fd <- ifelse(df1$Fd > up, NaN, df1$Fd)
  # 
  # summary(df1)
  
  p <- ggplot(data = df1, aes(Timestamp, Fd*0.036)) + 
    geom_point(aes(group = "whatever"), size = 0.4, na.rm = T, color= "red") + 
    geom_line(aes(group = "whatever"), na.rm = T) + facet_grid(facets = mydata_4D$TT_ID ~ 
                                                                 ., margins = FALSE) + labs(x = "", y = "sap flow density (l dm-2 h-1)") + 
    scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) + 
    #scale_x_datetime(minor_breaks = ("1 day")) + theme(legend.position = "none") + 
    scale_x_datetime(minor_breaks = ("1 day"), date_labels = "%d-%m-%y") + theme(legend.position = "none")+ 
    # theme(strip.text.y = element_text(angle = 0, hjust = 0))
    ylim(0, 1)
  p
  
  


########################## MEAN ALL SENSORS IN 24 HOURS DISTRIBUTION
# library(lubridate)
df1$doy <- yday(df1$Timestamp)

df1$HOUR <- strftime(df1$Timestamp, format="%H")

tmp <- df1 %>% group_by(HOUR, id_col_ind) %>% summarize(Fd = mean(Fd, na.rm=TRUE)*0.036)
plot(tmp$HOUR, tmp$Fd)

p<- ggplot(data = tmp , aes(HOUR, Fd, group=TT_ID)) + geom_point()+  labs(x = "Timestamp", y = "sap flow density (l dm-2 h-1)") 

print(p)

###########Plotting climate###########

setwd("C:/Users/Dell Latitude/OneDrive - Universidad de Córdoba/Dendrolatlab/Acachinero/VertLab/Field_data/Clima/Rabanales_IAS")
#setwd("C:/Users/Dell Latitude/OneDrive - Universidad de Córdoba/Dendrolatlab/Acachinero/VertLab/Field_data/Clima/Rabanales_IAS")
df2 <- read_excel("clima_rabanales.xlsx",1)
#df2 <-df2 [c(36:129),] 

df1$doy <- yday(df1$Timestamp)
#df1 <- df1 %>% group_by(doy, TT_ID)

df<- df %>% group_by(doy, Timestamp) %>% 
 summarize(TT_ID = mean(TT_ID), Prec = mean(Prec, na.rm=TRUE),Tmed = mean(Tmed, na.rm=TRUE), HR = mean(HR, na.rm=TRUE),
            DPV = mean(DPV, na.rm=TRUE),Radiation = mean(Radiation, na.rm=TRUE), Rn = mean(Rn, na.rm=TRUE),
            ET0 = mean(ET0, na.rm=TRUE))

df2$HR <- as.numeric(as.character(df2$HR))
df2$Prec <- as.numeric(as.character(df2$Prec))
df2$ET0 <- as.numeric(as.character(df2$ET0))

df <- merge(df1, df2, by= c("doy"), na.rm=TRUE, all.y =T)  %>% distinct()

summary (df)

time_vline <- as.numeric("63") ##doy = 63 = thinning day
time_vline <- which(df$doy %in% time_vline)

factor <- 10 ###la escala de dividir VWC max (32.8) entre Fd max (16.8)
p1 <- ggplot(data = df, aes(Timestamp, Fd*0.036 )) + 
  geom_boxplot(aes(group = doy, color=treatment), size = 0.4, na.rm = T)+ 
  # geom_boxplot(data = df, aes(Timestamp, VWC, group =doy))+
  #facet_grid(facets = df$TT_ID ~., margins = FALSE)+  
  geom_boxplot(mapping=aes( y=ET0/factor, group = doy),color="darkgreen", lwd=1)+
  # scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) + 
  scale_x_datetime(minor_breaks = ("1 day")) + theme(legend.position = "none") +
  scale_y_continuous(sec.axis=sec_axis(trans= ~.*factor,name= "ET0 (mm/day)"))+
  theme(strip.text.y = element_text(angle = 0, hjust = 0)) + 
  # ylim(0, max(Fd*0.036, na.rm = T))+
  labs(y=expression("Fd"~(dm^3~dm^-2~h^-1)), title="Pinus pinea Rabanales")+
  labs (x="time")
p1

factor <- 50 ###la escala de dividir VWC max (32.8) entre Fd max (16.8)
p2 <- ggplot(data = df, aes(Timestamp, Fd*0.036 )) + 
  geom_smooth(aes(group = doy, fill = "palette"), size = 0.4, na.rm = T)+ 
  # geom_boxplot(data = df, aes(Timestamp, VWC, group =doy))+
  facet_grid(facets = df$TT_ID ~., margins = FALSE)+  
  geom_boxplot(mapping=aes( y=Prec/factor, group = doy),color="darkgreen", lwd=1)+
  scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) + 
  scale_x_datetime(minor_breaks = ("1 day")) + theme(legend.position = "none") +
  scale_y_continuous(sec.axis=sec_axis(trans= ~.*factor,name= "Prec (mm/day)"))+
  theme(strip.text.y = element_text(angle = 0, hjust = 0)) + 
  # ylim(0, max(Fd*0.036, na.rm = T))+
  labs(y=expression("Fd"~(dm^3~dm^-2~h^-1)), title="Pinus pinea Rabanales")+
  labs (x="time")
p2

factor <- 120 ###la escala de dividir VWC max (32.8) entre Fd max (16.8)
p3 <- ggplot(data = df, aes(Timestamp, Fd*0.036 )) + 
  geom_line(aes(group = doy, color=treatment), size = 0.4, na.rm = T)+ 
  # geom_boxplot(data = df, aes(Timestamp, VWC, group =doy))+
  #facet_grid(facets = df$TT_ID ~.,)+  
  geom_boxplot(mapping=aes( y=HR/factor, group = doy),color="darkgreen", lwd=1)+
  #scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) + 
  scale_x_datetime(minor_breaks = ("1 day")) + theme(legend.position = "none") +
  scale_y_continuous(sec.axis=sec_axis(trans= ~.*factor,name= "HR (%)"))+
  theme(strip.text.y = element_text(angle = 0, hjust = 0)) + 
  # ylim(0, max(Fd*0.036, na.rm = T))+
  labs(y=expression("Fd"~(dm^3~dm^-2~h^-1)), title="Pinus pinea Rabanales")+
  labs (x="time")
p3

factor <- 10 ###la escala de dividir VWC max (32.8) entre Fd max (16.8)
p2 <- ggplot(data = df, aes(Timestamp, Fd*0.036 )) + 
  geom_line(aes(group = doy, fill = "palette", color=treatment), size = 0.4, na.rm = T)+ 
  # geom_boxplot(data = df, aes(Timestamp, VWC, group =doy))+
  #facet_grid(facets = df$TT_ID ~., margins = FALSE)+  
  geom_boxplot(mapping=aes( y=ET0/factor, group = doy),color="darkgreen", lwd=1)+
  #scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) + 
  scale_x_datetime(minor_breaks = ("1 day")) + theme(legend.position = "none") +
  scale_y_continuous(sec.axis=sec_axis(trans= ~.*factor,name= "ET0 (mm/day)"))+
  theme(strip.text.y = element_text(angle = 0, hjust = 0)) + 
  # ylim(0, max(Fd*0.036, na.rm = T))+
  labs(y=expression("Fd"~(dm^3~dm^-2~h^-1)), title="Pinus pinea Rabanales")+
  labs (x="time")
p2

plot1 <- p3 + geom_vline(xintercept = as.numeric(df$Timestam[time_vline]), col="darkgreen", lwd=1)
plot1

#grid.arrange(plot2,plot1, ncol=1)
ggarrange(plot2,plot1, ncol=1, common.legend = TRUE, legend="right")
