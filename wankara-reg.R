###########################################################
# INTRODUCCIÓN A LA CIENCIA DE DATOS
# Autor: Luis Balderas Ruiz
# EDA+Regresion
# Dataset: wankara (01/01/1994 to 28/05/1998)
############################################################

library(ggplot2)
library(tidyverse)


binwd = function(data){
  size = length(data)
  dt = sd(data)
  cr = size^(1/3)
  return(1/(cr)*dt*3.49)
}
wankara = read.csv("./data/wankara/wankara.dat",header=FALSE, comment.char = "@")
colnames(wankara) = c("Max_temperature", "Min_temperature", "Dewpoint", "Precipitation", "Sea_level_pressure", "Standard_pressure", "Visibility", "Wind_speed", "Max_wind_speed","Mean_temperature")


# Resumen estadístico
summary(wankara)

# Visualización de las variables respecto de mean_temperature
temp <- wankara
plotY <- function (x,y) {
  plot(temp[,y]~temp[,x], xlab=paste(names(temp)[x]),
       ylab=names(temp)[y])
}
par(mfrow=c(3,4)) #Si margin too large => (2,3)
x <- sapply(1:(dim(temp)[2]-1), plotY, dim(temp)[2])
par(mfrow=c(1,1))


#############################################################################################
# HISTOGRAMAS

# Max-temperature
skewness(wankara$Max_temperature)
kurtosis(wankara$Max_temperature)
ggplot(data=wankara, aes(x=Max_temperature)) +
  geom_histogram(binwidth = binwd(wankara$Max_temperature),fill="blue") +
  ggtitle("Histograma de temperatura máxima") +
  labs(x="Temperatura máxima", y="Count\nof Records")

# Min-temperature
skewness(wankara$Min_temperature)
kurtosis(wankara$Min_temperature)
ggplot(data=wankara, aes(x=Min_temperature)) +
  geom_histogram(binwidth = binwd(wankara$Min_temperature),fill="blue") +
  ggtitle("Histograma de temperatura mínima") +
  labs(x="Temperatura mínima", y="Count\nof Records")

# Dewpoint
skewness(wankara$Dewpoint)
kurtosis(wankara$Dewpoint)
ggplot(data=wankara, aes(x=Dewpoint)) +
  geom_histogram(binwidth = binwd(wankara$Dewpoint),fill="blue") +
  ggtitle("Histograma Dewpoint") +
  labs(x="Dewpoint", y="Count\nof Records")

# Precipitation
skewness(wankara$Precipitation)
kurtosis(wankara$Precipitation)
ggplot(data=wankara, aes(x=Precipitation)) +
  geom_histogram(binwidth = binwd(wankara$Precipitation),fill="blue") +
  ggtitle("Histograma Precipitaciones") +
  labs(x="Precipitaciones", y="Count\nof Records")

# Sea level pressure
skewness(wankara$Sea_level_pressure)
kurtosis(wankara$Sea_level_pressure)
ggplot(data=wankara, aes(x=Sea_level_pressure)) +
  geom_histogram(binwidth = binwd(wankara$Sea_level_pressure),fill="blue") +
  ggtitle("Histograma Sea_level_pressure") +
  labs(x="Sea_level_pressure", y="Count\nof Records")

# Standard pressure
skewness(wankara$Standard_pressure)
kurtosis(wankara$Standard_pressure)
ggplot(data=wankara, aes(x=Standard_pressure)) +
  geom_histogram(binwidth = binwd(wankara$Standard_pressure),fill="blue") +
  ggtitle("Histograma Standard pressure") +
  labs(x="Standard pressure", y="Count\nof Records")

# Visibility
skewness(wankara$Visibility)
kurtosis(wankara$Visibility)
ggplot(data=wankara, aes(x=Visibility)) +
  geom_histogram(binwidth = binwd(wankara$Visibility),fill="blue") +
  ggtitle("Histograma Visibility") +
  labs(x="Visibility", y="Count\nof Records")

# Wind speed
skewness(wankara$Wind_speed)
kurtosis(wankara$Wind_speed)
ggplot(data=wankara, aes(x=Wind_speed)) +
  geom_histogram(binwidth = binwd(wankara$Wind_speed),fill="blue") +
  ggtitle("Histograma Wind speed") +
  labs(x="Wind speed", y="Count\nof Records")

# Max Wind speed
skewness(wankara$Max_wind_speed)
kurtosis(wankara$Max_wind_speed)
ggplot(data=wankara, aes(x=Max_wind_speed)) +
  geom_histogram(binwidth = binwd(wankara$Max_wind_speed),fill="blue") +
  ggtitle("Histograma Max Wind speed") +
  labs(x="Max Wind speed", y="Count\nof Records")

# Histograma temperatura media
skewness(wankara$Mean_temperature)
kurtosis(wankara$Mean_temperature)
ggplot(data=wankara, aes(x=Mean_temperature)) +
  geom_histogram(binwidth=binwd(wankara$Mean_temperature),fill="blue") +
  ggtitle("Histograma de temperatura media") +
  labs(x="Temperatura média", y="Count\nof Records")

mean_t1 = wankara_scale %>% filter(Mean_temperature < 0.5 & Mean_temperature > 0.3)
ggplot(data=mean_t1, aes(x=Mean_temperature)) +
  geom_histogram(binwidth = binwd(mean_t1$Mean_temperature), fill="blue") +
  ggtitle("Histograma de temperatura media (entre 0.3 y 0.5)") +
  labs(x="Temperatura média", y="Count\nof Records")


#############################################################################################
# VALORES PERDIDOS

wankara[is.na(wankara)]

#############################################################################################
# OUTLIERS
install.packages("outliers")
library(outliers)

sapply(wankara,outlier)
sapply(wankara,outlier,opposite=TRUE)


#############################################################################################
# DIVISIÓN POR MESES

dias = seq(1,1609,30)
max_temp = c()
min_temp = c()
dewp = c()
precip = c()
slp = c()
sp = c()
visib = c()
Ws = c() 
Msp = c()
Mean_temp = c()
for(i in 1:54){
  if(i == 54){
    max_temp = append(max_temp, mean(wankara$Max_temperature[dias[i]:1609]))
    min_temp = append(min_temp,mean(wankara$Min_temperature[dias[i]:1609]))
    dewp = append(dewp, mean(wankara$Dewpoint[dias[i]:1609]))
    precip = append(precip, mean(wankara$Precipitation[dias[i]:1609]))
    slp = append(slp,mean(wankara$Sea_level_pressure[dias[i]:1609]))
    sp = append(sp, mean(wankara$Standard_pressure[dias[i]:1609]))
    visib = append(visib, mean(wankara$Visibility[dias[i]:1609]))
    Ws = append(Ws, mean(wankara$Wind_speed[dias[i]:1609]))
    Msp = append(Msp, mean(wankara$Max_wind_speed[dias[i]:1609]))
    Mean_temp = append(Mean_temp, mean(wankara$Mean_temperature[dias[i]:1609]))
  }
  else{
    max_temp = append(max_temp, mean(wankara$Max_temperature[dias[i]:(dias[i+1]-1)]))
    min_temp = append(min_temp,mean(wankara$Min_temperature[dias[i]:(dias[i+1]-1)]))
    dewp = append(dewp, mean(wankara$Dewpoint[dias[i]:(dias[i+1]-1)]))
    precip = append(precip, mean(wankara$Precipitation[dias[i]:(dias[i+1]-1)]))
    slp = append(slp,mean(wankara$Sea_level_pressure[dias[i]:(dias[i+1]-1)]))
    sp = append(sp, mean(wankara$Standard_pressure[dias[i]:(dias[i+1]-1)]))
    visib = append(visib, mean(wankara$Visibility[dias[i]:(dias[i+1]-1)]))
    Ws = append(Ws, mean(wankara$Wind_speed[dias[i]:(dias[i+1]-1)]))
    Msp = append(Msp, mean(wankara$Max_wind_speed[dias[i]:(dias[i+1]-1)]))
    Mean_temp = append(Mean_temp, mean(wankara$Mean_temperature[dias[i]:(dias[i+1]-1)]))
  }
}
meses = rep(month.abb,5)[1:54]
j=94
for(i in 1:54){
  if(i%%12 == 1){
    j = j+1
  }
  meses[i] = paste(meses[i],j,sep="")
}
df = as.data.frame(cbind(meses, max_temp))
ggplot(df,aes(x=meses, y=max_temp))+geom_point()
ggplot(df,aes(x=meses,y=min_temp)) + geom_point()

#############################################################################################
# HIPÓTESIS

# Hipótesis: mayor temperatura máxima, mayor temperatura media
ggplot(data=wankara_scale, aes(x=wankara_scale$Max_temperature, y=wankara_scale$Mean_temperature)) +
  geom_point(alpha=.4, size=4, color="#880011") +
  ggtitle("Temperatura máxima vs Temperatura media") +
  labs(x="Temperatura máxima", y="Temperatura media")

# Hipótesis: mayor temperatura mínima, mayor temperatura media
ggplot(data=wankara_scale, aes(x=wankara_scale$Min_temperature, y=wankara_scale$Mean_temperature)) +
  geom_point(alpha=.4, size=4, color="#880011") +
  ggtitle("Temperatura mínima vs Temperatura media") +
  labs(x="Temperatura mínima", y="Temperatura media")


###########################################################################################
# CORRELACIÓN

install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(wankara, histogram=TRUE, pch=19)

#############################################################################################3
# REESCALADO

library("scales")
wankara_scale = sapply(wankara,rescale)
wankara_scale = as.data.frame(wankara_scale)
summary(wankara_scale)
