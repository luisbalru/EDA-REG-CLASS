###########################################################
# INTRODUCCIÓN A LA CIENCIA DE DATOS
# Autor: Luis Balderas Ruiz
# EDA+Regresion
# Dataset: wankara
############################################################
library(ggplot2)
library(tidyverse)
install.packages("outliers")
library(outliers)

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

library("scales")
wankara_scale = sapply(wankara,rescale)
wankara_scale = as.data.frame(wankara_scale)
summary(wankara_scale)

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


mean_t1 = wankara_scale %>% filter(Mean_temperature < 0.5 & Mean_temperature > 0.3)
ggplot(data=mean_t1, aes(x=Mean_temperature)) +
  geom_histogram(binwidth = binwd(mean_t1$Mean_temperature), fill="blue") +
  ggtitle("Histograma de temperatura media (entre 0.3 y 0.5)") +
  labs(x="Temperatura média", y="Count\nof Records")


#############################################################################################
# HISTOGRAMAS

# Max-temperature
ggplot(data=wankara, aes(x=Max_temperature)) +
  geom_histogram(binwidth = binwd(wankara$Max_temperature),fill="blue") +
  ggtitle("Histograma de temperatura máxima") +
  labs(x="Temperatura máxima", y="Count\nof Records")

# Min-temperature
ggplot(data=wankara, aes(x=Min_temperature)) +
  geom_histogram(binwidth = binwd(wankara$Min_temperature),fill="blue") +
  ggtitle("Histograma de temperatura mínima") +
  labs(x="Temperatura mínima", y="Count\nof Records")

# Dewpoint
ggplot(data=wankara, aes(x=Dewpoint)) +
  geom_histogram(binwidth = binwd(wankara$Dewpoint),fill="blue") +
  ggtitle("Histograma Dewpoint") +
  labs(x="Dewpoint", y="Count\nof Records")

# Precipitation
ggplot(data=wankara, aes(x=Precipitation)) +
  geom_histogram(binwidth = binwd(wankara$Precipitation),fill="blue") +
  ggtitle("Histograma Precipitaciones") +
  labs(x="Precipitaciones", y="Count\nof Records")

# Sea level pressure
ggplot(data=wankara, aes(x=Sea_level_pressure)) +
  geom_histogram(binwidth = binwd(wankara$Sea_level_pressure),fill="blue") +
  ggtitle("Histograma Sea_level_pressure") +
  labs(x="Sea_level_pressure", y="Count\nof Records")

# Standard pressure
ggplot(data=wankara, aes(x=Standard_pressure)) +
  geom_histogram(binwidth = binwd(wankara$Standard_pressure),fill="blue") +
  ggtitle("Histograma Standard pressure") +
  labs(x="Standard pressure", y="Count\nof Records")

# Visibility
ggplot(data=wankara, aes(x=Visibility)) +
  geom_histogram(binwidth = binwd(wankara$Visibility),fill="blue") +
  ggtitle("Histograma Visibility") +
  labs(x="Visibility", y="Count\nof Records")

# Wind speed
ggplot(data=wankara, aes(x=Wind_speed)) +
  geom_histogram(binwidth = binwd(wankara$Wind_speed),fill="blue") +
  ggtitle("Histograma Wind speed") +
  labs(x="Wind speed", y="Count\nof Records")

# Max Wind speed
ggplot(data=wankara, aes(x=Max_wind_speed)) +
  geom_histogram(binwidth = binwd(wankara$Max_wind_speed),fill="blue") +
  ggtitle("Histograma Max Wind speed") +
  labs(x="Max Wind speed", y="Count\nof Records")

# Histograma temperatura media
ggplot(data=wankara, aes(x=Mean_temperature)) +
  geom_histogram(binwidth=binwd(wankara$Mean_temperature),fill="blue") +
  ggtitle("Histograma de temperatura media") +
  labs(x="Temperatura média", y="Count\nof Records")

#############################################################################################
# OUTLIERS

outlier(wankara$Max_temperature)
