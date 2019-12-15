###########################################################
# INTRODUCCIÓN A LA CIENCIA DE DATOS
# Autor: Luis Balderas Ruiz
# EDA+Regresion
# Dataset: wankara
############################################################
library(ggplot2)
library(tidyverse)
library(outlier)

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

# Histograma temperatura media
ggplot(data=wankara_scale, aes(x=Mean_temperature)) +
  geom_histogram(fill="blue") +
  ggtitle("Histograma de temperatura media") +
  labs(x="Temperatura média", y="Count\nof Records")


mean_t1 = wankara_scale %>% filter(Mean_temperature < 0.5 & Mean_temperature > 0.3)
ggplot(data=mean_t1, aes(x=Mean_temperature)) +
  geom_histogram(binwidth = binwd(mean_t1$Mean_temperature), fill="blue") +
  ggtitle("Histograma de temperatura media (entre 0.3 y 0.5)") +
  labs(x="Temperatura média", y="Count\nof Records")

