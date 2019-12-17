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

# Días en cada mes
dias_mes = c(31,28,31,30,31,30,31,31,30,31,30,31)
# Días del dataset
dias = rep(dias_mes,5)[1:53]
# 1996 fue bisiesto
dias[12*3+2] = 29

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
actual = 1
for(i in 1:53){
  if(i == 53){
    max_temp = append(max_temp, mean(wankara$Max_temperature[actual:1609]))
    min_temp = append(min_temp,mean(wankara$Min_temperature[actual:1609]))
    dewp = append(dewp, mean(wankara$Dewpoint[actual:1609]))
    precip = append(precip, mean(wankara$Precipitation[actual:1609]))
    slp = append(slp,mean(wankara$Sea_level_pressure[actual:1609]))
    sp = append(sp, mean(wankara$Standard_pressure[actual:1609]))
    visib = append(visib, mean(wankara$Visibility[actual:1609]))
    Ws = append(Ws, mean(wankara$Wind_speed[actual:1609]))
    Msp = append(Msp, mean(wankara$Max_wind_speed[actual:1609]))
    Mean_temp = append(Mean_temp, mean(wankara$Mean_temperature[actual:1609]))
  }
  else{
    max_temp = append(max_temp, mean(wankara$Max_temperature[actual:actual+dias[i]-1]))
    min_temp = append(min_temp,mean(wankara$Min_temperature[actual:actual+dias[i]-1]))
    dewp = append(dewp, mean(wankara$Dewpoint[actual:actual+dias[i]-1]))
    precip = append(precip, mean(wankara$Precipitation[actual:actual+dias[i]-1]))
    slp = append(slp,mean(wankara$Sea_level_pressure[actual:actual+dias[i]-1]))
    sp = append(sp, mean(wankara$Standard_pressure[actual:actual+dias[i]-1]))
    visib = append(visib, mean(wankara$Visibility[actual:actual+dias[i]-1]))
    Ws = append(Ws, mean(wankara$Wind_speed[actual:actual+dias[i]-1]))
    Msp = append(Msp, mean(wankara$Max_wind_speed[actual:actual+dias[i]-1]))
    Mean_temp = append(Mean_temp, mean(wankara$Mean_temperature[actual:actual+dias[i]-1]))
    actual = actual+dias[i]
    print(actual)
  }
}
meses = rep(month.abb,5)[1:53]
j=94
for(i in 1:53){
  if(i%%12 == 1 && i > 12){
    j = j+1
  }
  meses[i] = paste(meses[i],j,sep="")
}
df = as.data.frame(cbind(meses, max_temp,min_temp))
df$meses = factor(df$meses,levels=df$meses)
ggplot(df[1:12,],aes(x=meses, y=max_temp))+geom_point()
ggplot(df[1:12,],aes(x=meses,y=min_temp)) + geom_point()
# Los datos no parecen tener un orden cronológico

############################################################################################
# Normalidad
library(nortest)
library(car)

# Ninguna variable es normal
sapply(wankara,shapiro.test)
sapply(wankara,lillie.test)
sapply(wankara,qqPlot)

#############################################################################################
# HIPÓTESIS

# Hipótesis: mayor temperatura máxima, mayor temperatura media
ggplot(data=wankara, aes(x=wankara$Max_temperature, y=wankara$Mean_temperature)) +
  geom_point(alpha=.4, size=4, color="#880011") +
  ggtitle("Temperatura máxima vs Temperatura media") +
  labs(x="Temperatura máxima", y="Temperatura media")

# Hipótesis: mayor temperatura mínima, mayor temperatura media
ggplot(data=wankara, aes(x=wankara$Min_temperature, y=wankara$Mean_temperature)) +
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


# Modelo lineal simple con la variable con más correlación: Max_temperature
fit_mls = lm(wankara_scale$Mean_temperature~wankara_scale$Max_temperature)
summary(fit_mls)

par(mfrow=c(1,1))
plot(wankara_scale$Mean_temperature~wankara_scale$Max_temperature)
abline(fit_mls,col="red")
confint(fit_mls)

# Error cuadrático medio
yprime=predict(fit_mls,data.frame(Max_temp=wankara_scale$Max_temperature))
sqrt(sum(abs(wankara_scale$Mean_temperature-yprime)^2)/length(yprime))

# Cross-validation

nombre <- "./data/wankara/wankara"
run_lm_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@", header=FALSE)
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@", header=FALSE)
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") {
    test <- x_tra
  }
  else {
    test <- x_tst
  }
  fitMulti=lm(Y~X1,x_tra)
  yprime=predict(fitMulti,test)
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
resultados_mls_train = sapply(1:5,run_lm_fold,nombre,"train")
resultados_mls_test = sapply(1:5,run_lm_fold,nombre,"test")
lmMSEtrain<-mean(resultados_mls_train)
lmMSEtest<-mean(resultados_mls_test)

# MODELO LINEAL MÚLTIPLE
# BACKWARD MODEL

fit_mlm1=lm(wankara_scale$Mean_temperature~.,data=wankara_scale)
summary(fit_mlm1)

# Elimino Precipitation por tener un p-valor de 0.885
fit_mlm2=lm(wankara_scale$Mean_temperature~.-Precipitation,data=wankara_scale)
summary(fit_mlm2)

# Elimino Sea_level_pressure por tener el mayor error standard
fit_mlm3=lm(wankara_scale$Mean_temperature~.-Precipitation-Sea_level_pressure,data=wankara_scale)
summary(fit_mlm3)

# Elimino Max wind speed por tener el mayor error standard
fit_mlm4 = lm(wankara_scale$Mean_temperature~.-Precipitation-Sea_level_pressure-Max_wind_speed,data=wankara_scale)
summary(fit_mlm4)

# Elimino visibility
fit_mlm5 = lm(wankara_scale$Mean_temperature~.-Precipitation-Sea_level_pressure-Max_wind_speed-Visibility,data=wankara_scale)
summary(fit_mlm5)

# Elimino standard pressure
fit_mlm6 = lm(wankara_scale$Mean_temperature~.-Standard_pressure-Precipitation-Sea_level_pressure-Max_wind_speed-Visibility,data=wankara_scale)
summary(fit_mlm6)

# Elimino Wind speed --> Modelo más interpretable
fit_mlm7 = lm(wankara_scale$Mean_temperature~.-Wind_speed-Standard_pressure-Precipitation-Sea_level_pressure-Max_wind_speed-Visibility,data=wankara_scale)
summary(fit_mlm7)

# INTERACCIONES

# Interacción entre la presión a nivel del mar y la estándar. Mejor resultado hasta ahora: 0.9899
fit_i1=lm(wankara_scale$Mean_temperature~.-Precipitation+Sea_level_pressure*Standard_pressure,data=wankara_scale)
summary(fit_i1)

# Interacción entre la temperatura mínima y dewpoint
fit_i2=lm(wankara_scale$Mean_temperature~.-Precipitation+Min_temperature*Dewpoint,data=wankara_scale)
summary(fit_i2)

# Mejor resultado hasta el momento 0.99
fit_i3 = lm(wankara_scale$Mean_temperature~.-Precipitation+Min_temperature*Dewpoint+I(Dewpoint^2)-Dewpoint,data=wankara_scale)
summary(fit_i3)

# Eliminamos Visibility por su alto p-value
fit_i4 = lm(wankara_scale$Mean_temperature~.-Precipitation+Min_temperature*Dewpoint+I(Dewpoint^2)-Dewpoint-Visibility,data=wankara_scale)
summary(fit_i4)

# Mejor resultado hasta el momento, 0.9916
fit_i5 = lm(wankara_scale$Mean_temperature~.-Precipitation+I(Max_temperature^2)+Min_temperature*Dewpoint+I(Dewpoint^2)-Dewpoint-Visibility,data=wankara_scale)
summary(fit_i5)

# Mejor resultado --> 0.992
fit_i6 = lm(wankara_scale$Mean_temperature~.-Precipitation+I(Min_temperature^2)+I(Max_temperature^2)+Min_temperature*Dewpoint+I(Dewpoint^2)-Dewpoint-Visibility,data=wankara_scale)
summary(fit_i6)

# 0.9921
fit_i7 = lm(wankara_scale$Mean_temperature~.-Precipitation+Max_wind_speed*Wind_speed+I(Min_temperature^2)+I(Max_temperature^2)+Min_temperature*Dewpoint+I(Dewpoint^2)-Dewpoint-Visibility-Max_wind_speed,data=wankara_scale)
summary(fit_i7)

# Cálculo de RMSE
yprime_i7 = predict(fit_i7,wankara_scale)
sqrt(sum(abs(wankara_scale$Mean_temperature-yprime_i7)^2)/length(yprime_i7))

# CV del model más satisfactorio
nombre <- "./data/wankara/wankara"
run_i7_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@", header=FALSE)
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@", header=FALSE)
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") {
    test <- x_tra
  }
  else {
    test <- x_tst
  }
  fit_i7 = lm(Y~.-X4+X9*X8+I(X2^2)+I(X1^2)+X2*X3+I(X3^2)-X3-X7-X9,data=test)
  yprime=predict(fit_i7,test)
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
resultados_i7_train = sapply(1:5,run_i7_fold,nombre,"train")
resultados_i7_test = sapply(1:5,run_i7_fold,nombre,"test")
i7MSEtrain<-mean(resultados_i7_train)
i7MSEtest<-mean(resultados_i7_test)

