###########################################################
# INTRODUCCIÓN A LA CIENCIA DE DATOS
# Autor: Luis Balderas Ruiz
# EDA+Clasificación
# Dataset: vowel
############################################################

# Cálculo de binwidth para un histograma
binwd = function(data){
  size = length(data)
  dt = sd(data)
  cr = size^(1/3)
  return(1/(cr)*dt*3.49)
}

# Lectura del fichero de datos para proceder al EDA
vowel = read.csv("./data/vowel/vowel.dat",header=FALSE, comment.char="@")
colnames(vowel) = c("TT","SpeakerNumber","Sex","F0","F1","F2","F3","F4","F5","F6","F7","F8","F9","Class")

# Estudiemos la estructura del conjunto
str(vowel)

# Borramos la variable TT porque no considero necesario dividir entre training y test para el EDA
vowel$TT = NULL

# Convierto SpeakerNumber y Sex a factores
vowel$Sex = factor(vowel$Sex, levels = c(0,1), labels = c("Masculino","Femenino"))
vowel$SpeakerNumber = factor(vowel$SpeakerNumber)

# Resumen estadístico de cada variable
summary(vowel)
sapply(vowel[,3:12],sd)

# Búsqueda visual de correlaciones entre las variables en el dataset completo m
plot(vowel[,3:12])

# Separación por sexos
library(tidyverse)
hombres = vowel %>% filter(vowel$Sex == "Masculino")
mujeres = vowel %>% filter(vowel$Sex == "Femenino")

# Búsqueda visual de correlaciones por sexos
plot(hombres[,3:12])
plot(mujeres[,3:12])

#################################################################333
# Distribución de las variables numéricas. Skewness
library(ggplot2)
library(e1071)
dist_sexo = ggplot(vowel,aes(x=Sex, fill=SpeakerNumber)) + geom_bar(alpha=1/3) +theme(legend.position = "top") + labs(title="Distribución por sexos")
dist_sexo

distf0 = ggplot(vowel,aes(x=F0, fill=Sex)) + geom_histogram(binwidth = binwd(vowel$F0)) + theme(legend.position = "right") + labs(title="Distribución de la variable F0")
distf0
# positiva
skewness(vowel$F0)

distf1 = ggplot(vowel,aes(x=F1, fill=Sex)) + geom_histogram(binwidth = binwd(vowel$F1)) + theme(legend.position = "right") + labs(title="Distribución de la variable F1")
distf1
#negativa
skewness(vowel$F1)

distf2 = ggplot(vowel,aes(x=F2, fill=Sex)) + geom_histogram(binwidth = binwd(vowel$F2)) + theme(legend.position = "right") + labs(title="Distribución de la variable F2")
distf2
# positiva
skewness(vowel$F2)

distf3 = ggplot(vowel,aes(x=F3, fill=Sex)) + geom_histogram(binwidth = binwd(vowel$F3)) + theme(legend.position = "right") + labs(title="Distribución de la variable F3")
distf3
# positiva
skewness(vowel$F3)

distf4 = ggplot(vowel,aes(x=F4, fill=Sex)) + geom_histogram(binwidth = binwd(vowel$F4)) + theme(legend.position = "right") + labs(title="Distribución de la variable F4")
distf4
# positiva
skewness(vowel$F4)

distf5 = ggplot(vowel,aes(x=F5, fill=Sex)) + geom_histogram(binwidth = binwd(vowel$F5)) + theme(legend.position = "right") + labs(title="Distribución de la variable F5")
distf5
# positiva
skewness(vowel$F5)

distf6 = ggplot(vowel,aes(x=F6, fill=Sex)) + geom_histogram(binwidth = binwd(vowel$F6)) + theme(legend.position = "right") + labs(title="Distribución de la variable F6")
distf6
# negativa
skewness(vowel$F6)

distf7 = ggplot(vowel,aes(x=F7, fill=Sex)) + geom_histogram(binwidth = binwd(vowel$F7)) + theme(legend.position = "right") + labs(title="Distribución de la variable F7")
distf7
# positiva
skewness(vowel$F7)

distf8 = ggplot(vowel,aes(x=F8, fill=Sex)) + geom_histogram(binwidth = binwd(vowel$F8)) + theme(legend.position = "right") + labs(title="Distribución de la variable F8")
distf8
# positiva
skewness(vowel$F8)

distf9 = ggplot(vowel,aes(x=F9, fill=Sex)) + geom_histogram(binwidth = binwd(vowel$F9)) + theme(legend.position = "right") + labs(title="Distribución de la variable F9")
distf9
# positiva
skewness(vowel$F9)
##############################################################################################

##############################################################################################
# BOXPLOTS

bps0 = ggplot(vowel, aes(x=Sex, y=F0, fill = SpeakerNumber)) + geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)
bps0
bpsn0 = ggplot(vowel, aes(x = SpeakerNumber, y = F0, fill = Sex))+ geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)
bpsn0

bps1 = ggplot(vowel, aes(x=Sex, y=F1, fill = SpeakerNumber)) + geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)
bps1
bpsn1 = ggplot(vowel, aes(x = SpeakerNumber, y = F1, fill = Sex))+ geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)
bpsn1

bps2 = ggplot(vowel, aes(x=Sex, y=F2, fill = SpeakerNumber)) + geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)
bps2
bpsn2 = ggplot(vowel, aes(x = SpeakerNumber, y = F2, fill = Sex))+ geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)
bpsn2

bps3 = ggplot(vowel, aes(x=Sex, y=F3, fill = SpeakerNumber)) + geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)
bps3
bpsn3 = ggplot(vowel, aes(x = SpeakerNumber, y = F3, fill = Sex))+ geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)
bpsn3

bps4 = ggplot(vowel, aes(x=Sex, y=F4, fill = SpeakerNumber)) + geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)
bps4
bpsn4 = ggplot(vowel, aes(x = SpeakerNumber, y = F4, fill = Sex))+ geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)
bpsn4

bps5 = ggplot(vowel, aes(x=Sex, y=F5, fill = SpeakerNumber)) + geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)
bps5
bpsn5 = ggplot(vowel, aes(x = SpeakerNumber, y = F5, fill = Sex))+ geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)
bpsn5

bps6 = ggplot(vowel, aes(x=Sex, y=F6, fill = SpeakerNumber)) + geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)
bps6
bpsn6 = ggplot(vowel, aes(x = SpeakerNumber, y = F6, fill = Sex))+ geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)
bpsn6

bps7 = ggplot(vowel, aes(x=Sex, y=F7, fill = SpeakerNumber)) + geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)
bps7
bpsn7 = ggplot(vowel, aes(x = SpeakerNumber, y = F7, fill = Sex))+ geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)
bpsn7

bps8 = ggplot(vowel, aes(x=Sex, y=F8, fill = SpeakerNumber)) + geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)
bps8
bpsn8= ggplot(vowel, aes(x = SpeakerNumber, y = F8, fill = Sex))+ geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)
bpsn8

bps9 = ggplot(vowel, aes(x=Sex, y=F9, fill = SpeakerNumber)) + geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)
bps9
bpsn9 = ggplot(vowel, aes(x = SpeakerNumber, y = F9, fill = Sex))+ geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)
bpsn9

##############################################################################################

################################################################333
# Medida de la "normalidad" de las variables numéricas
# Test de Shapiro-Wilks vs Kolmogorov-Smirnov (Corrección de Lillie)
library(nortest)
library(car)
# Variable F0. 
# Shapiro: El bajo p-valor (9.807e-5) nos hace rechazar la hipótesis de normalidad
shapiro.test(vowel$F0)
# Lillie p-valor 0.000719 Rechazamos la normalidad
lillie.test(vowel$F0)

# Variable F1.
# Shapiro:pvalue (0.0312) rechaza normalidad
shapiro.test(vowel$F1)
# Lillie: pvalue 0.2628. No podemos rechazar la hipótesis de normalidad.
lillie.test(vowel$F1)
# Comprobamos que es bastante próxima a la normal con un qqPlot
qqPlot(vowel$F1)
ggplot(vowel, aes(x=F1)) + geom_histogram(aes(y=..density..),binwidth = binwd(vowel$F1)) + stat_function(fun=dnorm, args=list(mean=mean(vowel$F1),sd=sd(vowel$F1)))

# Variable F2. 
# Shapiro:El bajo p-valor (4.245e-5) nos hace rechazar la hipótesis de normalidad
shapiro.test(vowel$F2)
#Lillie: p-value = 0.001264 rechazamos la normalidad
lillie.test(vowel$F2)

# Variable F3. 
# Shapiro:El bajo p-valor (4.324e-9) nos hace rechazar la hipótesis de normalidad
shapiro.test(vowel$F3)
qqPlot(vowel$F3)
# Lillie: p-value = 5.169e-07. Rechazamos la normalidad
lillie.test(vowel$F3)

# Variable F4.
# Shapiro: p-valor (0.07073) mayor que 0.05, por lo que no podemos rechazar la hipótesis de normalidad
shapiro.test(vowel$F4)
qqPlot(vowel$F4)
# Lillie: p-value = 0.08258. No podemos rechazar la hipótesis de normalidad
lillie.test(vowel$F4)

# Variable F5. 
#Shapiro: El bajo p-valor (5.435e-08) nos hace rechazar la hipótesis de normalidad
shapiro.test(vowel$F5)
# Lillie:p-value = 1.921e-08. Rechazamos la hipótesis de normalidad
lillie.test(vowel$F5)

# Variable F6. 
# Shapiro:pvalue (0.0225)<0.05, rechazamos hipótesis de normalidad
shapiro.test(vowel$F6)
# Lillie: pvalue  0.09071, no podemos rechazar la hipótesis de normalidad
lillie.test(vowel$F6)
qqPlot(vowel$F6)


# Variable F7. 
# Shapiro: pvalue (0.0005086)<0.05, rechazamos hipótesis de normalidad
shapiro.test(vowel$F7)
# Lillie:p-value = 0.0002122. Rechazamos la hipótesis de normalidad
lillie.test(vowel$F7)

# Variable F8. 
# Shapiro:pvalue (0.001437)<0.05, rechazamos hipótesis de normalidad
shapiro.test(vowel$F8)
# Lillie: p-value = 0.1505. No podemos rechazar la hipótesis de normalidad
lillie.test(vowel$F8)
qqPlot(vowel$F8)

# Variable F9. 
# Shapiro: pvalue (2.208e-12)<0.05, rechazamos hipótesis de normalidad
shapiro.test(vowel$F9)
# Lillie: p-value = 7.729e-14. Rechazamos la hipótesis de normalidad
lillie.test(vowel$F9)

## Por sexos

# Hombres

# pvalue (6.965e-05) < 0.05. Rechazo hipótesis de normalidad
shapiro.test(hombres$F0)
# pvalue (0.0002435) < 0.05. Rechazo hipótesis de normalidad
shapiro.test(hombres$F1)
# pvalue (0.0003589) < 0.05. Rechazo hipótesis de normalidad
shapiro.test(hombres$F2)
# pvalue (1.919e-07) < 0.05. Rechazo hipótesis de normalidad
shapiro.test(hombres$F3)
# pvalue (0.07804) > 0.05. Acepto hipótesis de normalidad
shapiro.test(hombres$F4)
# pvalue (7.718e-09) < 0.05. Rechazo hipótesis de normalidad
shapiro.test(hombres$F5)
# pvalue (3.306e-07) < 0.05. Rechazo hipótesis de normalidad
shapiro.test(hombres$F6)
# pvalue (0.009972) < 0.05. Rechazo hipótesis de normalidad
shapiro.test(hombres$F7)
# pvalue (0.00119) < 0.05. Rechazo hipótesis de normalidad
shapiro.test(hombres$F8)
# pvalue (1.466e-15) < 0.05. Rechazo hipótesis de normalidad
shapiro.test(hombres$F9)

# Mujeres
# pvalue (0.005136) < 0.05. Rechazo hipótesis de normalidad
shapiro.test(mujeres$F0)
# pvalue (0.02526) < 0.05. Rechazo hipótesis de normalidad
shapiro.test(mujeres$F1)
# pvalue (1.284e-05) < 0.05. Rechazo hipótesis de normalidad
shapiro.test(mujeres$F2)
# pvalue (3.099e-07) < 0.05. Rechazo hipótesis de normalidad
shapiro.test(mujeres$F3)
# pvalue (0.1163) > 0.05. Acepto hipótesis de normalidad
shapiro.test(mujeres$F4)
qqPlot(mujeres$F4)
# pvalue (0.04365) < 0.05. Rechazo hipótesis de normalidad
shapiro.test(mujeres$F5)
# pvalue (0.09697) > 0.05. Acepto hipótesis de normalidad
shapiro.test(mujeres$F6)
# pvalue (8.115e-07) < 0.05. Rechazo hipótesis de normalidad
shapiro.test(mujeres$F7)
# pvalue (0.003301) < 0.05. Rechazo hipótesis de normalidad
shapiro.test(mujeres$F8)
# pvalue (9.116e-08) < 0.05. Rechazo hipótesis de normalidad
shapiro.test(mujeres$F9)
qqPlot(mujeres$F9)
##################################################################

##################################################################
# ANOVA. Kruskal–Wallis Test

##################################################################