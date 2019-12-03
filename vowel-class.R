###########################################################
# INTRODUCCIÓN A LA CIENCIA DE DATOS
# Autor: Luis Balderas Ruiz
# EDA+Clasificación
# Dataset: vowel
############################################################


# Lectura del fichero de datos para proceder al EDA
vowel = read.csv("./data/vowel/vowel.dat",header=FALSE, comment.char="@")
colnames(vowel) = c("TT","SpeakerNumber","Sex","F0","F1","F2","F3","F4","F5","F6","F7","F8","F9","Class")

# Estudiemos la estructura del conjunto
str(vowel)

# Borramos la variable TT porque no considero necesario dividir entre training y test para el EDA
vowel$TT = NULL

# Convierto SpeakerNumber y Sex a factores
vowel$Sex = factor(vowel$Sex, levels = c(0,1), labels = c("Masculino","Feminino"))
vowel$SpeakerNumber = factor(vowel$SpeakerNumber)

