###########################################################
# INTRODUCCIÓN A LA CIENCIA DE DATOS
# Autor: Luis Balderas Ruiz
# EDA+Regresion
# Dataset: wankara
############################################################

wankara = read.csv("./data/wankara/wankara.dat",header=FALSE, comment.char = "@")
colnames(wankara) = c("Max_temperature", "Min_temperature", "Dewpoint", "Precipitation", "Sea_level_pressure", "Standard_pressure", "Visibility", "Wind_speed", "Max_wind_speed","Mean_temperature")
