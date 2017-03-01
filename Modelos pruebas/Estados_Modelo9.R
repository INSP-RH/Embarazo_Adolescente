#Se propone un modelo intermedio entre el que tiene 4 estados y el que tiene 15. Tiene un total de nueve estados.

#Valor     #Estado
# 1         No sexualmente activa
# 2         Sexualmente activa con Anticonceptivos
# 3         Sexualmente activa con Naturales
# 4         Sexualmente activa sin AC poco modificable
# 5         Sexualmente activa sin AC modificable
# 6         Sexualmente activa sin AC otras razones
# 7         Sexualmente inactiva
# 8         Probabilidad Baja de Embarazo
# 9         Embarazada

setwd("/Users/Dalia/Documents/INSP/Embarazo/Bases")
Embarazo_estados <- read.csv("Embarazo.estados.csv")

Embarazo_estados$modelo9 <- NA

Embarazo_estados$modelo9[Embarazo_estados$estado == 1]  <- 1 #No sexualmente activa
Embarazo_estados$modelo9[Embarazo_estados$estado == 2]  <- 2 #Sexualmente activa con Pastilla -> Con AC
Embarazo_estados$modelo9[Embarazo_estados$estado == 3]  <- 2 #Sexualmente activa con Pastilla de Emergencia -> Con AC
Embarazo_estados$modelo9[Embarazo_estados$estado == 4]  <- 2 #Sexualmente activa con Inyecciones -> Con AC
Embarazo_estados$modelo9[Embarazo_estados$estado == 5]  <- 2 #Sexualmente activa con Condón -> Con AC
Embarazo_estados$modelo9[Embarazo_estados$estado == 6]  <- 2 #Sexualmente activa con Implante -> Con AC
Embarazo_estados$modelo9[Embarazo_estados$estado == 7]  <- 2 #Sexualmente activa con DIU -> Con AC
Embarazo_estados$modelo9[Embarazo_estados$estado == 8]  <- 3 #Sexualmente activa con Naturales
Embarazo_estados$modelo9[Embarazo_estados$estado == 9]  <- 2 #Sexualmente activa con Otros anticonceptivos -> Con AC
Embarazo_estados$modelo9[Embarazo_estados$estado == 10] <- 4 #Sexualmente activa sin AC poco modificable
Embarazo_estados$modelo9[Embarazo_estados$estado == 11] <- 5 #Sexualmente activa sin AC modificable
Embarazo_estados$modelo9[Embarazo_estados$estado == 12] <- 6 #Sexualmente activa sin AC otras razones
Embarazo_estados$modelo9[Embarazo_estados$estado == 13] <- 7 #Sexualmente inactiva
Embarazo_estados$modelo9[Embarazo_estados$estado == 14] <- 8 #Probabilidad Baja de Embarazo
Embarazo_estados$modelo9[Embarazo_estados$estado == 15] <- 9 #Embarazada

write.csv(Embarazo_estados, "Embarazo_modelo9.csv")
