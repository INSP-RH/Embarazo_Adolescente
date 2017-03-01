#Probabilidad inicial del modelo de embarazo
#Modelo 
#Valor     #Estado
# 1         No sexualmente activa
# 2         Sexualmente activa con Pastilla/Parche
# 3         Sexualmente activa con LARCS (DIU, Implante, Inyección)
# 4         Sexualmente activa con Condón
# 5         Sexualmente activa con Naturales
# 6         Sexualmente activa con Otros AC/ con AC no dijo cuál
# 7         Sexualmente activa sin AC no modificable
# 8         Sexualmente activa sin AC poco modificable
# 9         Sexualmente activa sin AC modificable
# 10        Sexualmente inactiva
# 11        Probabilidad Baja de Embarazo
# 12        Embarazada
# 13        Muerte- se inicia con cero adolescentes muertas

setwd("/Users/Dalia/Documents/INSP/Embarazo/Bases")
Embarazo <- read.csv("Embarazo.estados.csv")

Tabla.Edad.year <- table(Embarazo$edad,Embarazo$estado, Embarazo$year)
Tabla.Edad.2002 <- Tabla.Edad.year[1:6,,1]
Tabla.Edad.2002 <- Tabla.Edad.2002/rowSums(Tabla.Edad.2002)
Tabla.Edad.2005 <- Tabla.Edad.year[1:6,,2]
Tabla.Edad.2005 <- Tabla.Edad.2005/rowSums(Tabla.Edad.2005)
Tabla.Edad.2009 <- Tabla.Edad.year[1:6,,3]
Tabla.Edad.2009 <- Tabla.Edad.2009/rowSums(Tabla.Edad.2009)

probas <- rep(0, 13)
for(i in 1:12){
  probas[i] <- mean(c(Tabla.Edad.2002[1,i],Tabla.Edad.2005[1,i],Tabla.Edad.2009[1,i]))
}
