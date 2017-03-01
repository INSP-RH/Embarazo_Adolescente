#Probabilidad inicial del modelo de embarazo
Tabla.Edad.year <- table(Embarazo$edad,Embarazo$modelo9, Embarazo$year)
Tabla.Edad.2002 <- Tabla.Edad.year[1:6,,1]
Tabla.Edad.2002 <- Tabla.Edad.2002/rowSums(Tabla.Edad.2002)
Tabla.Edad.2005 <- Tabla.Edad.year[1:6,,2]
Tabla.Edad.2005 <- Tabla.Edad.2005/rowSums(Tabla.Edad.2005)
Tabla.Edad.2009 <- Tabla.Edad.year[1:6,,3]
Tabla.Edad.2009 <- Tabla.Edad.2009/rowSums(Tabla.Edad.2009)

probas <- rep(NA, 9)
for(i in 1:9){
  probas[i] <- mean(c(Tabla.Edad.2002[1,i],Tabla.Edad.2005[1,i],Tabla.Edad.2009[1,i]))
}