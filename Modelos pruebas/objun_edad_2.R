#Se tiene la proporción de adolescentes en cada categoría por edad, se busca minimizar Xp_i-p_{i+1},
#donde p_i es el vector de proporciones para la edad i y P es la matriz de transición,
#para i=14
#Ver la distribución de estados por edad
setwd("/Users/Dalia/Documents/INSP/Embarazo/Bases")
Embarazo <- read.csv("Embarazo.estados.csv")

Tabla.Edad.year <- table(Embarazo$edad,Embarazo$estado, Embarazo$year)
Tabla.Edad.2002 <- Tabla.Edad.year[1:6,,1]
Tabla.Edad.2002 <- Tabla.Edad.2002/rowSums(Tabla.Edad.2002)
Tabla.Edad.2005 <- Tabla.Edad.year[1:6,,2]
Tabla.Edad.2005 <- Tabla.Edad.2005/rowSums(Tabla.Edad.2005)
Tabla.Edad.2009 <- Tabla.Edad.year[1:6,,3]
Tabla.Edad.2009 <- Tabla.Edad.2009/rowSums(Tabla.Edad.2009)

objfun     <- function(X) {
  X <- matrix(X,ncol=9, byrow = TRUE)
  S <- 0
  i <- 4
  S <- S + sum((X%*%Tabla.Edad.2002[i,]-Tabla.Edad.2002[i+1,])^2)
  S <- S + sum((X%*%Tabla.Edad.2005[i,]-Tabla.Edad.2005[i+1,])^2)
  S <- S + sum((X%*%Tabla.Edad.2009[i,]-Tabla.Edad.2009[i+1,])^2)
  
  S
}