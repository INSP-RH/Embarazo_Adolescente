#Función objetivo tal que se minmiza P_iX-P_{i+1} 
#Gradiente para la función objetivo
#Recibe X en forma vectorial donde se define una matriz nxn en el orden de las columnas

setwd("/Users/Dalia/Documents/INSP/Embarazo/Bases")
Embarazo <- read.csv("Embarazo.estados.csv")

Tabla.Edad.year <- table(Embarazo$edad,Embarazo$estado, Embarazo$year)
Tabla.Edad.2002 <- Tabla.Edad.year[1:6,,1]
Tabla.Edad.2002 <- Tabla.Edad.2002/rowSums(Tabla.Edad.2002)
Tabla.Edad.2005 <- Tabla.Edad.year[1:6,,2]
Tabla.Edad.2005 <- Tabla.Edad.2005/rowSums(Tabla.Edad.2005)
Tabla.Edad.2009 <- Tabla.Edad.year[1:6,,3]
Tabla.Edad.2009 <- Tabla.Edad.2009/rowSums(Tabla.Edad.2009)

P0 <- Tabla.Edad.2002[1,]
P1 <- Tabla.Edad.2002[2,]
objfun_pX <- function(X){
  n   <- sqrt(length(X))
  Sum <- 0
  for(i in 1:n){
    s <- 0
    for(j in 1:n){
      s <- s + P0[j]*X[n*(i-1)+j]
    }
    Sum <- Sum + (s-P1[i])^2
  }
  return(Sum)
}

grad_objfun_pX <- function(X){
  n   <- sqrt(length(X))
  gradiente <- c()
  for(i in 1:n){
    s <- 0
    for(j in 1:n){
      s <- s + P0[j]*X[n*(i-1)+j]
    }
    s    <- 2*(s-P1[i])
    gradiente <- c(gradiente, s*P0)
  }
  return(gradiente)
}
