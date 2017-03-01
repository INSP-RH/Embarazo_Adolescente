#Función objetivo tal que se minimiza la distancia entre X^3 y la matriz de transiciones a tres años
library(msm)
setwd("/Users/Dalia/Documents/INSP/Embarazo/Bases")
Embarazo <- read.csv("Embarazo.estados.csv")
#Tabla de Transición
Tabla.Transicion <- statetable.msm(estado, subject = el_folio, data = Embarazo)

#Matriz de transición al año 3
Mat.transicion.3 <- Tabla.Transicion/rowSums(Tabla.Transicion)

objfun_X3  <- function(X) {
  X <- matrix(X,ncol=12, byrow = TRUE)
  M <- (X%*%X%*%X-Mat.transicion.3)^2
  sum(M)
}