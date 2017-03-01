#Función objetivo tal que se minimiza la distancia entre X^3 y la matriz de transiciones a tres años 
#X está definido por columnas
library(msm)
setwd("/Users/Dalia/Documents/INSP/Embarazo/Bases")
Embarazo <- read.csv("Embarazo.estados.csv")
#Tabla de Transición
Tabla.Transicion  <- statetable.msm(estado, subject = el_folio, data = Embarazo)
Tabla.Transicion2 <- statetable.msm(estado, subject = el_folio, data = Embarazo[Embarazo$edad>= 14 & Embarazo$edad<=25,])
#Matriz de transición al año 3
Mat.transicion.3 <- Tabla.Transicion/rowSums(Tabla.Transicion)
Mat.transicion.3.2 <- Tabla.Transicion2/rowSums(Tabla.Transicion2)

objfun_X3    <- function(X) {
  X <- matrix(X,ncol=12)
  M <- (X%*%X%*%X-Mat.transicion.3.2)^2
  sum(M)
}