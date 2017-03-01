#Función objetivo tal que se minimiza la distancia entre X y la matriz de transición a tres años 
#X se define por columnas
library(msm)
setwd("/Users/Dalia/Documents/INSP/Embarazo/Bases")
Embarazo <- read.csv("Embarazo.estados.csv")
#Tabla de Transición
Tabla.Transicion <- statetable.msm(estado, subject = el_folio, data = Embarazo)

#Matriz de transición al año 3
Mat.transicion.3 <- Tabla.Transicion/rowSums(Tabla.Transicion)

objfun_X     <- function(X) {
  X <- matrix(X,ncol=12)
  M <- (X-Mat.transicion.3)^2
  sum(M)
}