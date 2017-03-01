#Función objetivo tal que se minimiza la distancia entre X^3 y la matriz de transiciones a tres años

#Tabla de Transición
Tabla.Transicion <- statetable.msm(modelo9, subject = el_folio, data = Embarazo)

#Matriz de transición al año 3
Mat.transicion.3 <- Tabla.Transicion/rowSums(Tabla.Transicion)

objfun     <- function(X) {
  X <- matrix(X,ncol=9, byrow = TRUE)
  M <- (X%*%X%*%X-Mat.transicion.3)^2
  sum(M)
}