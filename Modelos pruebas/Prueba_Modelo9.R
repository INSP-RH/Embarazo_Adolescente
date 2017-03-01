#Modelo Muy simple de Embarazo
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

#Pruebas usando el paquete msm
library(msm)
library(minqa)
library(rootSolve)

#Leer Embarazo
setwd("/Users/Dalia/Documents/INSP/Embarazo/Bases")
Embarazo <- read.csv("Embarazo_modelo9.csv")

#Tabla de transiciones 
Tabla.Transicion <- statetable.msm(modelo9,  el_folio, data = Embarazo)

#Matriz de trancisión en tres años
Mat.Transicion.3t <- Tabla.Transicion/rowSums(Tabla.Transicion)

f2<-function(x) {
  X <- matrix(nr = 9, x)
  X %*% X %*% X - Mat.Transicion.3t
}
Root           <- multiroot(f2, start = rep(0,length=81), positive = TRUE)
Mat.Transicion <- matrix(nrow = 9, Root$root)
Mat.Transicion <- Mat.Transicion/rowSums(Mat.Transicion)
round(Mat.Transicion,2)
norm(f2(Mat.Transicion))

Qrand        <- matrix(runif(81),nrow = 9)

Q.sugerida   <- crudeinits.msm(modelo9 ~ edad, subject = el_folio,
                               data = Embarazo, qmatrix = Qrand)

Modelo2      <- msm(modelo9 ~ edad, subject = el_folio,
                    data = Embarazo, qmatrix = Q.sugerida, opt.method = "nlm")

P.modelo2 <- pmatrix.msm(Modelo2)

