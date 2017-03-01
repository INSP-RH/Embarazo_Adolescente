#Modelo Muy simple de Embarazo
#Valor     #Estado
# 1         No sexualmente activa
# 2         Sexualmente activa sin AC
# 3         Sexualmente activa con AC
# 4         Sexualmente inactiva
# 5         Embarazada

#Pruebas usando el paquete msm
library(msm)
library(minqa)
library(rootSolve)
#Leer Embarazo
setwd("/Users/Dalia/Documents/INSP/Embarazo/Bases")
Embarazo <- read.csv("Embarazo.csv")
#Dividir a las adolescentes en los estados correspondientes
Embarazo$Estado.simple <- NA
Embarazo$Estado.simple[Embarazo$ivsa==0] <- 1 #No sexualmente activa

Embarazo$Estado.simple[Embarazo$ivsa==1 & Embarazo$ac11==3]   <- 2 #Sexualmente Activa sin anticonceptivos
Embarazo$Estado.simple[Embarazo$ivsa==1 & Embarazo$ac11==1]   <- 3 #Sexualmente Activa con anticonceptivos
Embarazo$Estado.simple[Embarazo$ivsa==1 & Embarazo$ac37_1==4] <- 4 #Sexualmente inactiva

Embarazo$Estado.simple[Embarazo$he01_1c==1]  <- 5 #Embarazada
Embarazo$Estado.simple[Embarazo$he01a_1c==1] <- 5 #Embarazada
Embarazo$Estado.simple[Embarazo$he01c==1]    <- 5 #Embarazada

#Tabla de transiciones 
Tabla.Transicion <- statetable.msm(Estado.simple,  el_folio, data = Embarazo)

#Matriz de trancisión en tres años
Mat.Transicion.3t <- Tabla.Transicion/rowSums(Tabla.Transicion)

#Encontrar raíz cúbica de la matriz de transición a tres años
#Una solución redondeada a dos cifras significativas es aproximadamente: (Ojo: no es matriz de transición)
Sol <- matrix(c(0.86, 0.03,0.04,0.04,0.02,
                0,0.27,0.29,0.15,0.28,
                0,0.13,0.81,0.03,0.03, 
                0, 0.29,0.17, 0.55,0,
                0,0.44,0.46,0,0.1),byrow = TRUE, ncol = 5)


f2<-function(x) {
  X <- matrix(nr = 5, x)
  X %*% X %*% X - Mat.Transicion.3t
}
Root           <- multiroot(f2, start = runif(25), positive = TRUE)
Mat.Transicion <- matrix(nrow = 5, Root$root)
Mat.Transicion <- Mat.Transicion/rowSums(Mat.Transicion)
round(Mat.Transicion,3)
norm(f2(Mat.Transicion))

#Modelo utilizando msm
#Matriz Q inicial para modelo muy simple

Q.muy.simple <- matrix(c( c(-0.25,.15,.1,0,0), c(0,-1,0.1,0.8,0.1),
                          c(0,0.2,-0.45,0.05,0.2), c(0,0.2,0.2,-0.4,0),
                          c(0,0.2,0.2,0.2,-0.8)), ncol = 5, byrow = TRUE)

Q.sugerida   <- crudeinits.msm(Estado.simple ~ year, subject = el_folio,
                               data = Embarazo, qmatrix = Q.muy.simple)


Modelo1      <- msm(Estado.simple ~ year, subject = el_folio,
                    data = Embarazo, qmatrix = Q.sugerida, opt.method = "nlm")
P.modelo1    <- pmatrix.msm(Modelo1)
sojourn.msm(Modelo1)

#Modelo con edad como tiempo

Q.sugerida   <- crudeinits.msm(Estado.simple ~ edad, subject = el_folio,
                               data = Embarazo, qmatrix = Q.muy.simple)

Modelo2      <- msm(Estado.simple ~ edad, subject = el_folio,
                    data = Embarazo, qmatrix = Q.sugerida, opt.method = "nlm")

P.modelo2 <- pmatrix.msm(Modelo2)

