#Prueba de matrices para un primer modelo que divide anticonceptivos y razones
#Modelo
#Valor     #Estado
# 1         No sexualmente activa
# 2         Sexualmente activa con Pastilla
# 3         Sexualmente activa con Pastilla de Emergencia
# 4         Sexualmente activa con Inyecciones
# 5         Sexualmente activa con Condón
# 6         Sexualmente activa con Implante
# 7         Sexualmente activa con DIU
# 8         Sexualmente activa con Naturales
# 9         Sexualmente activa con Otros anticonceptivos 
# 10        Sexualmente activa sin AC poco modificable
# 11        Sexualmente activa sin AC modificable
# 12        Sexualmente activa sin AC otras razones
# 13        Sexualmente inactiva
# 14        Probabilidad Baja de Embarazo
# 15        Embarazada

setwd("/Users/Dalia/Documents/INSP/Embarazo/Bases")
Embarazo_estados <- read.csv("Embarazo.estados.csv")

library(msm)
library(rootSolve)

#Tabla de Transición
Tabla.Transicion <- statetable.msm(estado, subject = el_folio, data = Embarazo_estados)

#No hay información disponible de las transiciones de usar pastilla de emergencia. 
#Por el momento se utilizará como renglón estimado la cantidad que ha usado pastilla de emergencia en algún momento 
#y ahora en dónde se encuentran
R3 <- table(Embarazo_estados$ac03h, Embarazo_estados$estado)[1,]
Tabla.Transicion2 <- rbind(Tabla.Transicion[1:2,],R3, Tabla.Transicion[3:14,])

#Matriz de transición al año 3
Mat.transicion.3 <- Tabla.Transicion2/rowSums(Tabla.Transicion2)


#La raíz cúbica que se obtiene depende mucho de las condiciones iniciales
#Encontrar raíz cúbica para  tener matriz de transición a un año
f2<-function(x) {
  X <- matrix(nr = 15, x)
  X %*% X %*% X - Mat.transicion.3
}
Root           <- multiroot(f2, start = runif(15^2), positive = TRUE)
Mat.Transicion <- matrix(nrow = 15, Root$root)
Mat.Transicion <- Mat.Transicion/rowSums(Mat.Transicion)
round(Mat.Transicion,2)
norm(f2(Mat.Transicion))

#Proponer una matriz inicial Q
Q.inicial    <- matrix(rep(0.01, length=15^2), ncol = 15)
#Buscar matriz de transición
Q.sugerida   <- crudeinits.msm(estado ~ edad, subject = el_folio, data = Embarazo_estados, qmatrix = Q.inicial) 




ptm <- proc.time()
Modelo1      <- msm(estado ~ edad, subject = el_folio,
                    data = Embarazo_estados, qmatrix = Q.sugerida, opt.method = "nlm")
ptm <- proc.time()-ptm
P.modelo1    <- pmatrix.msm(Modelo1)
write.csv(P.modelo1, "Matriz_Modelo1.csv")
sojourn.msm(Modelo1)
