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

#Proponer una matriz inicial Q
Q.inicial    <- read.csv("Q_inicial.csv", stringsAsFactors = FALSE)
Q.inicial    <- Q.inicial[,2:16]
Q.inicial    <-as.numeric( as.matrix(Q.inicial))
Q.inicial    <- matrix(Q.inicial, ncol=15)
#Buscar matriz de transición
Q.sugerida   <- crudeinits.msm(estado ~ edad, subject = el_folio, data = Embarazo_estados, qmatrix = Q.inicial)

ptm <- proc.time()
Modelo2      <- msm(estado ~ edad, subject = el_folio,
                    data = Embarazo_estados, qmatrix = Q.sugerida, opt.method = "nlm")
ptm <- proc.time()-ptm
P.modelo1    <- pmatrix.msm(Modelo1)
write.csv(P.modelo1, "Matriz_Modelo2.csv")
sojourn.msm(Modelo1)
