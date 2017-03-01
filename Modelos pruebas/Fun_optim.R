#Definir función a minimizar
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

#Efectividad de AC
#Método     #Perfecto   #Típico       #Prob Embarazo #Porcentaje de uso según la base (Después dividir por edad)
#Pastilla     99%         91%           0.09          10.64%
#Emergencia         88%                 0.12          0.3%
#Inyección    99%         94%           0.06          11.11%
#Condón       98%         82%           0.18          26.34%
#Implante     99%         99%           0.01          3.22%
#DIU          99%         99%           0.01          33.72%
#Naturales    95%         70-80%        0.25          6.31%
#Irreversibles 99%                      0.01          6.73%
#Otros            72-91%                0.185         1.63%

library(MASS)
library(numDeriv)
library(msm)
library(rootSolve)
library(optimx)
library(NlcOptim)

setwd("/Users/Dalia/Documents/INSP/Embarazo/Bases")
Embarazo_estados <- read.csv("Embarazo.estados.csv")


#Tabla de Transición
Tabla.Transicion <- statetable.msm(estado, subject = el_folio, data = Embarazo_estados)

#No hay información disponible de las transiciones de usar pastilla de emergencia. 
#Por el momento se utilizará como renglón estimado la cantidad que ha usado pastilla de emergencia en algún momento 
#y ahora en dónde se encuentran
R3 <- table(Embarazo_estados$ac03h, Embarazo_estados$estado)[1,]
Tabla.Transicion2 <- rbind(Tabla.Transicion[1:2,],R3, Tabla.Transicion[3:14,])

#Matriz de transición al año 3
Mat.transicion.3 <- Tabla.Transicion2/rowSums(Tabla.Transicion2)

#Objfun es la función objetivo a minimizar, se busca obtener una matriz de transición tal que se minimice la distancia entre
#X^3 y la matriz de transiciones a tres años, minimizando la diferencia entre 1-efectividad de los anticonceptivos en la práctica
#con las probabilidades de transición del modelo.
objfun     <- function(X) {
  X2        <- rep(0,15^2)
  X3        <- rep(0,15^2)
  S         <-0
  for(j in 1:15){
    for(i in 1:15){
      for(k in 1:15){
        X2[15*(i-1)+j] <- X2[15*(i-1)+j] + X[15*(i-1)+k]*X[15*(k-1)+j]
      }
    }
  }
  for(j in 1:15){
    for(i in 1:15){
      for(k in 1:15){
        X3[15*(i-1)+j] <- X3[15*(i-1)+j] + X2[15*(i-1)+k]*X[15*(k-1)+j]
      }
    }
    S <- S + abs(X3[15*(i-1)+j]-Mat.transicion.3[i,j])
  }
  minimize <- S
  minimize
}

#Condiciones que deben ser iguales a cero
confun <- function(x){
  ceq <- NULL #Condiciones de igualdad
  for(i in 1:14){
    ceq <- rbind(ceq, x[i*15+1])
  }
  ceq <- rbind(ceq, x[15], x[13*15]) #NSA e inactivas no pueden embarazarse
  ceq <- rbind(ceq, x[6*15] - 0.01) #Probabilidad de ser SA con implante a embarazada
  ceq <- rbind(ceq, x[7*15] - 0.01) #Probabilidad de ser SA con DIU a embarazada
  
  #Condiciones de desigualdad
  c   <- rbind(x[1]-0.975, 0.875-x[1]) #Probabilidad de mantenerse en NSA
  c   <- rbind(c, x[13], x[14])    #NSA no pasan a inactivas o a embarazo poco probable
  c   <- rbind(c, x[2*15] - 0.09,  0.01 - x[2*15] ) #Probabilidad de SA con pastilla a embarazada
  c   <- rbind(c, x[3*15] - 0.12,  0.05 - x[3*15] ) #Probabilidad de SA con pastilla de emergencia a embarazada
  c   <- rbind(c, x[4*15] - 0.06,  0.01 - x[4*15] ) #Probabilidad de SA con inyecciones a embarazada
  c   <- rbind(c, x[5*15] - 0.18,  0.02 - x[5*15] ) #Probabilidad de SA con condón a embarazada
  c   <- rbind(c, x[8*15] - 0.3,    0.2 - x[8*15] ) #Probabilidad de SA con AC naturales a embarazada
  c   <- rbind(c, x[9*15] - 0.185, 0.01 - x[9*15] ) #Probabilidad de SA con otros AC a embarazada
  c   <- rbind(c, x[10*15] - 0.5,  0.2 - x[10*15] ) #Probabilidad de SA sin AC poco modificable a embarazada
  c   <- rbind(c, x[11*15] - 0.5,  0.2 - x[11*15] ) #Probabilidad de SA sin AC modificable a embarazada
  c   <- rbind(c, x[12*15] - 0.5,  0.2 - x[12*15] ) #Probabilidad de SA sin AC otros a embarazada
  c   <- rbind(c, x[14*15] - 0.01)  #Probabilidad de embarazo poco probable a embarazada
  
  list(ceq=ceq, c=c)
}

#Establecer que la suma de las entradas debe ser igual a uno
Beq <- rep(1,15)
Aeq <- matrix(0,ncol = 15^2, nrow = 15)

for (i in 1:15){
  Aeq[i,(15*(i-1)+1):(15*i)] <- 1
}

#Establecer que las entradas toman valores entre cero y uno
lb <- matrix(rep(0,15^2),ncol=1)
ub <- matrix(rep(1, 15^2),ncol=1)
X0 <- matrix(runif(15^2),ncol=15)
X0 <- X0/rowSums(X0)
X0 <- matrix(t(X0),ncol = 1)
#X <- matrix(X0,ncol=15, byrow = TRUE)

opt <- NlcOptim(X0, objfun = objfun, confun = confun, Aeq = Aeq, Beq = Beq,
                lb=lb, maxIter = 1)
fval <- opt$fval
sol  <- matrix(opt$p, ncol = 15, byrow = TRUE)
for(i in 1:20){
  opt <- NlcOptim(opt$p, objfun = objfun, confun = confun, Aeq = Aeq, Beq = Beq,
                  lb=lb, maxIter = 1)
  if(opt$fval < fval){
    fval <- opt$fval
    sol  <- matrix(opt$p, ncol = 15, byrow = TRUE)
  }
  print(i)
}

round(sol,2)
write.csv(sol, "Matriz_optim15_1")
