#Simulación del modelo de embarazo para el modelo con 9 estados. Se asume un modelo markoviano homogéneo
library(markovchain)
source("proba_inicial_14.R")
sol2 <- round(sol,8)
sol2 <- sol2/rowSums(sol2)
#Reescribir como objeto markovchain
statesNames=c("1","2", "3","4","5","6","7","8", "9")
mcA    <- new("markovchain", transitionMatrix=matrix(sol2, ncol = 9, dimnames=list(statesNames,statesNames)))
#Número de individuos en la simulación
N   <- 400
Individuo   <- c()
Edad        <- rep(c("14","15","16","17","18","19"), N)
Estado.sim  <- c()
for(i in 1:N){
  Estado0     <- sample(statesNames, 1, prob = probas)
  Estado.sim  <- c(Estado.sim, Estado0, NA, NA, NA, NA, NA)
  Individuo   <- c(Individuo, rep(i,6))
}
Simulacion.Embarazo      <- as.data.frame(Individuo)
Simulacion.Embarazo$Edad <- Edad 
Simulacion.Embarazo$Estado.sim <- Estado.sim

for(i in 1:N){
  outs <- markovchainSequence(n=5, markovchain = mcA, t0 =Simulacion.Embarazo$Estado.sim[6*(i-1)+1])
  Simulacion.Embarazo$Estado.sim[6*(i-1)+2] <- outs[1]
  Simulacion.Embarazo$Estado.sim[6*(i-1)+3] <- outs[2]
  Simulacion.Embarazo$Estado.sim[6*(i-1)+4] <- outs[3]
  Simulacion.Embarazo$Estado.sim[6*(i-1)+5] <- outs[4]
  Simulacion.Embarazo$Estado.sim[6*i]       <- outs[5]
  
}

Tabla <- statetable.msm(Estado.sim,Individuo,data=Simulacion.Embarazo)
Tabla <- Tabla/rowSums(Tabla)
Tabla
Tabla.Edad.sim  <- table(Simulacion.Embarazo$Edad, Simulacion.Embarazo$Estado.sim)
Tabla.Edad.sim  <- Tabla.Edad.sim/rowSums(Tabla.Edad.sim)
Tabla.Edad.sim  <- Tabla.Edad.sim[1:6,]
Tabla.Edad.sim
