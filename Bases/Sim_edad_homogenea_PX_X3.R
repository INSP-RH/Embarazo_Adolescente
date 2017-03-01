#Simulación del modelo de embarazo para el modelo restringido Se asume un modelo markoviano homogéneo
#Modelo 
#Valor     #Estado
# 1         No sexualmente activa
# 2         Sexualmente activa con Pastilla/Parche
# 3         Sexualmente activa con LARCS (DIU, Implante, Inyección)
# 4         Sexualmente activa con Condón
# 5         Sexualmente activa con Naturales
# 6         Sexualmente activa con Otros AC/ con AC no dijo cuál
# 7         Sexualmente activa sin AC no modificable
# 8         Sexualmente activa sin AC poco modificable
# 9         Sexualmente activa sin AC modificable
# 10        Sexualmente inactiva
# 11        Probabilidad Baja de Embarazo
# 12        Embarazada
# 13        Muerte
setwd("/Users/Dalia/Documents/INSP/Embarazo/Bases")
Embarazo <- read.csv("Embarazo.estados.csv")
set.seed(834873)
library(markovchain)
library(msm)
source("proba_inicial_14.R")

sol  <- as.matrix(read.csv("Mat_PX_X3.csv")[,2:14])
sol2 <- (abs(sol))
sol2 <- sol2/rowSums(sol2)
#Reescribir como objeto markovchain
#statesNames <- c("NSA","Pastilla", "LARCS","Condón","Naturales","Otros AC",
#"No_Mod","Poco_Mod", "Modificable","Inactiva","Prob_Baja", "Embarazada")

statesNames  <- c(".1",".2",".3",".4",".5",".6",".7",".8",".9","10","11","12","13")
mcA         <- new("markovchain", transitionMatrix=matrix(sol2, ncol = 13, dimnames=list(statesNames,statesNames)))
#Número de individuos en la simulación
N   <- 10000
m   <- 500
prop.embarazos.edad <- matrix(NA, nrow = 5)
prop.embarazos      <- c()
for(j in 1:m){
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
  #Tabla por edad y estado 
  tabla      <- table(Simulacion.Embarazo[Simulacion.Embarazo$Edad>14,]$Estado.sim)
  tabla.edad <- table(Simulacion.Embarazo[Simulacion.Embarazo$Edad>14,]$Edad,
                      Simulacion.Embarazo[Simulacion.Embarazo$Edad>14,]$Estado.sim)
  
  #Tabla de proporciones de edad y estado quitando las que han muerto
  emb           <- which(names(tabla)=="12")
  muertas       <- which(names(tabla)=="13")
  if(length(muertas!=0)){
    tabla <- tabla[-muertas]
    tabla.edad <- tabla.edad[,-muertas]
  }
  tabla <- tabla/sum(tabla)
  tabla.edad <- tabla.edad/rowSums(tabla.edad)
  prop.embarazos.edad <- cbind(prop.embarazos.edad, tabla.edad[,emb]*100)
  prop.embarazos <- c(prop.embarazos, tabla[emb]*100)
  print(j)
}
write.csv(prop.embarazos.edad[2:m],"Prop_Embarazos_edad_PX_X3_homogeneo.csv")
write.csv(prop.embarazos[2:m],"Prop_Embarazos_PX_X3_homogeneo.csv")
summary(prop.embarazos[2:500])
