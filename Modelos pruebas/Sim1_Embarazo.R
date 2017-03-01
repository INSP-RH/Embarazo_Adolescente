library(markovchain)
sol2 <- round(sol,8)
sol2 <- sol2/rowSums(sol2)
statesNames=c("1","2", "3","4","5","6","7","8", "9","10","11","12","13","14", "15")
mcA<-new("markovchain", transitionMatrix=matrix(sol2, ncol = 15, dimnames=list(statesNames,statesNames)))

Individuo   <- c()
Estado.sim  <- rep(c("1", NA, NA, NA, NA, NA),10000)
for(i in 1:10000){
  Individuo <- c(Individuo, rep(i,6))
}
Simulacion.Embarazo <- as.data.frame(Individuo)

Simulacion.Embarazo$Estado.sim <- Estado.sim

for(i in 1:10000){
  outs <- markovchainSequence(n=5, markovchain = mcA, t0 ="1")
  Simulacion.Embarazo$Estado.sim[6*(i-1)+2] <- outs[1]
  Simulacion.Embarazo$Estado.sim[6*(i-1)+3] <- outs[2]
  Simulacion.Embarazo$Estado.sim[6*(i-1)+4] <- outs[3]
  Simulacion.Embarazo$Estado.sim[6*(i-1)+5] <- outs[4]
  Simulacion.Embarazo$Estado.sim[6*i]       <- outs[5]
  
}
