#Definir una condición inicial dentro del conjunto factible por columnas
set.seed(98459534)
X0           <- rep(0,12^2)
X0[1]        <- 0.875   #Probabilidad de sequir siendo NSA
X0[12*11+1]  <- 0     #NSA no pueden embarazarse
X0[12*11+2]  <- 0.09  #Probabilidad de SA con Pastilla/Parche
X0[12*11+3]  <- 0.01  #Probabilidad de SA con LARCS
X0[12*11+4]  <- 0.18  #Probabilidad de SA con Condón
X0[12*11+5]  <- 0.3   #Probabilidad de SA con Naturales
X0[12*11+6]  <- 0.185 #Probabilidad de SA con Otros
X0[12*11+7]  <- 0.9   #Probabilidad de SA sin AC no modificable a embarazada
X0[12*11+8]  <- 0.9   #Probabilidad de SA sin AC poco modificable a embarazada
X0[12*11+9]  <- 0.9   #Probabilidad de SA sin AC poco modificable a embarazada
X0[12*11+10] <- 0     #Inactivas no pueden embarazarse
X0[12*11+11] <- 0     #Operadas y estériles no pueden embarazarse
X0[12*9+1]   <- 0     #NSA no pueden pasar a inactivas
X0[12*10+1]  <- 0     #NSA no pueden pasar a operadas o estériles
#No pueden regresar a NSA
X0[2:12]    <- 0

#Las que están operadas o son estériles no pueden tener hijos independientemente
#si son sexualmente activas o no, por lo que una vez que son estériles no se consideran en el modelo más que para mortalidad
for(i in 1:10){
  X0[12*(i-1)+11] <- 0
}
X0[12*11+11]      <- 0