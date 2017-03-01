#Restricciones de los parámetros
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

#Efectividad de AC
#Método           #Perfecto   #Típico       #Prob Embarazo
#Pastilla/Parche     99%         91%           0.09         
#LARCS               99%         91%           0.01        
#Condón              98%         82%           0.18        
#Naturales           95%         70%           0.25    
#Irreversibles      100%                       0.0          
#Otros             72-91%                      0.185       

setwd("/Users/Dalia/Documents/INSP/Embarazo/Bases")
Embarazo <- read.csv("Embarazo.estados.csv")


#Condiciones que deben ser iguales a cero
confun <- function(x){
  ceq <- NULL #Condiciones de igualdad
  for(i in 1:11){
    ceq <- rbind(ceq, x[i*12+1]) #No se puede regresar a NSA
  }
  ceq <- rbind(ceq, x[12], x[10*12]) #NSA e inactivas no pueden embarazarse
  ceq <- rbind(ceq, x[10], x[11])    #NSA no pasan a inactivas o a embarazo poco probable
  ceq <- rbind(ceq, x[2*12] - 0.09)  #Probabilidad de SA con Pastilla/Parche
  ceq <- rbind(ceq, x[3*12] - 0.01)  #Probabilidad de SA con LARCS
  ceq <- rbind(ceq, x[4*12] - 0.018) #Probabilidad de SA con Condón
  ceq <- rbind(ceq, x[5*12] - 0.3)   #Probabilidad de SA con Naturales
  ceq <- rbind(ceq, x[6*12] - 0.185) #Probabilidad de SA con Otros
  ceq <- rbind(ceq, x[11*12])        #Probabilidad de embarazo poco probable a embarazada
  
  #Condiciones de desigualdad
  c   <- rbind(x[1]-0.999, 0.8 - x[1]) #Probabilidad de mantenerse en NSA
  c   <- rbind(c,  0.4 - x[7*12] ) #Probabilidad de SA sin AC no modificable a embarazada
  c   <- rbind(c,  0.4 - x[8*12] ) #Probabilidad de SA sin AC poco modificable a embarazada
  c   <- rbind(c,  0.4 - x[9*12] ) #Probabilidad de SA sin AC poco modificable a embarazada
  list(ceq=ceq, c=c)
}

#Establecer que la suma de las entradas debe ser igual a uno
Beq <- rep(1,12)
Aeq <- matrix(0,ncol = 12^2, nrow = 12)
for (i in 1:12){
  Aeq[i,(12*(i-1)+1):(12*i)] <- 1
}

#Establecer que las entradas toman valores entre cero y uno
lb <- matrix(rep(0,12^2),ncol=1)
ub <- matrix(rep(1, 12^2),ncol=1)