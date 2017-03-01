#Restricciones de los parámetros definiendo a X por columnas
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
# 13        Muerte  (Como ya está dada la tasa de mortalidad y en los datos el estado muerte no existe, no se hace la optimización sobre muerte)
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


#Establecer que la suma de las entradas debe ser igual a uno menos la tasa de mortalidad
Beq <- rep((1-0.00042),12) 
sec <- seq(0,12*11, length=12)
Aeq <- matrix(0,ncol = 12^2, nrow = 12)
for (i in 1:12){
  Aeq[i, sec+i] <- 1
}

heq <- function(x){
  M   <- (Aeq%*%x-Beq)^2
  return(sum(M))
}


#Condiciones de igualdad y desigualdad
#Establecer que las entradas toman valores entre cero y uno
lb <- matrix(rep(0,12^2),ncol=1)
lb[1]        <- 0.8   #Probabilidad de sequir siendo NSA
lb[12*11+1]  <- 0     #NSA no pueden embarazarse
lb[12*11+2]  <- 0.09  #Probabilidad de SA con Pastilla/Parche
lb[12*11+3]  <- 0.01  #Probabilidad de SA con LARCS
lb[12*11+4]  <- 0.18  #Probabilidad de SA con Condón
lb[12*11+5]  <- 0.3   #Probabilidad de SA con Naturales
lb[12*11+6]  <- 0.185 #Probabilidad de SA con Otros
lb[12*11+7]  <- 0.88   #Probabilidad de SA sin AC no modificable a embarazada
lb[12*11+8]  <- 0.88   #Probabilidad de SA sin AC poco modificable a embarazada
lb[12*11+9]  <- 0.88   #Probabilidad de SA sin AC poco modificable a embarazada
lb[12*11+10] <- 0     #Inactivas no pueden embarazarse
lb[12*11+11] <- 0     #Operadas y estériles no pueden embarazarse
lb[12*9+1]   <- 0     #NSA no pueden pasar a inactivas
lb[12*10+1]  <- 0     #NSA no pueden pasar a operadas o estériles
ub <- matrix(rep(1, 12^2),ncol=1)
ub[1]        <- 0.94  #Probabilidad de sequir siendo NSA
ub[12*11+1]  <- 0     #NSA no pueden embarazarse
ub[12*11+2]  <- 0.09  #Probabilidad de SA con Pastilla/Parche
ub[12*11+3]  <- 0.01  #Probabilidad de SA con LARCS
ub[12*11+4]  <- 0.18  #Probabilidad de SA con Condón
ub[12*11+5]  <- 0.3   #Probabilidad de SA con Naturales
ub[12*11+6]  <- 0.185 #Probabilidad de SA con Otros
ub[12*11+7]  <- 1     #Probabilidad de SA sin AC no modificable a embarazada
ub[12*11+8]  <- 1     #Probabilidad de SA sin AC poco modificable a embarazada
ub[12*11+9]  <- 1     #Probabilidad de SA sin AC poco modificable a embarazada
ub[12*11+10] <- 0     #Inactivas no pueden embarazarse
ub[12*11+11] <- 0     #Operadas y estériles no pueden embarazarse
ub[12*9+1]   <- 0     #NSA no pueden pasar a inactivas
ub[12*10+1]  <- 0     #NSA no pueden pasar a operadas o estériles
#No pueden regresar a ser NSA
ub[2:12]     <- 0
#Las que están operadas o son estériles no pueden tener hijos independientemente
#si son sexualmente activas o no, por lo que una vez que son estériles no se consideran en el modelo más que para mortalidad
for(i in 1:10){
  ub[12*(i-1)+11] <- 0
}
ub[12*11+11]      <- 0