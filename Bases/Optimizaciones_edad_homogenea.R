#Optimizar matriz de transición tal que X se parezca a la mtriz de transición estimada
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
# 13        Muerta

#Efectividad de AC
#Método           #Perfecto   #Típico       #Prob Embarazo
#Pastilla/Parche     99%         91%           0.09         
#LARCS               99%         91%           0.01        
#Condón              98%         82%           0.18        
#Naturales           95%         70%           0.25    
#Irreversibles      100%                       0.0          
#Otros             72-91%                      0.185     

#Probabilidad de embarazo con AC no naturales 
library(nloptr)
setwd("/Users/Dalia/Documents/INSP/Embarazo/Bases")
Embarazo <- read.csv("Embarazo.estados.csv")
source("Restricciones_columna.R")
source("Fun_obj_PX_homogenea.R")
source("Fun_obj_X3_columnas.R")
source("Fun_obj_X_columnas.R")
source("X_inicial.R")

aug_PX            <- auglag(X0, fn=objfun_PX, lower = lb, upper = ub, heq = heq, localsolver = "SLSQP")
aug_X3            <- auglag(X0, fn=objfun_X3, lower = lb, upper = ub, heq = heq, localsolver = "SLSQP")
aug_X             <- auglag(X0, fn=objfun_X, lower = lb, upper = ub, heq = heq, localsolver = "SLSQP")
aug_PX_X3         <- auglag(X0, fn =function(X){objfun_PX(X)+objfun_X3(X)}, lower = lb, upper = ub, heq = heq, localsolver = "SLSQP")
aug_PX_X          <- auglag(X0, fn =function(X){objfun_PX(X)+objfun_X(X)}, lower = lb, upper = ub, heq = heq, localsolver = "SLSQP")
aug_PX_X_X3       <- auglag(X0, fn =function(X){objfun_PX(X)+objfun_X(X)+objfun_X3(X)}, lower = lb, upper = ub, heq = heq, localsolver = "SLSQP")
 
mort       <- rep(0.00042,12)
muerta     <- c(rep(0,12),1)

sol_PX             <- rbind(cbind(matrix(aug_PX$par,ncol = 12),mort),muerta)
sol_X3             <- rbind(cbind(matrix(aug_X3$par,ncol = 12),mort),muerta)
sol_X              <- rbind(cbind(matrix(aug_X$par,ncol = 12),mort),muerta)
sol_PX_X3          <- rbind(cbind(matrix(aug_PX_X3$par, ncol = 12),mort),muerta)
sol_PX_X           <- rbind(cbind(matrix(aug_PX_X$par, ncol = 12),mort),muerta)
sol_PX_X_X3        <- rbind(cbind(matrix(aug_PX_X_X3$par, ncol = 12),mort),muerta)


round(sol_PX,3)
round(sol_X3,3)
round(sol_X,3)
round(sol_PX_X3,3)
round(sol_PX_X,3)
round(sol_PX_X_X3,3)

write.csv(sol_PX, "Mat_PX.csv")
write.csv(sol_X3, "Mat_X3.csv")
write.csv(sol_X, "Mat_X.csv")
write.csv(sol_PX_X, "Mat_PX_X.csv")
write.csv(sol_PX_X3, "Mat_PX_X3.csv")
write.csv(sol_PX_X_X3, "Mat_PX_X_X3.csv")
