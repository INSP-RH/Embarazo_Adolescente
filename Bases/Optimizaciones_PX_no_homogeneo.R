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
library(MASS)
library(optimx)
library(NlcOptim)
library(nloptr)
setwd("/Users/Dalia/Documents/INSP/Embarazo/Bases")
Embarazo <- read.csv("Embarazo.estados.csv")
source("Restricciones_columna.R")
source("Fun_obj_PX_no_homogenea.R")
source("X_inicial.R")

aug_14_15  <- auglag(X0, fn=objfun_14_15, lower = lb, upper = ub, heq = heq, localsolver = "SLSQP")
aug_15_16  <- auglag(X0, fn=objfun_15_16, lower = lb, upper = ub, heq = heq, localsolver = "SLSQP")
aug_16_17  <- auglag(X0, fn=objfun_16_17, lower = lb, upper = ub, heq = heq, localsolver = "SLSQP")
aug_17_18  <- auglag(X0, fn=objfun_17_18, lower = lb, upper = ub, heq = heq, localsolver = "SLSQP")
aug_18_19  <- auglag(X0, fn=objfun_18_19, lower = lb, upper = ub, heq = heq, localsolver = "SLSQP")

mort       <- rep(0.00042,12)
muerta     <- c(rep(0,12),1)
sol_14_15  <- rbind(cbind(matrix(aug_14_15$par,ncol = 12), mort), muerta)
sol_15_16  <- rbind(cbind(matrix(aug_15_16$par,ncol = 12), mort), muerta)
sol_16_17  <- rbind(cbind(matrix(aug_16_17$par,ncol = 12), mort), muerta)
sol_17_18  <- rbind(cbind(matrix(aug_17_18$par,ncol = 12), mort), muerta)
sol_18_19  <- rbind(cbind(matrix(aug_18_19$par,ncol = 12), mort), muerta)


round(sol_14_15,3)
round(sol_15_16,3)
round(sol_16_17,3)
round(sol_17_18,3)
round(sol_18_19,3)

write.csv(sol_14_15,"Mat_14_15.csv")
write.csv(sol_15_16,"Mat_15_16.csv")
write.csv(sol_16_17,"Mat_16_17.csv")
write.csv(sol_17_18,"Mat_17_18.csv")
write.csv(sol_18_19,"Mat_18_19.csv")

