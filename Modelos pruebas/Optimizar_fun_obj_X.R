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

setwd("/Users/Dalia/Documents/INSP/Embarazo/Bases")
Embarazo <- read.csv("Embarazo.estados.csv")
source("Restricciones.R")
source("Fun_obj_X.R")

X0 <- matrix(runif(12^2),ncol=12)
X0 <- X0/rowSums(X0)
X0 <- matrix(t(X0),ncol = 1)
opt <- NlcOptim(X0, objfun = objfun, confun = confun, Aeq = Aeq, Beq = Beq,
                lb=lb, maxIter = 2)
fval <- opt$fval
sol <- matrix(opt$p, ncol=12, byrow = TRUE)
round(sol,3)
write.csv(sol, "Mat_X.csv")