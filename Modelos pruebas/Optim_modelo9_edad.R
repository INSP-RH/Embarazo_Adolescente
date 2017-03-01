#Optimizar matriz de transición bajo función objetivo que considera minimizar Xp_i-p_{i+1},
#donde p_i es el vector de proporciones para la edad i y P es la matriz de transición, es un odelo markoviano homogéneo
#Modelo
#Valor     #Estado
# 1         No sexualmente activa
# 2         Sexualmente activa con Anticonceptivos
# 3         Sexualmente activa con Naturales
# 4         Sexualmente activa sin AC poco modificable
# 5         Sexualmente activa sin AC modificable
# 6         Sexualmente activa sin AC otras razones
# 7         Sexualmente inactiva
# 8         Probabilidad Baja de Embarazo
# 9         Embarazada

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

#Probabilidad de embarazo con AC no naturales 
library(MASS)
library(numDeriv)
library(msm)
library(rootSolve)
library(optimx)
library(NlcOptim)

source("Restricciones.R")
source("objun_edad.R")
X0  <- matrix(runif(9^2),ncol=9)
X0  <- X0/rowSums(X0)
X0  <- matrix(t(X0),ncol = 1)

opt  <- NlcOptim(X0, objfun = objfun, confun = confun, Aeq = Aeq, Beq = Beq,
                lb=lb, maxIter = 1)
fval <- opt$fval
sol  <- matrix(opt$p, ncol = 9, byrow = TRUE)

for(i in 1:100){
  opt <- NlcOptim(X0, objfun = objfun, confun = confun, Aeq = Aeq, Beq = Beq,
               lb=lb, maxIter = i)
  if(opt$fval < fval){
    fval <- opt$fval
    sol  <- matrix(opt$p, ncol = 9, byrow = TRUE)
  }
  print(i)
}
round(sol,3)

write.csv(sol, "Matriz_optim9_1.csv")
