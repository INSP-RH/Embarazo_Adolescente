#Simulación del modelo de embarazo para el modelo con 9 estados. Se asume un modelo de compartimentos 
#cuya matriz de transición depende de la edad
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

setwd("/Users/Dalia/Documents/INSP/Embarazo/Bases")
Embarazo <- read.csv("Embarazo.estados.csv")
source("proba_inicial_14.R")
#Leer bases
MAT_14_15 <- as.matrix(read.csv("Mat_14_15.csv")[,2:14])
MAT_15_16 <- as.matrix(read.csv("Mat_15_16.csv")[,2:14])
MAT_16_17 <- as.matrix(read.csv("Mat_16_17.csv")[,2:14])
MAT_17_18 <- as.matrix(read.csv("Mat_17_18.csv")[,2:14])
MAT_18_19 <- as.matrix(read.csv("Mat_18_19.csv")[,2:14])

N          <- 10000 #Número de individuos
E_0        <- sample(1:13,N, prob=probas, replace = TRUE)

Individuo <- c()
for(i in 1:N){
  Individuo   <- c(Individuo, rep(i,6))
}
Simulacion.Embarazo      <- as.data.frame(Individuo)
Edad        <- rep(c("14","15","16","17","18","19"), N)
Simulacion.Embarazo$Edad <- Edad 

Estado.sim <- c()

for(i in 1:N){
  e1  <- sample(1:13,1,prob = MAT_14_15[E_0[i],])
  e2  <- sample(1:13,1,prob = MAT_15_16[e1,])
  e3  <- sample(1:13,1,prob = MAT_16_17[e2,])
  e4  <- sample(1:13,1,prob = MAT_17_18[e3,])
  e5  <- sample(1:13,1,prob = MAT_18_19[e4,])
  Estado.sim <- c(Estado.sim, E_0[i],e1,e2,e3,e4,e5)
}

Simulacion.Embarazo$Estado.sim <- Estado.sim
# 
 Tabla.Edad <- table(Embarazo$edad, Embarazo$estado)[1:6,]
 Tabla.Edad <- round(Tabla.Edad/(rowSums(Tabla.Edad))*100,2)
 Tabla.Edad.sim  <- table(Simulacion.Embarazo$Edad, Simulacion.Embarazo$Estado.sim)
 Tabla.Edad.sim  <- Tabla.Edad.sim/rowSums(Tabla.Edad.sim)*100
 Tabla.Edad.sim
 Tabla.Edad
# table(Simulacion.Embarazo$Estado.sim)/sum(table(Simulacion.Embarazo$Estado.sim))
# table(Embarazo[Embarazo$edad<20,]$estado)/sum(table(Embarazo[Embarazo$edad<20,]$estado))
# statetable.msm(Estado.sim,subject = Individuo, data = Simulacion.Embarazo)
table(Simulacion.Embarazo$Estado.sim)/(sum(table(Simulacion.Embarazo$Estado.sim)))*100
table(Simulacion.Embarazo$Estado.sim)[1:12]/(sum(table(Simulacion.Embarazo$Estado.sim)[1:12]))*100

table(Embarazo$estado[Embarazo$edad>=14 & Embarazo$edad<=19])/(sum(table(Embarazo$estado[Embarazo$edad>=14 & Embarazo$edad<=19])))*100

