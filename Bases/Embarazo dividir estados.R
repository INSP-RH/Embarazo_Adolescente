#Se define en qué estado del modelo está una adolescente
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

setwd("/Users/Dalia/Documents/INSP/Embarazo/Bases")
Embarazo <- read.csv("Embarazo.csv")

#Anticonceptivos
#1     Pastilla/Parche
#2     LARCS (Inyecciones, Implante, DIU)
#3     Condón
#4     Naturales
#5     Irreversibles
#6     Otros
#Definir metodo anticonceptivo acotando
Embarazo$metodo <- NA
Embarazo$metodo[Embarazo$ac11==3]    <- 0 #No usa       
Embarazo$metodo[Embarazo$ac12_1==1]  <- 1 #Pastilla    -> Pastilla/Parche
Embarazo$metodo[Embarazo$ac12_1==2]  <- 6 #Emergencia  -> Otros
Embarazo$metodo[Embarazo$ac12_1==3]  <- 2 #Inyecciones -> LARCS
Embarazo$metodo[Embarazo$ac12_1==4]  <- 3 #Condón      -> Condón
Embarazo$metodo[Embarazo$ac12_1==5]  <- 2 #Implante    -> LARCS
Embarazo$metodo[Embarazo$ac12_1==6]  <- 4 #Hierbas     -> Naturales
Embarazo$metodo[Embarazo$ac12_1==7]  <- 2 #DIU         -> LARCS
Embarazo$metodo[Embarazo$ac12_1==8]  <- 4 #Ritmo       -> Naturales
Embarazo$metodo[Embarazo$ac12_1==9]  <- 4 #Coito interrumpido -> Naturales
Embarazo$metodo[Embarazo$ac12_1==10] <- 5 #Operada    -> Irreversible
Embarazo$metodo[Embarazo$ac12_1==11] <- 5 #Vasectomía -> Irreversible
Embarazo$metodo[Embarazo[Embarazo$year != 2009,]$ac12_1==12] <- 6 #Otros -> Otros
Embarazo$metodo[Embarazo[Embarazo$year == 2009,]$ac12_1==12] <- 1 #Parche -> Pastilla/Parche
Embarazo$metodo[Embarazo$ac12_1==13] <- 6 #Otros -> Otros

#Definir razones por las que no usa AC
#0    Sí Usa
#1    Embarazada
#2    No modificable
#3    Poco modificable con política de acceso a AC
#4    Modificable con política de acceso a AC
#5    Inactiva
#6    Embarazo poco probable

Embarazo$razon <-NA
Embarazo$razon[Embarazo$ac11==1]    <- 0 #Usa anticonceptivos
Embarazo$razon[Embarazo$razon1==1]  <- 1 #Embarazada
Embarazo$razon[Embarazo$razon1==2]  <- 2 #Quiere Embarazarse    -> No modificable 
Embarazo$razon[Embarazo$razon1==3]  <- 4 #Falta de Conocimiento -> Modificable
Embarazo$razon[Embarazo$razon1==4]  <- 3 #Desaprobación         -> Poco modificale
Embarazo$razon[Embarazo$razon1==5]  <- 4 #Costo                 -> Modificable
Embarazo$razon[Embarazo$razon1==6]  <- 4 #Razones de Salud      -> Modificable
Embarazo$razon[Embarazo$razon1==7]  <- 4 #Efectos secundarios   ->  Modificable
Embarazo$razon[Embarazo$razon1==8]  <- 4 #Consejo de Doctor/Enfermera -> Modificable
Embarazo$razon[Embarazo$razon1==9]  <- 4 #Dificultad de obtener AC    -> Modificable 
Embarazo$razon[Embarazo$razon1==10] <- 3 #Religión                    -> Poco modificable
Embarazo$razon[Embarazo$razon1==11] <- 5 #Poca Frec de sexo           -> Inactiva/Modificable
Embarazo$razon[Embarazo$razon1==12] <- 4 #Dificultad de Embarazo      -> Modificable
Embarazo$razon[Embarazo$razon1==13] <- 6 #Menopausia                  -> Embarazo poco probable
Embarazo$razon[Embarazo$razon1==14] <- 4 #Dar a luz: No regla         -> Modificable
Embarazo$razon[Embarazo$razon1==15] <- 5 #Dar a luz: No sexo          -> Inactiva
Embarazo$razon[Embarazo$razon1==16] <- 4 #Dando pecho                 -> Modificable
Embarazo$razon[Embarazo$razon1==17] <- 6 #Esteril                     -> Embarazo poco probable/Modificable(si el enfoque es también hacia ETS)
Embarazo$razon[Embarazo$razon1==18] <- 5 #Ausencia temporal de la pareja -> Inactiva
Embarazo$razon[Embarazo$razon1==19] <- 4 #No necesita                 -> Modificable
Embarazo$razon[Embarazo$razon1==20] <- 6 #Operada                     -> Embarazo poco probable
Embarazo$razon[Embarazo$razon1==21] <- 4 #No quiere                   -> Modificable
Embarazo$razon[Embarazo$razon1==22] <- 3 #Otro método                 -> Poco modificable

#Definir los estados del modelo
Embarazo$estado <- NA

#Sexualmente Activa y usa anticonceptivos
Embarazo$estado[Embarazo$metodo == 1] <- 2  #Pastilla/Parche
Embarazo$estado[Embarazo$metodo == 2] <- 3  #LARCS
Embarazo$estado[Embarazo$metodo == 3] <- 4  #Condón
Embarazo$estado[Embarazo$metodo == 4] <- 5  #Naturales
Embarazo$estado[Embarazo$metodo == 6] <- 6  #Otros AC
Embarazo$estado[Embarazo$ac11==1 & is.na(Embarazo$estado)] <- 6  #Otros AC/No dijo

#Mujeres que son estériles, están operadas, su pareja tiene vasectomía, etc
Embarazo$estado[Embarazo$metodo == 5]  <- 11  #Irreversibles/Embarazo poco probable
Embarazo$estado[Embarazo$razon  == 6]  <- 11  #Irreversibles/Embarazo poco probable

#Sexualmente Activas sin Anticonceptivos que pueden quedar embarazadas
Embarazo$estado[Embarazo$razon == 2]  <- 7 #No Modificable
Embarazo$estado[Embarazo$razon == 3]  <- 8 #Poco Modificable
Embarazo$estado[Embarazo$razon == 4]  <- 9 #Modificable
#Inactivas
Embarazo$estado[Embarazo$ac37_1 == 4]  <- 10 #Inactiva
Embarazo$estado[Embarazo$razon == 5 & Embarazo$ac36 == 3] <- 10 #Inactiva

#No sexualmente Activa
Embarazo$estado[Embarazo$ivsa == 0]   <- 1  #No sexualmente activa
Embarazo$estado[Embarazo$ac11==3 & is.na(Embarazo$estado)] <- 8 #No usa AC y no dio razones es poco modificable.

#Embarazadas considerando no sólo actualmente embarazadas, 
#sino aquellas que estuvieron embarazadas durante el año que se hace el estudio 
Embarazo$estado[Embarazo$he01c==1]    <- 12  #Embarazada
Embarazo$estado[Embarazo$he01a_1c==1] <- 12  #Embarazada
Embarazo$estado[Embarazo$he01_1c==1]  <- 12  #Embarazada

# Embarazo$estado[which(is.na(Embarazo$he12_231)==FALSE & Embarazo$he12_231==Embarazo$year & is.na(Embarazo$estado))] <- 12
# Embarazo$estado[which(is.na(Embarazo$he12_232)==FALSE & Embarazo$he12_232==Embarazo$year & is.na(Embarazo$estado))] <- 12
# Embarazo$estado[which(is.na(Embarazo$he12_233)==FALSE & Embarazo$he12_233==Embarazo$year & is.na(Embarazo$estado))] <- 12
# Embarazo$estado[which(is.na(Embarazo$he12_234)==FALSE & Embarazo$he12_234==Embarazo$year & is.na(Embarazo$estado))] <- 12

Embarazo$estado[which(is.na(Embarazo$he12_231)==FALSE & Embarazo$he12_231==Embarazo$year)] <- 12
Embarazo$estado[which(is.na(Embarazo$he12_232)==FALSE & Embarazo$he12_232==Embarazo$year)] <- 12
Embarazo$estado[which(is.na(Embarazo$he12_233)==FALSE & Embarazo$he12_233==Embarazo$year)] <- 12
Embarazo$estado[which(is.na(Embarazo$he12_234)==FALSE & Embarazo$he12_234==Embarazo$year)] <- 12


round(table(Embarazo[Embarazo$edad<=19,]$edad,Embarazo[Embarazo$edad<=19,]$estado
            )/rowSums(table(Embarazo[Embarazo$edad<=19,]$edad,Embarazo[Embarazo$edad<=19,]$estado)),3)*100

round(table(Embarazo[Embarazo$edad<=19,]$estado
)/sum(table(Embarazo[Embarazo$edad<=19,]$estado)),3)*100


write.csv(Embarazo,"Embarazo.estados.csv")
