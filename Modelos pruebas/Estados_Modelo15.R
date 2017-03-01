#Se define en qué estado del modelo está una adolescente
#Modelo 
#Valor     #Estado
# 1         No sexualmente activa
# 2         Sexualmente activa con Pastilla
# 3         Sexualmente activa con Pastilla de Emergencia
# 4         Sexualmente activa con Inyecciones
# 5         Sexualmente activa con Condón
# 6         Sexualmente activa con Implante
# 7         Sexualmente activa con DIU
# 8         Sexualmente activa con Naturales
# 9         Sexualmente activa con Otros anticonceptivos 
# 10        Sexualmente activa sin AC poco modificable
# 11        Sexualmente activa sin AC modificable
# 12        Sexualmente activa sin AC otras razones
# 13        Sexualmente inactiva
# 14        Probabilidad Baja de Embarazo
# 15        Embarazada

setwd("/Users/Dalia/Documents/INSP/Embarazo/Bases")
Embarazo <- read.csv("Embarazo.csv")
#Definir metodo anticonceptivo acotando
Embarazo$metodo <- NA
Embarazo$metodo[Embarazo$ac11==3]    <- 0 #No usa       
Embarazo$metodo[Embarazo$ac12_1==1]  <- 1 #Pastilla
Embarazo$metodo[Embarazo$ac12_1==2]  <- 2 #Emergencia
Embarazo$metodo[Embarazo$ac12_1==3]  <- 3 #Inyecciones 
Embarazo$metodo[Embarazo$ac12_1==4]  <- 4 #Condón
Embarazo$metodo[Embarazo$ac12_1==5]  <- 5 #Implante
Embarazo$metodo[Embarazo$ac12_1==6]  <- 7 #Hierbas -> Naturales
Embarazo$metodo[Embarazo$ac12_1==7]  <- 6 #DIU 
Embarazo$metodo[Embarazo$ac12_1==8]  <- 7 #Ritmo -> Naturales
Embarazo$metodo[Embarazo$ac12_1==9]  <- 7 #Coito interrumpido -> Naturales
Embarazo$metodo[Embarazo$ac12_1==10] <- 8 #Operada -> Irreversible
Embarazo$metodo[Embarazo$ac12_1==11] <- 8 #Vasectomía -> Irreversible
Embarazo$metodo[Embarazo$ac12_1==12] <- 9 #Otros/Parche -> Otros
Embarazo$metodo[Embarazo$ac12_1==13] <- 9 #Otros -> Otros

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
Embarazo$razon[Embarazo$razon1==2]  <- 2 #Quiere Embarazarse -> No modificable 
Embarazo$razon[Embarazo$razon1==3]  <- 3 #Falta de Conocimiento -> Modificable
Embarazo$razon[Embarazo$razon1==4]  <- 2 #Desaprobación -> Poco modificale
Embarazo$razon[Embarazo$razon1==5]  <- 3 #Costo -> Modificable
Embarazo$razon[Embarazo$razon1==6]  <- 3 #Razones de Salud -> Modificable
Embarazo$razon[Embarazo$razon1==7]  <- 3 #Efectos secundarios ->  Modificable
Embarazo$razon[Embarazo$razon1==8]  <- 3 #Consejo de Doctor/Enfermera -> Modificable
Embarazo$razon[Embarazo$razon1==9]  <- 3 #Dificultad de obtener AC -> Modificable 
Embarazo$razon[Embarazo$razon1==10] <- 2 #Religión -> Poco modificable
Embarazo$razon[Embarazo$razon1==11] <- 4 #Poca Frec de sexo -> Inactiva/ No modificable
Embarazo$razon[Embarazo$razon1==12] <- 5 #Dificultad de Embarazo -> Modificable
Embarazo$razon[Embarazo$razon1==13] <- 5 #Menopausia -> Embarazo poco probable
Embarazo$razon[Embarazo$razon1==14] <- 5 #Dar a luz: No regla -> Modificable
Embarazo$razon[Embarazo$razon1==15] <- 4 #Dar a luz: No sexo -> Inactiva
Embarazo$razon[Embarazo$razon1==16] <- 6 #Dando pecho -> Modificable
Embarazo$razon[Embarazo$razon1==17] <- 5 #Esteril -> Modificable
Embarazo$razon[Embarazo$razon1==18] <- 4 #Ausencia temporal de la pareja -> Inactiva
Embarazo$razon[Embarazo$razon1==19] <- 2 #No necesita -> Modificable
Embarazo$razon[Embarazo$razon1==20] <- 5 #Operada -> Embarazo poco probable
Embarazo$razon[Embarazo$razon1==21] <- 2 #No quiere -> Modificable
Embarazo$razon[Embarazo$razon1==22] <- 6 #Otro método -> Poco modificable

#Definir los estados del modelo
Embarazo$estado <- NA

#No sexualmente Activa
Embarazo$estado[Embarazo$ivsa == 0]   <- 1  #No sexualmente activa

#Sexualmente Activa y usa anticonceptivos
Embarazo$estado[Embarazo$metodo == 1] <- 2  #Pastilla
Embarazo$estado[Embarazo$metodo == 2] <- 3  #Emergencia
Embarazo$estado[Embarazo$metodo == 3] <- 4  #Inyecciones
Embarazo$estado[Embarazo$metodo == 4] <- 5  #Condón
Embarazo$estado[Embarazo$metodo == 5] <- 6  #Implante
Embarazo$estado[Embarazo$metodo == 6] <- 7  #DIU
Embarazo$estado[Embarazo$metodo == 7] <- 8  #Naturales
Embarazo$estado[Embarazo$metodo == 8] <- 9  #Otros AC
Embarazo$estado[Embarazo$ac11 & is.na(Embarazo$estado)] <- 7  #Otros AC

#Mujeres que son estériles, están operadas, su pareja tiene vasectomía, etc
Embarazo$estado[Embarazo$metodo == 6] <- 14  #Irreversibles/Embarazo muy poco probable
Embarazo$estado[Embarazo$razon == 5]  <- 14  #Irreversibles/Embarazo muy poco probable

#Sexualmente Activas sin Anticonceptivos que pueden quedar embarazadas
Embarazo$estado[Embarazo$razon == 2]  <- 10 #Poco Modificable
Embarazo$estado[Embarazo$razon == 3]  <- 11 #Modificable
Embarazo$estado[Embarazo$razon == 6]  <- 12 #Otras razones

#Inactivas
Embarazo$estado[Embarazo$ac37_1 == 4]  <- 13 #Inactiva
Embarazo$estado[Embarazo$razon == 4 & Embarazo$ac36 == 3] <- 13 #Inactiva

#Embarazadas
Embarazo$estado[Embarazo$he01c ==  1] <- 15  #Embarazada
Embarazo$estado[Embarazo$he01a_1c==1] <- 15  #Embarazada
Embarazo$estado[Embarazo$he01_1c ==1] <- 15  #Embarazada

write.csv(Embarazo,"Embarazo.estados.csv")
