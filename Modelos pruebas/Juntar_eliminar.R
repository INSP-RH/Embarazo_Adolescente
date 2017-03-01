#Bibliotecas para leer bases en stata
devtools::install_github("hadley/haven")
library(haven)
#Definir carpeta
setwd("/Users/Dalia/Documents/INSP/Embarazo/Bases")

#Leer base
Embarazo  <- read_dta("Base sin duplicados por ano _limpia.dta")
Wide      <- read_dta("base wide preeliminar.dta")

#Sacar razon Ac23
Ac23      <- read_dta("ac23.dta")
Ac23[Ac23=="NaN"] <- 0
Ac23$razon1 <- NA
for(i in 1:dim(Ac23)[1]){
  razones       <- which(Ac23[i,3:24]!=0)
  if(length(razones)!=0){
    Ac23$razon1[i] <- razones[1]
  }
}


Embarazo  <- cbind(Embarazo, Ac23[,3:25])
#Quitar chicas que pasan de ser sexualmente activas a no haber iniciado vida sexual
error.ivsa <- Wide$el_folio[which(Wide$ivsa2005==0 & Wide$ivsa2002==1)]
error.ivsa <- c(error.ivsa, Wide$el_folio[which(Wide$ivsa2009==0 & Wide$ivsa2002==1)])
error.ivsa <- c(error.ivsa, Wide$el_folio[which(Wide$ivsa2009==0 & Wide$ivsa2005==1)])
error.ivsa <- unique(error.ivsa)

for(i in 1: length(error.ivsa)){
  Embarazo <- Embarazo[Embarazo$el_folio!=error.ivsa[i],] 
  Wide     <- Wide[Wide$el_folio!=error.ivsa[i],]
}

#Quitar chicas cuya edad disminuye con el tiempo
#Error in msm.check.times(time, subject, state) : 
#   Observations within subjects 0046700002, 0698200003 are not ordered by time
Embarazo <- Embarazo[Embarazo$el_folio!="0046700002",]
Embarazo <- Embarazo[Embarazo$el_folio!="0698200003",] 
Wide     <- Wide[Wide$el_folio!="0046700002",]
Wide     <- Wide[Wide$el_folio!="0698200003",]
write.csv(Wide, "Wide.csv")
Embarazo <- Embarazo[order(Embarazo$el_folio),]
write.csv(Embarazo, "Embarazo.csv")