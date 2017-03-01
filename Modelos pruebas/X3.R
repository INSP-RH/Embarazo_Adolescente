objfun     <- function(X) {
  X2        <- rep(0,9^2)
  X3        <- rep(0,9^2)
  S         <-0
  for(j in 1:9){
    for(i in 1:9){
      for(k in 1:9){
        X2[9*(i-1)+j] <- X2[9*(i-1)+j] + X[9*(i-1)+k]*X[9*(k-1)+j]
      }
    }
  }
  for(j in 1:9){
    for(i in 1:9){
      for(k in 1:9){
        X3[9*(i-1)+j] <- X3[9*(i-1)+j] + X2[9*(i-1)+k]*X[9*(k-1)+j]
      }
      S <- S + (X3[9*(i-1)+j]-Mat.transicion.3[i,j])^2
    }
  }
  S
}