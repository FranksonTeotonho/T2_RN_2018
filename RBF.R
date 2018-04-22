#RBF
target <- function(x1, x2){
  return(2*(x2-x1 + 0.25*sin(pi*x1)>=0)-1)
}


N <- 300
X <- data.frame(x1 = runif(N,min = -1,max = 1),
                x2 = runif(N,min = -1,max = 1))

Y <- target(X$x1, X$x2)

plot(X$x1, X$x2, col = Y+3)

library(corpcor)

rbf <- function(X, Y, K=10, gama=1.0){
  N <- dim(X)[1]  #Numero de exemplos
  ncol <- dim(X)[2] #Numero de atributos
  
  repeat{
    km <- kmeans(X, K)
    if(min(km$size) > 0)
      break
  }
  
  #Calcular as saidas das gaussianas
  Phi <- matrix(rep(NA,(K+1)*N),ncol = K+1)
  mus <- km$centers
  for(lin in 1:N){
    Phi[lin,1] <- 1
    for(col in 1:K){
      Phi[lin,col+1] <- exp(-(1/(2*gama*gama))*sum((X[lin,]-mus)*(X[lin,]-mus)))
    }
  }
  
  #Calcula os pesos
  w <- pseudoinverse(t(Phi)%*%Phi) %*% t(Phi) %*% Y
  
  lista <- list()
  lista$pesos <- w
  lista$gama <- gama
  lista$centros <- mus
  
  return(lista)
  
}

rbf.predict <- function(modelo, X, classificacao = FALSE){
  gama <- modelo$gama
  centros <- modelo$centros
  w <- modelo$pesos
  N <- dim(X)[1]
  
  pred <- rep(w[1], N)
  
  for(j in 1:N){
    for(k in 1:nrow(centros)){
      pred[j] <- pred[j] + w[k+1] * exp((-1/2*gama*gama) * sum((X[j,] - centros)*(X[j,] - centros)))
    }
  }
  
  if(classificacao == TRUE){
    pred <- unlist(lapply(pred, sign))
  }
  
}

N.teste <- 200
X.out <- data.frame(x1 = runif(N.teste,min = -1,max = 1),
                    x2 = runif(N.teste,min = -1,max = 1))

Y.out <- target(X.out$x1, X.out$x2)

rbf.pred <- rbf.predict(modelo, X.out, classificacao = TRUE)

erro <- sum(rbf.pred != Y.out)/N.teste

plot(X.out$x1,X.out$x2, col = Y.out + 3, pch = 0)
points(X.out$x1,X.out$x2, col= rbf.pred + 3, pch = 3)
points(modelo$centros, col = 'black', pch = 19)
legend('topleft', c('verdadeiro','predito'), pch = c(0,3), bg = 'white')