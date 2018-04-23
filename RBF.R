library(corpcor)
source("PreProcessamento.R")

rbf <- function(X, Y, K=10, gama=1.0) {
  N <- dim(X)[1] # numero de observacoes
  ncols <- dim(X)[2] # numero de variaveis
  
  repeat {
    
    km <- kmeans(X, K) # agrupa os dados em K grupos
    
    if (min(km$size)>0) # nao pode haver grupos vazios
      break
  }

  mus <- km$centers # centros dos grupos (medias)
  
  # Calcula as saidas das Gaussianas
  Phi <- matrix(rep(NA,(K+1)*N), ncol=K+1) # Vai armazenar todas as saidas mais o bias
  for (lin in 1:N) {
    Phi[lin,1] <- 1
    # coluna do bias
    for (col in 1:K) {
      Phi[lin,col+1] <- exp( (-1/(2*gama*gama)) *
                               sum((X[lin,]-mus[col,])*(X[lin,]-mus[col,])) )
      #Phi[lin,col+1] <- exp( -gama * sum((X[lin,]-mus[col,])*(X[lin,]-mus[col,])))
      #Phi[lin,col+1] <- exp( -gama * norm(as.matrix(X[lin,]-mus[col,]),"F")^2 )
    }
  }
  # Calcula os pesos com a pseudo inversa -> w = inversa(t(Phi) * Phi) * t(Phi) * Y
  # Encontra os pesos fazendo a inversa
  # %*% é para multiplicacao de matrizes
  matrizY <- matrix(data = unlist(Y), nrow = nrow(Y), ncol = 3)
  w <- pseudoinverse(t(Phi) %*% Phi) %*% t(Phi) %*% matrizY
  
  return(list(pesos=w, centros=mus, gama=gama)) # retorna o modelo RBF
}

rbf.preditos <- function(dados){
  classesPreditas <- c()
  
  for(i in 1:nrow(dados)){
    classe <- maxPosicaoVetor(dados[i,])
    classesPreditas <- c(classesPreditas, classe)
  }
  
  return (classesPreditas)
  
}

rbf.crossValidation <- function(datasets, K=10, gama=1.0){
  
  rbf <- list()
  acc <- list()
  m <- list()
  accMedia <- 0
  
  for(i in 1:length(datasets)){
    
    cat("Fold: ", i, "\n")
    
    X <- datasets[[i]]$treinamento[ ,1:4]
    Y <- datasets[[i]]$treinamento[ ,5:7]
    X.out <- datasets[[i]]$teste[ ,1:4]
    Y.out <- datasets[[i]]$teste[ ,5:7]
    
    modelo <- rbf(X,Y, K, gama)
    
    pred <- rbf.predict(modelo, X.out, classification = TRUE)
    
    reais <- reais(datasets[[i]]$teste)
    preditos <- rbf.preditos(pred)
    
    acc[[i]] <- acuracia(reais, preditos)
    accMedia <- accMedia + acuracia(reais, preditos)
    m[[i]] <- matriz.confusao(reais, preditos)
  }
  
  rbf$accMedia <- accMedia/length(datasets)
  rbf$acc <- acc
  rbf$matrizes <- m 
  
  return(rbf)
  
}

rbf.predict <- function(modelo, X, classification=FALSE) {
  
  gama <- modelo$gama
  centros <- modelo$centros
  w <- modelo$pesos
  N <- dim(X)[1] # numero de observacoes
  
  pred <- matrix(data = c(rep(w[1,1],N),rep(w[1,2],N),rep(w[1,3],N)), nrow = N, ncol = 3)
  # inicia com o peso do bias ja que a entrada associada eh 1  
  
  
  for (j in 1:N) {
    # Predicao para o ponto xj
    for (k in 1:length(centros[,1])) {
      # o peso para o centro[k] é dado por w[k+1] porque w[1] eh o bias
      
      for(c in 1:3){
        pred[j,c] <- pred[j,c] + w[k+1,c] * exp( (-1/(2*gama*gama)) *
                                             sum((X[j,]-centros[k,])*(X[j,]-centros[k,])) )
      }
      #pred[j]<-pred[j]+w[k+1]*exp(-gama*sum((X[j,]-centros[k,])*(X[j,]-centros[k,])))
      #pred[j]<-pred[j]+w[k+1]*exp(-gama*norm(as.matrix(X[j,]-centros[k,]),"F")^2)
    }
  }
  # Se for classificacao, aplica a funcao sinal em cada pred
  if (classification) {
    for(i in 1:N){
      if(maxPosicaoVetor(pred[i,]) == 1){
        pred[i,] <- c(1,0,0)
      }else if (maxPosicaoVetor(pred[i,]) == 2){
        pred[i,] <- c(0,1,0)
      }else{
        pred[i,] <- c(0,0,1)
      }
    }
  }
  return(pred)
}

# Treina o modelo
#modelo <- rbf(X, Y) # using default values for K and gamma