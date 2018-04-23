library(corpcor)

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

rbf.predict <- function(modelo, X, classification=FALSE) {
  
  gama <- modelo$gama
  centros <- modelo$centros
  w <- modelo$pesos
  N <- dim(X)[1] # numero de observacoes
  
  pred <- rep(w[1],N)
  # inicia com o peso do bias ja que a entrada associada eh 1
  for (j in 1:N) {
    # Predicao para o ponto xj
    for (k in 1:length(centros[,1])) {
      # o peso para o centro[k] é dado por w[k+1] porque w[1] eh o bias
      pred[j] <- pred[j] + w[k+1] * exp( (-1/(2*gama*gama)) *
                                           sum((X[j,]-centros[k,])*(X[j,]-centros[k,])) )
      #pred[j]<-pred[j]+w[k+1]*exp(-gama*sum((X[j,]-centros[k,])*(X[j,]-centros[k,])))
      #pred[j]<-pred[j]+w[k+1]*exp(-gama*norm(as.matrix(X[j,]-centros[k,]),"F")^2)
    }
  }
  # Se for classificacao, aplica a funcao sinal em cada pred
  if (classification) {
    pred <- unlist(lapply(pred, sign))
  }
  return(pred)
}

# Treina o modelo
#modelo <- rbf(X, Y) # using default values for K and gamma