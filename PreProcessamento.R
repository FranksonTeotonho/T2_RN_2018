#Transformação de classes nominais para vetores com 3 algarismo
#Normalização
#Randomizar
dados.processados <- function(){
  #Classes nominais para numericas
  dados <- iris[1:4]
  classes <- factor(iris$Species,
                         levels = c('setosa', 'versicolor', 'virginica' ),
                         labels = c(1, 2, 3))
  
  
  for(i in 1:length(classes)){
    
    if(classes[i] == 1){
      dados$Species1[i] <- 1
      dados$Species2[i] <- 0
      dados$Species3[i] <- 0
    }else if (classes[i] == 2){
      dados$Species1[i] <- 0
      dados$Species2[i] <- 1
      dados$Species3[i] <- 0
    }else{
      dados$Species1[i] <- 0
      dados$Species2[i] <- 0
      dados$Species3[i] <- 1
    }
  }

  #Normalizando os atributos
  dados[ ,1:4] <- scale(dados[ ,1:4])
  #Randomizando
  
  dados <- dados[sample(150,150), ]
  return(dados)
}

#Matriz de confusão
matriz.confusao <- function(reais, preditos){
  
  m11 <- 0
  m12 <- 0
  m13 <- 0
  m21 <- 0
  m22 <- 0
  m23 <- 0
  m31 <- 0
  m32 <- 0
  m33 <- 0
  
  for(i in 1:length(reais)){
    if((reais[i] == 1) && (preditos[i] == 1)){m11 <- m11 + 1}
    else if((reais[i] == 1) && (preditos[i] == 2)){m12 <- m12 + 1}
    else if((reais[i] == 1) && (preditos[i] == 3)){m13 <- m13 + 1}
    
    else if((reais[i] == 2) && (preditos[i] == 1)){m21 <- m21 + 1}
    else if((reais[i] == 2) && (preditos[i] == 2)){m22 <- m22 + 1}
    else if((reais[i] == 2) && (preditos[i] == 3)){m23 <- m23 + 1}
    
    else if((reais[i] == 3) && (preditos[i] == 1)){m31 <- m31 + 1}
    else if((reais[i] == 3) && (preditos[i] == 2)){m32 <- m32 + 1}
    else{m33 <- m33 + 1}
  } 
  
  matriz <- matrix(c(m11,m21,m31,m12,m22,m32,m13,m23,m33), 3, 3)
  
  return (matriz)
}

#retorna a posição maxima de um vetor qualquer
maxPosicaoVetor <- function(vetor){
  
  pos <-0
  
  if((vetor[1] > vetor[2])&&(vetor[1]>vetor[3])){
    pos <- 1
  }else if ((vetor[2] > vetor[1])&&(vetor[2]>vetor[3])){
    pos <- 2
  }else{
    pos <- 3
  }
  
  return (pos)
  
}
#Transforma o vetor de 3 algarismos e um numero conrrespondente a classe

reais <- function(dados){
  
  classesReais <- c()
  
  for(i in 1:nrow(dados)){
    classe <- maxPosicaoVetor(dados[i,5:7])
    classesReais <- c(classesReais, classe)
  }
  
  return (classesReais)
}

k.fold.crossover <- function(k, dados){
  
  kfold <- list()
  grupos <- c()
  
  for (i in 1:k) {
    grupos <- c(grupos, i)  
  }
  
  holder <- split(dados, grupos)
  
  for(i in 1:k){
    n <- list()
    
    n$teste <- holder[i]
    n$treinamento <- holder[-i]
    
    cat(i)
    kfold[[i]] <- n
  }
  return (kfold)
  
}
