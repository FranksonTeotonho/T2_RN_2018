#SOM
source("PreProcessamento.R")

# Distancia Euclidiana
euc.dist <- function(x, y){
  distancia <- sqrt(sum((x - y)^2))
}

#Programa principal
som <- function(dados){
  
  som <- list()
  
  num.variaveis <- ncol(dados[,1:4])
  
  # Parametros do algoritmo
  n0 = 0.1 # taxa de aprendizado inicial
  fi0 = 8 # largura inicial da vizinhanca
  tal1 = 1000/log(fi0) # constante para a variacao da largura
  tal2 = 1000 # constante para a variacao da taxa de aprendizado
  iteracoes = 30 # numero maximo de iteracoes
  iteracoes.s = 5 # a cada iteracoes.s salvar a estado do mapa
  dimensao = 12 # dimensao do mapa
  
  
  # Mapa
  mapa <- matrix(ncol=dimensao ,nrow=dimensao, data=0)
  
  # Array tri-dimensional para armazenar os pesos
  # [linha,pesos,coluna]
  pesos <- array(runif(dimensao*num.variaveis*dimensao,min=-1,max=1),
                 dim=c(dimensao,num.variaveis,dimensao))
  fi <- fi0
  n <- n0
  
  # Inicio da iteracao
  for(it in 1:iteracoes){
    
    # Mistura os dados
    nr <- dim(dados)[1]
    dados <- dados[sample.int(nr),]
    dados.entrada <- dados[,1:4]
    dados.saida <- dados[,5]
    
    # Itera para cada exemplo
    for(i in 1:nrow(dados.entrada)){
     
      x <- dados.entrada[i,]
      # Para cada vetor de pesos dos neuronios
      min_j <- 1
      min_k <- 1
      min_dist <- 99999 
      
      for(j in 1:dimensao){
        
        # Pesos dos primeiros dimensao neuronios
        tpesos <- t(pesos[j,,])
        colnames(tpesos) <- colnames(x)
        matriz <- rbind(x,tpesos)
        matriz.distancias <- dist(matriz,method = "euclidean")
        # matriz.distancias eh tipo dist. As primeiras "dimensao" posicoes contem
        # as distancias entre x e os neuronios
        distancias <- sort(matriz.distancias[1:dimensao],index.return=TRUE)
        
        for(k in 1:dimensao){
          
          distancia <- distancias$x[k]
          # Guarda o neuronio com a menor distancia
          # Nao vamos mapear mais de um exemplo para o mesmo neuronio
          if(distancia < min_dist){
            if(mapa[j,distancias$ix[k]] == 0){
              min_dist <- distancia
              min_j <- j
              min_k <- distancias$ix[k]
              break
            }
          }
        }
        
      }
      
      #cat(i," ")
      # Marca o neuronio mais proximo
      mapa[min_j,min_k] <- dados.saida[i]
      # Atualizacao dos pesos
      # wj(it+1) = wj(it) + n(it)*hji(x)(it)*[x(it)-wj(it)]
      
      for(j in 1:dimensao){
        for(k in 1:dimensao){
          # Distancia entre neuronio vencedor e neuronio [j,k] no mapa
          #distancia <- dist(rbind(c(min_j,min_k),c(j,k)), method = "euclidean")
          #distancia <- as.numeric(distancia)
          distancia <- euc.dist(c(min_j,min_k),c(j,k))
          # Funcao Gaussiana
          vizinhanca = exp(-distancia / (2*fi*fi))
          # Atualiza os pesos
          w <- pesos[j,,k]
          w <- w + n * vizinhanca * (x - w)
          pesos[j,,k] <- unlist(w)
        }
        
        
      }
      
    }
    
    # Atualiza fi e n
    fi <- fi0 * exp(-it / tal1)
    n <- n0 * exp(-it / tal2)
    # Mapa inicial
    if(it == 1){
      cat("\nMapa inicial\n")
      print(mapa)
      cat("\n")
    }
    
    cat(it," ")
    # Mostra o mapa
    if((it %% iteracoes.s) == 0){
      cat("\nIteracao: ",it,"\n")
      print(mapa)
      cat("\n")
      #write.table(mapa,file=paste("Kohonen/mapa_it",it,".txt", sep=""), row.names=FALSE)
    }
    
    if(it == iteracoes){
      som$mapaFinal <- mapa
      som$pesos <- pesos
      som$dimensao <- dimensao
    }
    
    # Reinicia o mapa
    mapa <- matrix(ncol=dimensao ,nrow=dimensao, data=0)
  }
  
  return(som)
}

som.predict <- function(modelo, dadosTeste){
  
  mapaFinal <- modelo$mapaFinal
  pesos <- modelo$pesos
  dimensao <- modelo$dimensao
  pred <- c()
  
  for(i in 1:nrow(dadosTeste)){
    dmin <- 99999
    linha <- 1
    coluna <- 1
    for(j in 1:dimensao){
      for(k in 1:dimensao){
        w <- pesos[j, ,k]
        
        d <- euc.dist(as.numeric(dadosTeste[i, 1:4]), as.numeric(w))
        
        if(d < dmin){
          dmin <- d
          linha <- j
          coluna <- k
        }
      }
    }
    
    saidaMapa <- mapaFinal[linha, coluna]
    pred <- c(pred, saidaMapa)
    
  }
  
  return(pred)
  
}

som.crossValidation <- function(datasets){
  
  som <- list()
  acc <- list()
  m <- list()
  accMedia <- 0
  
  for(i in 1:length(datasets)){
    
    cat("Fold: ", i, "\n")
    
    dadosTreinamento <- datasets[[i]]$treinamento
  
    dadosTeste <- datasets[[i]]$teste
    
    modelo <- som(dadosTreinamento)
    
    pred <- som.predict(modelo, dadosTeste)
    reais <- reais(dadosTeste, FALSE)
    
    acc[[i]] <- acuracia(reais, pred)
    accMedia <- accMedia + acuracia(reais, pred)
    m[[i]] <- matriz.confusao(reais, pred)
  }
  
  som$accMedia <- accMedia/length(datasets)
  som$acc <- acc
  som$matrizes <- m 
  
  return(som)
  
}