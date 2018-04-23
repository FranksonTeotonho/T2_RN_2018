##########################################################
# Implementacao da Multi-layer Perceptron #
# Baseado nas aula do professor Rodrigo Mello - ICMC/USP #
# https://www.youtube.com/watch?v=FSvD2HT0Zfg #
# Ricardo Cerri #
# 27/03/2018 #
#
# Trabalho 1 - Alteração para 3 neuronios de saida #
# Aluno: Frankson Teotonho de Sousa RA:619540 #
# Aluno: Eduardo Yamauchi RA:619485 #
##########################################################

source("PreProcessamento.R")

# Funcao de ativacao
funcao.ativacao <- function(v){
  y <- 1 / (1 + exp(-v))
  return(y)
}
# Derivada da funcao de ativacao
der.funcao.ativacao <- function(y){
  derivada <- y * (1 - y)
  return(derivada)
}

mlp.predicoes <- function(arq, dados){
  
  classesPreditas <- c()
  
  for(i in 1:nrow(dados)){
    resultado <- mlp.propagacao(arq, dados[i, 1:4])
    classes <- resultado$y.escondida.saida
    classe <- maxPosicaoVetor(classes)
    
    classesPreditas <- c(classesPreditas, classe)
  }
  
  return(classesPreditas)
  
}
#Usa o dataset configurando com o conceito de crossValidation para treinar o modelo
mlp.crossValidation <- function(arq, datasets, n, limiar){
  
  mlp <- list()
  m <- list()
  acc <- list()
  accMedia <- 0
  
  for(i in 1:length(datasets)){
    cat("fold: ", i ,"\n")
    modelo <- mlp.retropropagacao(arq, datasets[[i]]$treinamento,n, limiar)
    
    reais <- reais(datasets[[i]]$teste)
    preditos <- mlp.predicoes(modelo$arq, datasets[[i]]$teste)
    
    acc[[i]] <- acuracia(reais, preditos)
    accMedia <- accMedia + acuracia(reais, preditos)
    m[[i]] <- matriz.confusao(reais, preditos) 
    arq <- modelo$arq
  }
  
  mlp$arq <- arq
  mlp$accMedia <- accMedia/length(datasets)
  mlp$acc <- acc
  mlp$matrizes <- m 
  
  return(mlp)
}

# Cria a arquitetura da MLP
arquitetura <- function(num.entrada, num.escondida, num.saida,
                        funcao.ativacao, der.funcao.ativacao){
  arq <- list()
  
  # Parametros da rede
  arq$num.entrada <- num.entrada
  arq$num.escondida <- num.escondida
  arq$num.saida <- num.saida
  arq$funcao.ativacao <- funcao.ativacao
  arq$der.funcao.ativacao <- der.funcao.ativacao
  
  # 4 neuronios na camada escondida
  # 4 entradas
  #
  # Ent1 Ent2 Ent3 Ent4 Bias
  #n1 w11 w12 w13  w14  w15
  #n2 w21 w22 w23  w24  w25
  #n3 w31 w32 w33  w34  w35
  #n4 w41 w42 w43  w44  w45
  
  # 4 neuronios na camada escondida
  # 3 neuronios de saida
  #
  #       n1   n2  n3  n4   Bias
  #saida1 w11 w12 w13  w14  w15
  #saida2 w21 w22 w23  w24  w25
  #saida3 w31 w32 w33  w34  w35
  
  # Pesos conectando entrada e escondida
  # Numero de pesos = 20
  num.pesos.entrada.escondida <- (num.entrada+1)*num.escondida
  arq$escondida <- matrix(runif(min=-0.5,max=0.5, num.pesos.entrada.escondida),
                          nrow=num.escondida, ncol=num.entrada+1)
  
  # Pesos conectando escondida e saida
  # Numero de pesos = 15
  num.pesos.escondida.saida <- (num.escondida+1)*num.saida
  arq$saida <- matrix(runif(min=-0.5,max=0.5, num.pesos.escondida.saida),
                      nrow=num.saida, ncol=num.escondida+1)
  return(arq)
}
# Fase de propagacao
mlp.propagacao <- function(arq, exemplo){
  
  # Entrada -> Cama Escondida
  v.entrada.escondida <- arq$escondida %*% as.numeric(c(exemplo,1))
  y.entrada.escondida <- arq$funcao.ativacao(v.entrada.escondida)
  
  # Camada Escondida -> Camada de Saida
  v.escondida.saida <- arq$saida %*% c(y.entrada.escondida,1)
  y.escondida.saida <- arq$funcao.ativacao(v.escondida.saida)
  
  # Resultados
  resultado <- list()
  resultado$v.entrada.escondida <- v.entrada.escondida
  resultado$y.entrada.escondida <- y.entrada.escondida
  resultado$v.escondida.saida <- v.escondida.saida
  resultado$y.escondida.saida <- y.escondida.saida
  
  return(resultado)
}
mlp.retropropagacao <- function(arq, dados, n, limiar){
  erroQuadratico <- 2 * limiar
  epocas <- 0
  # Treina eqto o erro quadratico for maior que um limiar
  while(erroQuadratico > limiar){
    erroQuadratico <- 0
    erroQuadratico1 <- 0
    erroQuadratico2 <- 0
    erroQuadratico3 <- 0
    
    # Treino para todos os exemplos (epoca)
    for(i in 1:nrow(dados)){
      # Pego um exemplo de entrada
      # Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
      x.entrada <- dados[i,1:arq$num.entrada]
      # Species
      x.saida <- dados[i,(ncol(dados)-2):ncol(dados)]
      
      # Pego a saida da rede para o exemplo
      # O resultado contém as saidas dos 3 neuronios de saida
      resultado <- mlp.propagacao(arq,x.entrada)
      
      
      #Obtendo as saidas da classe predita
      y1 <- resultado$y.escondida.saida[1]
      y2 <- resultado$y.escondida.saida[2]
      y3 <- resultado$y.escondida.saida[3]
      
      # Calculo dos erros de cada saida para o exemplo
      erro1 <- as.numeric(x.saida[1]) - y1
      erro2 <- as.numeric(x.saida[2]) - y2
      erro3 <- as.numeric(x.saida[3]) - y3
      
      # Soma erro quadratico
      erroQuadratico1 <- erroQuadratico1 + erro1*erro1
      erroQuadratico2 <- erroQuadratico2 + erro2*erro2
      erroQuadratico3 <- erroQuadratico3 + erro3*erro3
      
      erroQuadratico <- erroQuadratico1 + erroQuadratico2 + erroQuadratico3
      
      # Gradientes locais no neuronio de saida
      # erro * derivada da funcao de ativacao
      grad.local.saida1 <- erro1 * arq$der.funcao.ativacao(y1)
      grad.local.saida2 <- erro2 * arq$der.funcao.ativacao(y2)
      grad.local.saida3 <- erro3 * arq$der.funcao.ativacao(y3)
      
      grad.local.saida <- matrix(c(grad.local.saida1, grad.local.saida2, grad.local.saida3), nrow = 1, ncol = 3)
      
      # Gradiente local no neuronio escondido
      # derivada da funcao de ativacao no neuronio escondido * soma dos gradientes
      # locais dos neuronios conectados na proxima camada * pesos conectando a camada
      # escondida com a saida
      pesos.saida <- arq$saida[,1:arq$num.escondida]
      grad.local.escondida <-
        as.numeric(arq$der.funcao.ativacao(resultado$y.entrada.escondida)) *
        (grad.local.saida %*% pesos.saida)
      # Ajuste dos pesos
      # Saida
      arq$saida <- arq$saida + n * (t(grad.local.saida) %*%
                                      c(resultado$y.entrada.escondida,1))
      # Escondida
      
      arq$escondida <- arq$escondida + n * (t(grad.local.escondida) %*%
                                              as.numeric(c(x.entrada,1)))
    }
    erroQuadratico <- erroQuadratico / nrow(dados)
    cat("Erro Quadratico Medio = ",erroQuadratico, "\n")
    epocas <- epocas+1
  }
  retorno <- list()
  retorno$arq <- arq
  retorno$epocas <- epocas
  return(retorno)
}
# Chamadas para execucao
#dados <- dados.processados()
#dadosCV <- k.fold.crossValidation(k, dados)
#arq <- arquitetura(4,4,3,funcao.ativacao, der.funcao.ativacao)
#modeloCV <- mlp.crossValidation(arq,dadosCV,0.1,2e-2) #Tempo razoavel