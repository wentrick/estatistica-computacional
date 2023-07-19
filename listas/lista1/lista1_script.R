pacman::p_load(readr,tidyverse,stringr,future,foreach,doParallel,parallel)
options(scipen = 999, digits = 12)
#set.seed(050701)
library(dplyr)
##### Questao 1A ----
dicionario_ptbr <- as.data.frame(read_csv("dados/Dicionario.txt"))
colnames(dicionario_ptbr) = c("palavras")

dicionario_ptbr = dicionario_ptbr %>%
  mutate(tamanho = nchar(palavras)) %>%
  filter(tamanho == 5)
  tolower(dicionario_ptbr$palavras)

#criando uma funcao para gerar as palavras e comparar elas
criar_palavras_aleatorias <- function(lista_palavras, n_amostras) {
  # Cria um vetor com todas as letras do alfabeto
  alfabeto <- letters
  
  # Inicializa um vetor para armazenar o número de correspondências em cada amostra
  correspondencias <- numeric(n_amostras)
  amostra = c()
  for (i in seq_len(n_amostras)) {
    # Gera uma amostra aleatória de 5 letras do alfabeto
    amostra[i] <- paste(sample(alfabeto, 5, replace = TRUE), collapse = "")
  }
  
  # Verifica se a palavra aleatória gerada está na lista de palavras
  correspondencias <- lista_palavras %in% amostra
  # Retorna o número total de correspondências encontradas em todas as amostras
  return(sum(correspondencias))
}


# Cria uma lista de palavras de exemplo
palavras <- dicionario_ptbr$palavras

start = Sys.time()
# Chama a função para gerar 100 amostras aleatórias e verificar quantas correspondem à lista de palavras
correspondencias_total <- criar_palavras_aleatorias(palavras, 300000)
finish = Sys.time()

cat("tempo =", finish-start)
# Imprime o número total de correspondências encontradas
cat("Total de correspondências encontradas:", correspondencias_total/300000)

###############################################################################
#paralelizando o loop da funcao
# Especificando o número de núcleos a serem usados
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
x = 100000 #numero de palvras geradas
y = 100 #numero de loops dessa simulacao
# Registrando os núcleos para uso do foreach
registerDoParallel(cl)

start = Sys.time()
# Loop externo paralelizado
matches <- foreach(n = 1:y, .combine = "mean") %dopar% {
  criar_palavras_aleatorias(palavras, x)
}

finish = Sys.time()

cat("tempo =", finish-start)

# Fechando a conexão com os núcleos
stopCluster(cl)

# Imprime o número total de correspondências encontradas
cat("A probabilidade estimada a partir de uma media da quantiade de palvras validas encontradas de",y,"simulacoes gerando",x,"palvras aleatorias cada é:", matches/x, "\n")
cat("A probabilidade exata de se gerar uma palvra aleatoria de 5 letras e ela existir é:",5481/26^5)

##### Questao 1B ----

criar_palavras_aleatorias_palindromas <- function(n_amostras) {
  # Cria um vetor com todas as letras do alfabeto
  alfabeto <- letters
  
  # Inicializa um vetor para armazenar o número de palavras palíndromas geradas em cada amostra
  palindromos <- numeric(n_amostras)
  
  for (i in seq_len(n_amostras)) {
    # Gera uma amostra aleatória de 5 letras do alfabeto
    palavra_quebrada <- sample(alfabeto, 5, replace = TRUE)
    palavra = paste(palavra_quebrada, collapse = "")
    palavra_reversa = paste(rev(palavra_quebrada), collapse = "")
    
    # Verifica se a palavra gerada é um palíndromo
    if (palavra == palavra_reversa) {
      palindromos[i] <- 1
    } else {
      palindromos[i] <- 0
    }
  }
  
  # Retorna a proporção de palavras palíndromas geradas em todas as amostras
  return(mean(palindromos))
}

# Chama a função para gerar 100000 amostras aleatórias e estimar a probabilidade de gerar uma palavra palíndroma
prob_palindromo <- criar_palavras_aleatorias_palindromas(100000)
cat("Probabilidade de gerar uma palavra palíndroma:", prob_palindromo)


##### Questao 1C ----

criar_palavras_aleatorias_vogal_consoante <- function(lista_palavras, n_amostras) {
  # Cria um vetor com todas as letras do alfabeto
  vogais <- c("a", "e", "i", "o", "u")
  consoantes <- c("b", "c", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "w", "x", "y", "z")
  
  # Inicializa um vetor para armazenar o número de correspondências em cada amostra
  correspondencias <- numeric(n_amostras)
  amostra = c()
  for (i in seq_len(n_amostras)) {
    # Gera uma amostra aleatória de 5 letras do alfabeto
    palavra <- character(5) # inicializa um vetor vazio com 10 espaços
    for (n in 1:5) {
      if (n %% 2 == 0) { # se o índice for par, sorteia uma consoante (eu nao levei em consideracao a opcao de ser indice impar, ou seja começar com uma voga ou cmeçar com uma consoante)
        letra <- sample(consoantes, 1)
      } else { # senão, sorteia uma vogal
        letra <- sample(vogais, 1)
      }
      palavra[n] <- letra # adiciona a letra à palavra
    }
    amostra[i] = paste(palavra, collapse = "")
  }
  
  # Verifica se a palavra aleatória gerada está na lista de palavras
  correspondencias <- lista_palavras %in% amostra
  # Retorna o número total de correspondências encontradas em todas as amostras
  return(sum(correspondencias))
}

# Cria uma lista de palavras de exemplo
palavras <- dicionario_ptbr$palavras

start = Sys.time()
# Chama a função para gerar 100 amostras aleatórias e verificar quantas correspondem à lista de palavras
correspondencias_total <- criar_palavras_aleatorias_vogal_consoante(palavras, 300000)
finish = Sys.time()

cat("tempo =", finish-start)
# Imprime o número total de correspondências encontradas
cat("Total de correspondências encontradas:",correspondencias_total,"a probabilidade estimada é:", correspondencias_total/300000)

##### Questao 1D ----

criar_palavras_aleatorias <- function(lista_palavras, n_amostras) {
  # Cria um vetor com todas as letras do alfabeto
  alfabeto <- letters
  probabilidade_letras = c(0.1463, 0.0104, 0.0388, 0.0499, 0.1257, 0.0102, 0.013, 
                           0.0128, 0.0618, 0.004, 0.0002, 0.0278, 0.0474, 0.0505, 
                           0.1073, 0.0252, 0.012, 0.0653, 0.0781, 0.0434, 0.0463, 
                           0.0167, 0.0001, 0.0021, 0.0001, 0.0047)
  # Inicializa um vetor para armazenar o número de correspondências em cada amostra
  correspondencias <- numeric(n_amostras)
  amostra = c()
  for (i in seq_len(n_amostras)) {
    # Gera uma amostra aleatória de 5 letras do alfabeto
    amostra[i] <- paste(sample(alfabeto, 5, replace = TRUE,prob = probabilidade_letras), collapse = "")
  }
  
  # Verifica se a palavra aleatória gerada está na lista de palavras
  correspondencias <- lista_palavras %in% amostra
  # Retorna o número total de correspondências encontradas em todas as amostras
  return(sum(correspondencias))
}


# Cria uma lista de palavras de exemplo
palavras <- dicionario_ptbr$palavras

start = Sys.time()
# Chama a função para gerar 100 amostras aleatórias e verificar quantas correspondem à lista de palavras
correspondencias_total <- criar_palavras_aleatorias(palavras, 300000)
finish = Sys.time()

cat("tempo =", finish-start)
# Imprime o número total de correspondências encontradas
cat("Total de correspondências encontradas:", correspondencias_total/300000)

#o objetivo era pra fazer um loop while enquanto 

##### Questao 2A ----

cauchy_sample <- function(n, gamma, x0=0) {
  # gerar n variáveis aleatórias uniformes no intervalo [0,1]
  u <- runif(n)
  # calcular as variáveis aleatórias da distribuição Cauchy usando a transformada integral inversa
  x <- x0 + gamma * tan(pi * (u - 1/2))
  return(x)
}


x <- cauchy_sample(1000, 1, 0)


hist(x,xlim = c(-10, 10), breaks = seq(-1000, 1000, 0.4))
##### Questao 2B ----
  

icdf <- function(u) {
  ifelse(u <= 0.2, 2,
         ifelse(u <= 0.3, 3,
                ifelse(u <= 0.5, 5,
                       ifelse(u <= 0.7, 7, 9))))
}

# Gerar 1000 amostras aleatórias
u <- runif(1000)

# Aplicar a ICDF para obter as amostras aleatórias
x <- icdf(u)


tabela_emp <- table(x) / 1000
tabela_teo <- c(0.2, 0.1, 0.2, 0.2, 0.3)
names(tabela_emp) <- c(2, 3, 5, 7, 9)
names(tabela_teo) <- c(2, 3, 5, 7, 9)

#comparando com a funcao sample do R 
X2 <- sample(c(2, 3, 5, 7, 9), 1000, prob = c(0.2, 0.1, 0.2, 0.2, 0.3),replace = TRUE)

tabela_emp2 <- table(X2)/1000

rbind(tabela_teo,tabela_emp,tabela_emp2)

##### Questao 2C ----
gerar_amostra_normal_aceitacao_rejeicao <- function(n) {
  f <- function(x) dnorm(x)
  g <- function(x) dcauchy(x)
  c <- sqrt(2/exp(1))
  
  amostras <- numeric(n)
  i <- 1
  
  while (i <= n) {
    x <- rcauchy(1)
    u <- runif(1)
    
    if (u <= f(x)/(c * g(x))) {
      amostras[i] <- x
      i <- i + 1
    }
  }
  
  return(amostras)
}

amostras <- gerar_amostra_normal_aceitacao_rejeicao(10000)

hist(amostras)


