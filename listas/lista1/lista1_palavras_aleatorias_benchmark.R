pacman::p_load(readr,tidyverse,stringr,future,foreach,doParallel,parallel,furrr)


##### Questao 1 ----

alfabeto = c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","x","y","z")


################################################################################
#Funcao original
start = Sys.time()
matches = c()
for (n in 1:10) {
  amostra = c()
  for (i in 1:50000) {
    amostra[i] = paste(sample(alfabeto, 5, replace = TRUE), collapse = "")
  }
  matches[n] = sum(dicionario_ptbr$palavras %in% amostra)
}
matches

finish = Sys.time()

cat("tempo = ", finish-start)

################################################################################
#Funcao com o loop de comparacao paralelizado
# Especificando o número de núcleos a serem usados
n_cores <- 12
cl <- makeCluster(n_cores)

# Registrando os núcleos para uso do foreach
registerDoParallel(cl)

start = Sys.time()
# Loop externo paralelizado
matches <- foreach(n = 1:10, .combine = "sum") %dopar% {
  amostra <- c()
  for (i in 1:50000) {
    amostra[i] <- paste(sample(alfabeto, 5, replace = TRUE), collapse = "")
  }
  print(length(amostra))
  sum(dicionario_ptbr$palavras %in% amostra)
}

finish = Sys.time()

cat("tempo = ", finish-start)

# Fechando a conexão com os núcleos
stopCluster(cl)

# Imprime o número total de correspondências encontradas
cat("Total de correspondências encontradas em paralelo:", matches, "\n")
################################################################################
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
  correspondencias <- sum(lista_palavras %in% amostra)
  # Retorna o número total de correspondências encontradas em todas as amostras
  return(sum(correspondencias))
}


# Cria uma lista de palavras de exemplo
palavras <- dicionario_ptbr$palavras

start = Sys.time()
# Chama a função para gerar 100 amostras aleatórias e verificar quantas correspondem à lista de palavras
correspondencias_total <- criar_palavras_aleatorias(palavras, 50000)
finish = Sys.time()

cat("tempo = ", finish-start)
# Imprime o número total de correspondências encontradas
cat("Total de correspondências encontradas:", correspondencias_total)

################################################################################
#testando a funcao em um loop (como na original) 

start = Sys.time()

for (n in 1:10) {
  criar_palavras_aleatorias(palavras, 50000) #criar e comparar kkkk
}

finish = Sys.time()

cat("tempo = ", finish-start)

################################################################################
#paralelizando o loop da funcao
# Especificando o número de núcleos a serem usados
n_cores <- 12
cl <- makeCluster(n_cores)

# Registrando os núcleos para uso do foreach
registerDoParallel(cl)

start = Sys.time()
# Loop externo paralelizado
matches <- foreach(n = 1:10, .combine = "sum") %dopar% {
  criar_palavras_aleatorias(palavras, 50000)
}

finish = Sys.time()

cat("tempo = ", finish-start)

# Fechando a conexão com os núcleos
stopCluster(cl)

# Imprime o número total de correspondências encontradas
cat("Total de correspondências encontradas em paralelo:", matches, "\n")

################################################################################
#paralelizando o loop de criacao de palavras aleatorias
criar_palavras_aleatorias_paralelo <- function(lista_palavras, n_amostras, n_cores) {
  # Cria um vetor com todas as letras do alfabeto
  alfabeto <- letters
  
  # Cria um cluster com o número de núcleos especificado
  cl <- makeCluster(n_cores)
  
  # Registra os núcleos para uso do foreach
  registerDoParallel(cl)
  
  # Loop paralelizado
  amostra <- foreach(i = 1:n_amostras, .combine = "c") %dopar% {
    # Gera uma amostra aleatória de 5 letras do alfabeto
    palavra_aleatoria <- paste(sample(alfabeto, 5, replace = TRUE), collapse = "")
  }
  # Verifica se a palavra aleatória gerada está na lista de palavras
  resultados = sum(lista_palavras %in% amostra)
  
  # Fecha a conexão com os núcleos
  stopCluster(cl)
  
  # Retorna o número total de correspondências encontradas em todas as amostras
  return(sum(resultados))
}

# Cria uma lista de palavras de exemplo
palavras <- dicionario_ptbr$palavras

start = Sys.time()
# Chama a função para gerar 100 amostras aleatórias e verificar quantas correspondem à lista de palavras
correspondencias_total <- criar_palavras_aleatorias_paralelo(palavras, 50000, 12)
finish = Sys.time()

cat("tempo = ", finish-start)
# Imprime o número total de correspondências encontradas
cat("Total de correspondências encontradas:", correspondencias_total)

################################################################################
#testando a funcao com a craicao de palavras paralelizada em um loop (como na original) 

start = Sys.time()

for (n in 1:10) {
  criar_palavras_aleatorias_paralelo(palavras, 50000, 12) #criar e comparar kkkk
}

finish = Sys.time()

cat("tempo = ", finish-start)



























