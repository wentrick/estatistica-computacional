pacman::p_load(readr,tidyverse,stringr,future,foreach,doParallel,parallel)
options(scipen = 999, digits = 12)
library(dplyr)
##### Questao 1 ----
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
  correspondencias <- sum(lista_palavras %in% amostra)
  # Retorna o número total de correspondências encontradas em todas as amostras
  return(sum(correspondencias))
}


# Cria uma lista de palavras de exemplo
palavras <- dicionario_ptbr$palavras

start = Sys.time()
# Chama a função para gerar 100 amostras aleatórias e verificar quantas correspondem à lista de palavras
correspondencias_total <- criar_palavras_aleatorias(palavras, 300000)
finish = Sys.time()

cat("tempo = ", finish-start)
# Imprime o número total de correspondências encontradas
cat("Total de correspondências encontradas:", correspondencias_total/300000)

###############################################################################
#paralelizando o loop da funcao
# Especificando o número de núcleos a serem usados
n_cores <- 23
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

cat("tempo = ", finish-start)

# Fechando a conexão com os núcleos
stopCluster(cl)

# Imprime o número total de correspondências encontradas
cat("A probabilidade estimada a partir de uma media da quantiade de palvras validas encontradas de",y,"simulacoes gerando",x,"palvras aleatorias cada é:", matches/x, "\n")

































