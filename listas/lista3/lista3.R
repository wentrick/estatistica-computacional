pacman::p_load(tidyverse,MASS,e1071,mlbench,caret)
set.seed(321)  # Semente para reprodutibilidade dos resultados
#### Questao 1 ----

# Media e matriz de covariancia
mu <- c(0, 1, 2)  # Vetor médio com os valores médios para cada variável
Sigma <- matrix(c(1.0, -0.5, 0.5,
                  -0.5, 1.0, -0.5,
                  0.5, -0.5, 1.0), nrow = 3)  # Matriz de covariância

# Gerando observacoes aleatorias!
n <- 1000  # Número de observações a serem geradas


rmvn.cholesky <-  # Definição da função para gerar observações aleatórias
  function(n, mu, Sigma) {
    p <- length(mu)  # Número de variáveis
    Q <- chol(Sigma)  # Decomposição de Cholesky da matriz de covariância
    Z <- matrix(rnorm(n*p), nrow=n, ncol=p)  # Amostras aleatórias da distribuição normal padrão
    X <- Z %*% Q + matrix(mu, n, p, byrow=TRUE)  # Transformação das amostras para a distribuição desejada
    X  # Retorna as observações aleatórias
  }

a3 <- rmvn.cholesky(1000, mu, Sigma)  # Geração das observações aleatórias

plot(a3, xlab = "x", ylab = "y", pch = 20)  # Gráfico de dispersão das observações geradas

print(cor(a3))  # Cálculo e impressão da matriz de correlação das observações

pairs(a3, main = "Gráfico de Dispersao",
      pch = 19, col = "blue")  # Matriz de gráficos de dispersão das observações geradas


#### Questao 2 ----

# Set the number of iterations
N <- 100000

# funcao para estimar θ
funcao <- function(x) {
  return(exp(1)^x)
}

# Simple Monte Carlo method
simple_mc <- function(N) {
  u <- runif(N)  # Gerar N números aleatórios a partir de uma distribuição uniforme
  theta <- funcao(u)  # Estimar θ usando a função
  estimate <- mean(theta)  # Calcular a média de θ
  return(theta)
}

# Antithetic variate method
antithetic_mc <- function(N) {
  u <- runif(N/2)  # Gerar N/2 números aleatórios a partir de uma distribuição uniforme pq vamos estar gerando 2 ao mesmo tempo
  u_antithetic <- 1 - u  # Gerar as variáveis antitéticas
  u_combined <- c(u, u_antithetic)  # Combinar as variáveis
  theta <- funcao(u_combined)  # Estimar θ usando a função
  estimate <- mean(theta)  # Calcular a média de θ
  return(theta)
}

N=500000

# Estimar θ usando o método simples de Monte Carlo
estimate_simple_mc <- simple_mc(N)

hist(estimate_simple_mc)

theta_simple = mean(estimate_simple_mc)
# Estimar θ usando o método 'antithetic variate'
estimate_antithetic_mc <- antithetic_mc(N)

hist(estimate_antithetic_mc)

theta_antithetic = mean(estimate_simple_mc)

# Valor real
valor_real = as.numeric(integrate(funcao, 0, 1)[1])

# Imprimir os resultados
print(paste("θ estimado usando o método simples de Monte Carlo:", round(theta_simple,4)))  # resultado mais longe do valor real
print(paste("θ estimado usando o método 'antithetic variate':", round(theta_antithetic,4)))  # resultado mais próximo do valor real
print(paste("Valor real de θ:", round(valor_real,4)))

#plot

resultado = data.frame(simple = estimate_simple_mc,antithetic = estimate_antithetic_mc) %>%
  pivot_longer(c(simple,antithetic),values_to = "result",names_to = "simulation") %>%
  mutate(simulation = as.factor(simulation))

# Box plot by group
ggplot(resultado, aes(x = simulation, y = result)) + 
  geom_boxplot() +
  labs(x = "Simulation", y = "Result") +
  ggtitle("Boxplot por Tipo de Simulação")

#### Questao 3 ----

# Carregar o conjunto de dados "Glass" do pacote "mlbench"
data(Glass, package = "mlbench")

# Dividir o conjunto de dados em conjunto de treinamento e teste
index <- 1:nrow(Glass)
N <- trunc(length(index) / 3)
set.seed(123)
testindex <- sample(index, N)
testset <- Glass[testindex, ]
trainset <- Glass[-testindex, ]

# Treinar um modelo SVM
svm.model <- svm(Type ~ ., data = trainset, cost = 100, gamma = 0.1)
svm.model

# Fazer previsões nos dados de teste
svm.pred <- predict(svm.model, testset[, -10])
svm.pred

# Calcular a matriz de confusão do SVM
matriz_confusao <- table(pred = svm.pred, true = testset[, 10])
matriz_confusao <- as.matrix(matriz_confusao)

# Definir a lista de valores de gamma para avaliar
gammas <- c(0.01, 0.1, 1)

# Executar validação cruzada com k-fold
folds <- createFolds(seq(1:6), k = 10)

# Inicializar vetor para armazenar os erros
erros <- numeric()

# Loop sobre os valores de gamma
for (gamma in gammas) {
  erro1 <- numeric()  # Variável para armazenar o erro para cada fold
  erro2 <- numeric()  # Variável para armazenar o erro para cada fold
  erro3 <- numeric()  # Variável para armazenar o erro para cada fold
  
  # Loop sobre os folds
  for (i in 1:length(folds)) {
    # Dividir os dados em conjunto de treinamento e teste para o fold atual
    train_data <- matriz_confusao[-folds[[i]], ]
    test_data <- t(as.matrix(matriz_confusao[folds[[i]], ]))
    
    # Extrair as variáveis de entrada (X) e de saída (Y) dos dados de treinamento e teste
    train_X <- train_data[, -ncol(train_data)]
    train_Y <- as.vector(train_data[, ncol(train_data)])
    test_X <- as.vector(test_data[, -ncol(test_data)])
    test_Y <- as.vector(test_data[, ncol(test_data)])
    
    # Treinar o modelo SVM com o valor de gamma atual
    modelo1 <- svm(Type ~ ., data = trainset, cost = 100, gamma = gammas[1])
    modelo2 <- svm(Type ~ ., data = trainset, cost = 100, gamma = gammas[2])
    modelo3 <- svm(Type ~ ., data = trainset, cost = 100, gamma = gammas[3])
    
    # Fazer previsões nos dados de teste
    predicao1 <- predict(modelo1)
    predicao2 <- predict(modelo2)
    predicao3 <- predict(modelo3)
    
    # Calcular a taxa de erro no fold atual
    erro1[i] <- sum(predicao1 != test_Y) / length(test_Y)
    erro2[i] <- sum(predicao2 != test_Y) / length(test_Y)
    erro3[i] <- sum(predicao3 != test_Y) / length(test_Y)
    
  }
  
  # Calcular o erro médio para o valor de gamma atual
  avg_erro <- c(mean(erro1), mean(erro2), mean(erro3))
  erros <- c(erros, avg_erro)
}

# Encontrar o valor de gamma com o menor erro
melhor_gamma <- gammas[which.min(erros)]
melhor_erro <- min(erros)

# Imprimir os resultados
cat("Melhor valor de gamma:", melhor_gamma, "\n")
cat("Erro de teste correspondente:", melhor_erro, "\n")

# Mudar os valores de gamma para uma nova lista
gammas <- c(0.0001, 0.001, 0.01)



#### Questao 4 ----

### 9.3)

# Criar o vetor de valores de x
x <- seq(-10, 10, length.out = 1000)

# Definir a função densidade da distribuição Cauchy
densidade_cauchy <- function(x, locacao, escala) {
  return(1 / (pi * escala * (1 + ((x - locacao) / escala)^2)))
}

# Definir os parâmetros da distribuição Cauchy
locacao <- 0  # Parâmetro de localização
escala <- 1  # Parâmetro de escala = teta

# Número de iterações
N <- 10000

# Número de amostras descartadas
descarte <- 1000

# Iniciar a corrente
chain <- numeric(N + descarte)
chain[1] <- 0  # Valor inicial para a corrente

# Distribuição proposta inicialmente (e.g., distribuição normal)
dp_prop <- 1  # Desvio padrão da distribuição proposta inicialmente

# Definir a densidade alvo (Cauchy padrão)
target_density <- function(x) {
  return(1 / (pi * (1 + x^2)))
}

# Algoritmo de Metropolis-Hastings 
for (i in 2:(N + descarte)) {
  # Gerar uma amostra candidata da distribuição proposta
  candidato <- rnorm(1, mean = chain[i - 1], sd = dp_prop)
  
  # Calcular a taxa de aceitação
  acceptance_ratio <- target_density(candidato) / target_density(chain[i - 1])
  
  # Aceitar ou rejeitar a amostra candidata
  if (runif(1) < acceptance_ratio) {
    chain[i] <- candidato  # Aceitar o candidato
  } else {
    chain[i] <- chain[i - 1]  # Rejeitar o candidato e manter o valor anterior
  }
}

# Descartar as amostras
chain <- chain[(descarte + 1):(N + descarte)]

# Calcular os quantis gerados
decis_gerado <- quantile(chain, probs = seq(0.1, 0.9, by = 0.1))

# Calcular os quantis da distribuição Cauchy
decis_cauchy <- qcauchy(seq(0.1, 0.9, by = 0.1))

# Imprimir os valores dos quantis
print("Decis das observações geradas:")
print(decis_gerado)

print("Decis da Cauchy padrão:")
print(decis_cauchy)

# Plotar a função densidade da distribuição Cauchy
curve(densidade_cauchy(x, locacao, escala), from = -10, to = 10,
      xlab = "x", ylab = "Densidade", main = "Função densidade da distribuição Cauchy")

# Plotar a densidade das amostras geradas
plot(density(chain), main = "Simulação", xlab = "x", ylab = "Densidade", col = "blue",
     xlim = c(-10, 10))

# Calcular os valores da função densidade para cada valor de x
densidades <- densidade_cauchy(x, locacao, escala)

# Plotar a função densidade da distribuição Cauchy e os valores gerados
plot(x, densidades, type = "l",
     xlab = "x", ylab = "Densidade",
     main = "Função densidade da distribuição Cauchy e dos valores gerados")
lines(density(chain), col = "blue")
legend(legend = c("Gerada", "Cauchy Padrão"),
       col = c("blue", "black"), lty = 1, pch = 1, cex = 0.8, x = 2.3, y = 0.3)



### 9.7)

# Definir o número de iterações
N <- 1000

# Definir o coeficiente de correlação
rho <- 0.9

# Inicializar as cadeias
X <- numeric(N)
Y <- numeric(N)

# Definir os valores iniciais para X e Y
X[1] <- 0
Y[1] <- 0

# Executar o amostrador Gibbs
for (t in 2:N) {
  # Amostrar X[t] dado Y[t-1]
  X[t] <- rnorm(1, mean = rho * Y[t - 1], sd = sqrt(1 - rho^2))
  
  # Amostrar Y[t] dado X[t]
  Y[t] <- rnorm(1, mean = rho * X[t], sd = sqrt(1 - rho^2))
}

# Definir o comprimento do burn-in
burn_in <- 100

# Descartar as amostras do burn-in
X_discarded <- X[(burn_in + 1):N]
Y_discarded <- Y[(burn_in + 1):N]

# Plotar a cadeia bivariada normal após o burn-in
plot(X_discarded, Y_discarded, type = "l", xlab = "X", ylab = "Y", main = "Cadeia Bivariada Normal (Após o Burn-In)")

# Aplicar a Regressão Linear

# Criar um data frame com as amostras descartadas
Banco <- data.frame(Y = Y_discarded, X = X_discarded)

# Passo 2: Criar o modelo linear
modelo <- lm(Y ~ X, data = Banco)

# Passo 3: Verificar a normalidade dos resíduos usando quantis
# Q-Q plot
qqnorm(modelo$residuals)
qqline(modelo$residuals)

# Teste de Shapiro-Wilk
shapiro.test(modelo$residuals)  # normal

# Passo 4: Verificar os resíduos para avaliar a variância constante
# Plot dos resíduos em relação aos valores preditos
plot(modelo$fitted.values, modelo$residuals,
     xlab = "Valores Preditos", ylab = "Resíduos")
abline(h = 0, col = "red", lty = 2)
# Parece haver uma dispersão satisfatória dos dados

# Gráfico de dispersão
plot(X, Y, main = "Regressão Linear", xlab = "X", ylab = "Y")

# Linha de regressão
abline(modelo, col = "red")













