# Carregando os pacotes necessários
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readr, stringr, purrr, broom, readxl, corrplot, lubridate) 
set.seed(420)
#Organizando o banco de dados
climbing <- read_csv("listas/lista2/climbing_statistics.csv")
weather <- read_csv("listas/lista2/Rainier_Weather.csv")
convert <-function(x) (x-32) * 5/9
shift <-function(x) x - mean(x)
dados <- inner_join(climbing, weather) %>%
  select(-matches("Percent|Battery")) %>% 
  filter(Attempted >= Succeeded) %>%
  mutate(`Temperature AVG`= convert(`Temperature AVG`),
         Cleaver = Route =="Disappointment Cleaver",
         Date = lubridate::mdy(Date)) %>%
  select(Date, Succeeded, everything()) %>%
  rename(Data = Date,
         Sucessos = Succeeded,
         Tentativas = Attempted,
         Temperatura =`Temperature AVG`,
         Umidade_relativa =`Relative Humidity AVG`,
         Velocidade_vento =`Wind Speed Daily AVG`,
         Direc_vento =`Wind Direction AVG`,
         Radiacao_solar =`Solare Radiation AVG`)

# Agrupando por dia e rota e desconsiderando a rota 
dados2 <- dados %>% group_by(Data,Route) %>%
  mutate(Sucessos = sum(Sucessos),
         Tentativas = sum(Tentativas),
         Cleaver = case_when(Cleaver == T ~ 1,
                             T ~ 0)) %>% 
  summarise_if(is.numeric,mean) %>% 
  filter(Route != "glacier only - no summit attempt")

#1)

# Número de simulações
m <- 10000

# Ajustando variável Rota
dados3 <- dados2 %>% mutate(Rota = case_when(Route == "Disappointment Cleaver"~"Disappointment Cleaver", T ~ "Demais")) %>% 
  select(Sucessos, Rota) %>% 
  arrange(Rota)

# Cálculando a diferença real entre as médias dos grupos

media_real = abs(as.numeric(diff(tapply(dados3$Sucessos,dados3$Rota,mean))))

x <- lapply(1:m, function(a) {
  dados_sinteticos <- dados3
  dados_sinteticos$Sucessos <- sample(dados3$Sucessos, replace = FALSE)
  media_calculada <- abs(as.numeric(diff(tapply(dados_sinteticos$Sucessos,dados_sinteticos$Rota,mean))))
  media_calculada
})

results <- data.frame(estatistica = unlist(x))

# Gráfico da distribuição da diferênça entre as médias e a posição da média real em relação a distribuição

ggplot(data = results, aes(x = estatistica)) +
  geom_density() +
  geom_vline(xintercept = media_real,
             color = "red") +
  xlim(0, 17) +
  xlab("Valor da estatística") +
  ylab("Densidade")

#2)

# Variáveis
intercepto <- 1
temperatura <- dados2$Temperatura
sucessos <- dados2$Sucessos

logistica <- function(eta) exp(eta)
# Função para estimação dos parâmetros
log_verossimilhanca <- function(parametros, t, y) {
  eta <- t %*% parametros
  lambda <- logistica(eta)
  soma <- sum(dpois(y, lambda, log = TRUE))
  return(soma)
}

parametros_iniciais <- c(0, 0)
ajuste <- optim(parametros_iniciais, function(p) -log_verossimilhanca(p, t = cbind(intercepto, temperatura), y = sucessos),
                hessian = TRUE)
# Parâmetros estimados 
alfa_beta <- ajuste$par
alfa_beta


# Modelo linear generalizado
mod <- glm(Sucessos ~ Temperatura, data = dados2, family=poisson())
tidy(mod)


#3)

# alpha e beta estimados
a <- alfa_beta[1]
b <- alfa_beta[2]
# Distribuição de m = 1000 observações geradas a partir do alpha e beta estimados para uma temperatura de 15 graus 
sucessos <- rpois(m,exp(a + b * 15)) 

# Distribuição dos sucessos gerados 
plot(density(sucessos))



#4)


# Número de simulações
m <- 1000

# Extração dos dados
t <- dados2$Temperatura
y <- dados2$Sucessos
n <- nrow(dados2)

# Simulação das amostras e cálculo dos betas
beta.sim.mod <- unlist(lapply(1:m, function(amostra) {
  lambda <- exp(a + b * t)
  y.sim <- rpois(n, lambda)
  lambda2 <- exp(a + b * (t + 1))
  y.sim2 <- rpois(n, lambda2)
  log(mean(y.sim2) / mean(y.sim))
}))

# Intervalo de confiança para Beta com 5% de confiança
beta.ic.est.mod <- quantile(beta.sim.mod, probs = c(0.025, 0.5, 0.975))


#5)

# Ajuste do modelo original
mod <- lm(Sucessos ~ Temperatura, data = dados2, family = poisson())
t <- dados2$Temperatura
y.est <- fitted(mod)
sigma.est <- summary(mod)$sigma

# Parâmetros do modelo original
a <- coef(mod)[1]
b <- coef(mod)[2]

# Número de repetições e vetor para armazenar os MSEs
num_repeticoes <- 10000
mse_simulacao <- numeric(num_repeticoes)

# Função para simulação e cálculo dos MSEs
simular_e_calcular_mse <- function(i) {
  # Geração de dados sintéticos
  lambda <- exp(a + b * t)
  y.sim <- rpois(length(t), lambda)
  
  # Ajuste do modelo sobre os dados sintéticos
  mod_sintetico <- lm(y.sim ~ t, family = poisson())
  y.est_sintetico <- fitted(mod_sintetico)
  
  # Cálculo do MSE
  mean((y.sim - y.est_sintetico)^2)
}

# Simulação e cálculo dos MSEs usando lapply
mse_simulacao <- unlist(lapply(1:num_repeticoes, simular_e_calcular_mse))

# Cálculo do MSE original
mse_original <- mean((dados2$Sucessos - y.est)^2)

# Cálculo do MSE médio simulado
mse_simulacao_media <- mean(mse_simulacao)

# Comparação e comentários dos resultados
if (mse_simulacao_media > mse_original) {
  mensagem <- "Os MSEs simulados são maiores que o MSE original."
} else if (mse_simulacao_media < mse_original) {
  mensagem <- "Os MSEs simulados são menores que o MSE original."
} else {
  mensagem <- "Os MSEs simulados são iguais ao MSE original."
}

cat("MSE Original:", mse_original, "\n")
cat("MSE Médio Simulado:", mse_simulacao_media, "\n")
cat(mensagem)



#6)

# Número de pontos simulados 
m <- 100000

# Valor exato
a <- sqrt(2)
b <- sqrt(3)
c <- sqrt(4)

(v <- (4/3) * pi * a * b * c)


# Simulação 3 dimensões 
x <- runif(m, -a, a)
y <- runif(m, -b, b)
z <- runif(m, -c, c)

# Valores dentro da região do elipsoide 
dentro <- x^2/2 + y^2/3 + z^2/4 < 1

8 * a * b * c * mean(dentro)





