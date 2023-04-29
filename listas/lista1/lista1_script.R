pacman::p_load(readr,tidyverse,stringr)

##### Questao 1 ----

alfabeto = c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","x","y","z")

dicionario_ptbr <- read_csv("dados/dicionario_ptbr.txt") %>%
  select(palavras = 1)

matches = c()
for (n in 1:10) {
  amostra = c()
  for (i in 1:50000) {
    palavra[i] = str_flatten(paste(sample(alfabeto,5,replace = TRUE)))
    amostra[i] = palavra[i]
  }
match[n] = sum(dicionario_ptbr$palavras %in% amostra)
  
matches[n] = match[n]
}

matches

#testando
