# Importando a base de dados ----------------------------------------------

library(tidyverse)

?read.csv

#Salvando a base de dados dos moradores.
dados_brutos_moradores <- read.csv("dados/moradores.csv", sep = ";")

View(dados_brutos_moradores)


# Variáveis ---------------------------------------------------------------

# Regiao administrativa - Categórica nominal (localidade).
# Local de trabalho - Categórica nominal (I08).
# Meio de transporte - Categorica nominal (I09_9).
# Sexo - Categórica nominal (E03).
# Cor/raca - Categórica nominal (E05).
# Escolaridade - Categorica ordinal (escolaridade).
# Tempo - Categorica ordinal (I10).
# Renda - Quantitativa continua (renda_ind_r).


# Tratamento e manipulação dos dados --------------------------------------

#Base de dados dados_moradores_tratamento armazena as variáveis e as observações
#que não apresentam NA em nenhuma coluna.

dados_moradores_tratamento <- dados_brutos_moradores %>% 
  select(regiao_administrativa = localidade, 
         local_trabalho = I08,
         meio_transporte = I09_9,
         tempo_deslocamento = I10,
         escolaridade,
         cor_raca = E05,
         sexo = E03,
         renda = renda_ind_r) %>% 
  filter(
    !regiao_administrativa == 99999,
    !local_trabalho == 99999,
    !meio_transporte == 99999,
    !tempo_deslocamento == 99999,
    !escolaridade == 99999,
    !cor_raca == 99999,
    !sexo == 99999,
    !renda == 99999
  )

View(dados_moradores_tratamento)
