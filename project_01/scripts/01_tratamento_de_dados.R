# Importando a base de dados ----------------------------------------------

library(tidyverse)

dados_brutos_moradores <- read.csv("dados/moradores.csv", sep = ";")

View(dados_brutos_moradores)

# Manipulando a base de moradores -----------------------------------------

#Variáveis: 

##Localidade - Categórica nominal
##sexo (E03) - Categórica nominal
##cor/raça (E05) - Categórica nominal
##meio de transporte (I09_9) - Categórica nominal
#tempo gasto no deslocamento pro trabalho (I10) - Quantitativa contínua
##renda (renda_ind_r) - Quantitativa contínua
##escolaridade - Categórica ordinal
##Local de trabalho principal (I08) - Categórica nominal

dados_moradores <- dados_brutos_moradores %>% 
  select(localidade, I08, I09_9, I10, renda_ind_r, escolaridade, E05, E03) %>% 
  rename(regiao_administrativa = localidade, 
         local_de_trabalho = I08, 
         meio_de_transporte = I09_9,
         tempo_gasto_deslocamento = I10,
         renda = renda_ind_r,
         cor_raca = E05,
         sexo = E03) %>% 
  group_by(regiao_administrativa)

#Amostra

View(summarise(dados_moradores, tot = n()))

View(dados_moradores)

