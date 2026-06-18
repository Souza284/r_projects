# Importando a base de dados PDAD2021 do IPEDF ----------------------------------------------

library(tidyverse)
install.packages("readxl")
library(readxl)

?read.csv

#Salvando a base de dados dos moradores.
dados_brutos_moradores <- read.csv2("dados/PDAD_2021-Moradores.csv")

#Salvando a base de dados dos domicílios.
dados_brutos_domicilios <- read.csv2("dados/PDAD_2021-Domicilios(1).csv")


# Variáveis ---------------------------------------------------------------

# Regiao administrativa - Categórica nominal (A01ra).
# Local de trabalho - Categórica nominal (I08).
# Meio de transporte - Categorica nominal (I09_8).
# Sexo - Categórica nominal (E04).
# Cor/raca - Categórica nominal (E06).
# Escolaridade - Categorica ordinal (escolaridade).
# Tempo - Categorica ordinal (I10).
# Renda - Quantitativa continua (renda_ind_r).


# Tratamento e manipulação dos dados --------------------------------------

#Cruzando as bases de dados pelo número da ficha do morador e domicílio.
base_cruzada <- dados_brutos_moradores %>% 
  left_join(dados_brutos_domicilios, by = "A01nficha")

#Filtrando as variáveis para análise.
base_final <- base_cruzada %>% 
  select(
    regiao_administrativa = A01nficha,
    unidade_planejamento = UPT,
    local_trabalho = I08,
    meio_transporte = I09_8,
    tempo_deslocamento = I10,
    renda = renda_domiciliar_pc_r,
    escolaridade,
    sexo = E04,
    cor_raca = E06
  ) %>% 
  filter(
    tempo_deslocamento != 99999 & tempo_deslocamento != 88888,
    meio_transporte != 99999 & meio_transporte != 88888,
    local_trabalho != 99999 & local_trabalho != 88888,
    escolaridade != 99999 & escolaridade != 88888,
    sexo != 99999 & sexo != 88888,
    cor_raca != 99999 & cor_raca != 88888,
    !is.na(renda)
  ) %>% 
  arrange(unidade_planejamento) %>% 
  group_by(unidade_planejamento) %>% 
  mutate(
    unidade_planejamento = case_when(
      unidade_planejamento == 1 ~ "central",
      unidade_planejamento == 2 ~ "central adjacente 1",
      unidade_planejamento == 3 ~ "central adjacente 2",
      unidade_planejamento == 4 ~ "Oeste",
      unidade_planejamento == 5 ~ "Sul",
      unidade_planejamento == 6 ~ "Leste",
      unidade_planejamento == 7 ~ "Norte",
    )
  )

View(base_final)

#Total da amostra
nrow(base_final)


# Tratando as variáveis ---------------------------------------------------

#
