# Importando a base de dados PDAD2021 do IPEDF ----------------------------------------------

library(tidyverse)
# install.packages("readxl")
# library(readxl)

?read.csv

#Salvando a base de dados dos moradores.
dados_brutos_moradores <- read.csv2("dados/PDAD_2021-Moradores.csv")

#Tamanho da amostra dos dados brutos dos moradores: 83481
nrow(dados_brutos_moradores)

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

#Manipulando a base cruzada e gerando a base de dados final.
base_final <- base_cruzada %>% 
  #Seleciona as variáveis do escopo do projeto
  select(
    unidade_planejamento = UPT,
    regiao_administrativa = A01ra.x,
    local_trabalho = I08,
    meio_transporte = I09_8,
    tempo_deslocamento = I10,
    renda = renda_ind_r,
    escolaridade,
    sexo = E04,
    cor_raca = E06
  ) %>% 
  #Filtra apenas as observações em que todas as variáveis foram respondidas
  filter(
    tempo_deslocamento != 99999 & tempo_deslocamento != 88888,
    meio_transporte != 99999 & meio_transporte != 88888,
    local_trabalho != 99999 & local_trabalho != 88888,
    escolaridade != 99999 & escolaridade != 88888,
    sexo != 99999 & sexo != 88888,
    cor_raca != 99999 & cor_raca != 88888,
    !is.na(renda)
  ) %>% 
  #Ordena em ordem crescente as Unidades de Planejamento Territoriais e as regiões administrativas.
  arrange(unidade_planejamento, regiao_administrativa) %>% 
  #Ordena as Unidades de Planejamento Territoriais por grupos 
  group_by(unidade_planejamento) %>% 
  #Categoriza as variáveis.
  mutate(
    #Unidades de Planejamento Territoriais
    unidade_planejamento = case_when(
      unidade_planejamento == 1 ~ "Central",
      unidade_planejamento == 2 ~ "Central Adjacente 1",
      unidade_planejamento == 3 ~ "Central Adjacente 2",
      unidade_planejamento == 4 ~ "Oeste",
      unidade_planejamento == 5 ~ "Sul",
      unidade_planejamento == 6 ~ "Leste",
      unidade_planejamento == 7 ~ "Norte",
    ),
    unidade_planejamento = factor(
      unidade_planejamento,
      levels = c(
        "Central",
        "Central Adjacente 1",
        "Central Adjacente 2",
        "Oeste",
        "Sul",
        "Leste",
        "Norte"
      )
    ),
    #Regiões administrativas.
    regiao_administrativa = case_when(
      regiao_administrativa == 1 ~ "Plano Piloto",
      regiao_administrativa == 2 ~ "Gama",
      regiao_administrativa == 3 ~ "Taguatinga",
      regiao_administrativa == 4 ~ "Brazlândia",
      regiao_administrativa == 5 ~ "Sobradinho",
      regiao_administrativa == 6 ~ "Planaltina",
      regiao_administrativa == 7 ~ "Paranoá",
      regiao_administrativa == 8 ~ "Núcleo Bandeirante",
      regiao_administrativa == 9 ~ "Ceilândia",
      regiao_administrativa == 10 ~ "Guará",
      regiao_administrativa == 11 ~ "Cruzeiro",
      regiao_administrativa == 12 ~ "Samambaia",
      regiao_administrativa == 13 ~ "Santa Maria",
      regiao_administrativa == 14 ~ "São Sebastião",
      regiao_administrativa == 15 ~ "Recanto Das Emas",
      regiao_administrativa == 16 ~ "Lago Sul",
      regiao_administrativa == 17 ~ "Riacho Fundo",
      regiao_administrativa == 18 ~ "Lago Norte",
      regiao_administrativa == 19 ~ "Candangolândia",
      regiao_administrativa == 20 ~ "Águas Claras",
      regiao_administrativa == 21 ~ "Riacho Fundo II",
      regiao_administrativa == 22 ~ "Sudoeste e Octogonal",
      regiao_administrativa == 23 ~ "Varjão",
      regiao_administrativa == 24 ~ "Park Way",
      regiao_administrativa == 25 ~ "SCIA",
      regiao_administrativa == 26 ~ "Sobradinho II",
      regiao_administrativa == 27 ~ "Jardim Botânico",
      regiao_administrativa == 28 ~ "Itapoã",
      regiao_administrativa == 29 ~ "SIA",
      regiao_administrativa == 30 ~ "Vicente Pires",
      regiao_administrativa == 31 ~ "Fercal",
      regiao_administrativa == 32 ~ "Sol Nascente / Pôr do Sol",
      regiao_administrativa == 33 ~ "Arniqueira"
    ),
    regiao_administrativa = factor(regiao_administrativa),
    #Escolaridade
    escolaridade = case_when(
      escolaridade == 1 ~ "Sem instrução",
      escolaridade == 2 ~ "Fundamental incompleto",
      escolaridade == 3 ~ "Fundamental completo",
      escolaridade == 4 ~ "Médio incompleto",
      escolaridade == 5 ~ "Médio completo",
      escolaridade == 6 ~ "Superior incompleto",
      escolaridade == 7 ~ "Superior completo"
    ),
    escolaridade = factor(
      escolaridade,
      levels = c(
        "Sem instrução",
        "Fundamental incompleto",
        "Fundamental completo",
        "Médio incompleto",
        "Médio completo",
        "Superior incompleto",
        "Superior completo"
      )
    )
  )

View(base_final)
base_final %>% 
  group_by(unidade_planejamento) %>% 
  summarise(tot = n())

#Total da amostra da base final: 21488
nrow(base_final)

# 1 Leste                   3182
# 2 Norte                 1627
# 3 Oeste                 2992
# 4 Sul                   1756
# 5 central               4950
# 6 central adjacente 1   1683
# 7 central adjacente 2   5298


