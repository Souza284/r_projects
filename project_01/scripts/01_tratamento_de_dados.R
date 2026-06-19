# Importando a base de dados PDAD2021 do IPEDF ----------------------------------------------

library(tidyverse)
# install.packages("readxl")
library(readxl)

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
    cor_raca = E06,
    peso_amostral_morador = PESO_MOR,
    pop_ajustada = POP_AJUSTADA_PROJ,
    pos_estrato = POS_ESTRATO
  ) %>% 
  #Filtra apenas as observações em que todas as variáveis foram respondidas
  filter(
    tempo_deslocamento != 99999 & tempo_deslocamento != 88888,
    meio_transporte != 99999 & meio_transporte != 88888,
    local_trabalho != 99999 & local_trabalho != 88888 & local_trabalho %in% 1:33,
    escolaridade != 99999 & escolaridade != 88888,
    sexo != 99999 & sexo != 88888,
    cor_raca != 99999 & cor_raca != 88888,
    !is.na(renda),
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
    ),
    #Local de trabalho
    local_trabalho = case_when(
      local_trabalho %in% c(1,11, 19, 22) ~ "Central",
      local_trabalho %in% c(16, 18, 23, 24) ~ "Central Adjacente 1",
      local_trabalho %in% c(8, 10, 17, 20, 25, 29, 30, 33) ~ "Central Adjacente 2",
      local_trabalho %in% c(3, 4, 9, 12, 32) ~ "Oeste",
      local_trabalho %in% c(2, 13, 15, 21) ~ "Sul",
      local_trabalho %in% c(7, 14, 27, 28) ~ "Leste",
      local_trabalho %in% c(5, 6, 26, 31) ~ "Norte"
    ),
    #Meio de transporte
    meio_transporte = case_when(
      meio_transporte == 1 ~ "Ônibus",
      meio_transporte == 2 ~ "Automóvel",
      meio_transporte == 3 ~ "Transporte privado (empresa de aplicativo)",
      meio_transporte == 4 ~ "Metrô",
      meio_transporte == 5 ~ "Motocicleta",
      meio_transporte == 6 ~ "Bicicleta",
      meio_transporte == 7 ~ "A pé"
    ),
    #Tempo de deslocamento
    tempo_deslocamento = case_when(
      tempo_deslocamento == 1 ~ "Até 15 minutos",
      tempo_deslocamento == 2 ~ "Acima de 15 minutos até 30 minutos",
      tempo_deslocamento == 3 ~ "Acima de 30 minutos até 45 minutos",
      tempo_deslocamento == 4 ~ "Acima de 45 minutos até 1 hora",
      tempo_deslocamento == 5 ~ "Acima de 1 hora até 1 hora e 15 minutos",
      tempo_deslocamento == 6 ~ "Acima de 1 hora e 15 minutos até 1 hora e 30 minutos",
      tempo_deslocamento == 7 ~ "Acima de 1 hora e 30 minutos até 1 hora e 45 minutos",
      tempo_deslocamento == 8 ~ "Acima 1 hora e 45 minutos até 2 horas",
      tempo_deslocamento == 9 ~ "Acima de 2 horas"
    ),
    tempo_deslocamento = factor(
      tempo_deslocamento,
      levels = c(
        "Até 15 minutos",
        "Acima de 15 minutos até 30 minutos",
        "Acima de 30 minutos até 45 minutos",
        "Acima de 45 minutos até 1 hora",
        "Acima de 1 hora até 1 hora e 15 minutos",
        "Acima de 1 hora e 15 minutos até 1 hora e 30 minutos",
        "Acima de 1 hora e 30 minutos até 1 hora e 45 minutos",
        "Acima 1 hora e 45 minutos até 2 horas",
        "Acima de 2 horas"
      )
    ),
    #Sexo
    sexo = case_when(
      sexo == 1 ~ "Masculino",
      sexo == 2 ~ "Feminino"
    ),
    #Cor/raça
    cor_raca = case_when(
      cor_raca == 1 ~ "Branca",
      cor_raca == 2 ~ "Preta",
      cor_raca == 3 ~ "Amarela",
      cor_raca == 4 ~ "Parda",
      cor_raca == 5 ~ "Indígena"
    )
  )

View(base_final)

# Conferindo a distribuição da amostra ------------------------------------

nrow(base_final)

#Total da amostra da base final: 21488
base_final %>% 
  group_by(unidade_planejamento) %>% 
  summarise(tot = n())

#Distribuição bruta
# 1 Central               4950
# 2 Central Adjacente 1   1683
# 3 Central Adjacente 2   5298
# 4 Oeste                 2992
# 5 Sul                   1756
# 6 Leste                 3182
# 7 Norte                 1627

#AMOSTRA ENVIESADA!


# Retirar o viés da amostra -----------------------------------------------

#No banco de dados, existem variáveis que corrigem isso: peso dos moradores (fator de expansão),
#posição estrato(estrato do plano amostral) e população ajustada (total de pessoas por estrato)

#SOLUÇÃO: Atribuir os pesos - peso = proporção real da região / proporção da amostra
install.packages("survey")
library(survey)

#1. Criar uma tabela com as proporções reais da população do DF
prop_real <- data.frame(
  unidade_planejamento = c(
    "Central",
    "Central Adjacente 1",
    "Central Adjacente 2",
    "Oeste",
    "Sul",
    "Leste",
    "Norte"
  ),
  prop_real = c(9.78, 4.16, 19.07, 24.46, 16.29, 15.04, 11.19)
)

#2. Calcular a distribuição da sua amostra por UPT
dist_bruta <- base_final %>%
  group_by(unidade_planejamento) %>%
  summarise(
    n_bruto = n()  # conta quantas linhas tem em cada UPT
  ) %>%
  mutate(
    prop_bruta = n_bruto / sum(n_bruto) * 100  # calcula a porcentagem
  )

#3. Juntar a tabela real com a da amostra
comparacao <- prop_real %>%
  left_join(dist_bruta, by = "unidade_planejamento")

#4. Calcular o peso para cada UPT
comparacao <- comparacao %>%
  mutate(
    peso_upt = prop_real / prop_bruta
  )

#5. Adicionar o peso na base final
base_final <- base_final %>%
  left_join(
    comparacao %>% select(unidade_planejamento, peso_upt),
    by = "unidade_planejamento"
  )

#6. Garantir que peso_upt é numérico
base_final <- base_final %>%
  mutate(
    peso_upt = as.numeric(peso_upt)
  )

#7. Calcular a distribuição ponderada
dist_ponderada_ajustada <- base_final %>%
  group_by(unidade_planejamento) %>%
  summarise(
    n_ponderado_ajustado = sum(peso_upt, na.rm = TRUE)
  ) %>%
  mutate(
    prop_ponderada_ajustada = n_ponderado_ajustado / sum(n_ponderado_ajustado, na.rm = TRUE) * 100
  )

#8. Juntar tudo para comparar
comparacao_ajustada <- prop_real %>%
  left_join(dist_bruta, by = "unidade_planejamento") %>%
  left_join(dist_ponderada_ajustada, by = "unidade_planejamento") %>%
  mutate(
    diferenca_bruta = prop_bruta - prop_real,
    diferenca_ajustada = prop_ponderada_ajustada - prop_real
  )

#9. Ver o resultado
print(comparacao_ajustada)

#10. Calcular o erro total
cat("Erro sem peso:", sum(abs(comparacao_ajustada$diferenca_bruta)), "\n")
cat("Erro com peso:", sum(abs(comparacao_ajustada$diferenca_ajustada)), "\n")
