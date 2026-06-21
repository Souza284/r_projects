# Carregamento da base de dados final -------------------------------------


base_final <- read.csv2("dados/base_final.csv")


# VISUALIZAÇÃO E ANALISE DESCRITIVA ---------------------------------------


#1. Fluxo de mobilidade urbana: Proporção do deslocamento das pessoas -------


# Pacotes e bibliotecas ---------------------------------------------------


library(tidyverse)


# Onde as pessoas trabalham por UPT de residência: Gráfico de bolhas
dados_bolhas <- base_final %>%
  group_by(unidade_planejamento, local_trabalho) %>%
  summarise(n = sum(peso_upt, na.rm = TRUE), .groups = 'drop')

ggplot(dados_bolhas, aes(x = unidade_planejamento, y = local_trabalho, size = n)) +
  geom_point(alpha = 0.8, color = "#2c3e50") +
  scale_size_continuous(range = c(2, 15)) +
  theme_minimal() +
  labs(title = "Residência X Local de trabalho",
       subtitle = "Relação entre UPT de residência e local de trabalho",
       x = "Unidade de Planejamento (Residência)",
       y = "Local de Trabalho",
       size = "Pop. Ponderada") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 2. Composição do tempo de deslocamento de cada UPT ----------------------


# Pacotes e bibliotecas ---------------------------------------------------


#install.packages("sf")
library(sf)


# UPT X Tempo de deslocamento ---------------------------------------------


# Mapa de composição: cada UPT recebe uma coloração específica do gradiente de cor
# contínuo associada a proporção de cada faixa de tempo dentro da UPT


#Carregando os dados geográficos

dados_espaciais_upt <- st_read("dados/dados_geograficos/UPT.shp")

#Proporções de cada UPT

proporcoes_upt <- base_final %>% 
  group_by(unidade_planejamento, tempo_deslocamento) %>% 
  summarise(n_ponderado = sum(peso_upt)) %>% 
  group_by(unidade_planejamento) %>% 
  mutate(prop = n_ponderado/sum(n_ponderado)) %>% 
  ungroup() %>% 
  select(
    unidade_planejamento,
    tempo_deslocamento,
    prop
  ) %>% 
  pivot_wider(
    names_from = tempo_deslocamento,
    values_from = prop,
    values_fill = 0
  )

names(proporcoes_upt) <- c(
  "unidade_planejamento",
  "prop_30min",
  "prop_30min_1h",
  "prop_1h_1h30",
  "prop_1h30_mais"
)

base_mapa <- dados_espaciais_upt %>% 
  left_join(proporcoes_upt, by = c("nome" = "unidade_planejamento"))

#Calcular o índice de tempo (média ponderada das categorias)

base_mapa <- base_mapa %>%
  mutate(
    # Cada categoria recebe um peso: 1 (até 30 min) a 4 (acima de 90 min)
    indice_tempo = prop_30min * 1 + 
      prop_30min_1h * 2 + 
      prop_1h_1h30 * 3 + 
      prop_1h30_mais * 4
  )

#Desenhando o mapa

#?geom_sf

ggplot(base_mapa) +
  geom_sf(aes(fill = indice_tempo)) +
  scale_fill_gradient2(
    low = "#2b83ba",      # azul (tempo baixo)
    mid = "#fdae61",      # laranja (tempo médio)
    high = "#d7191c",     # vermelho (tempo alto)
    midpoint = 2,         # ponto médio da escala (entre 1 e 4)
    name = "Índice de tempo\n(1 = baixo, 4 = alto)"
  ) +
  geom_sf_label(aes(label = nome), size = 3, color = "white") +
  labs(
    title = "Perfil de tempo de deslocamento por UPT",
    subtitle = "Índice baseado nas proporções de cada categoria de tempo",
    caption = "Fonte: PDAD 2021"
  ) +
  theme_void()


# 3. Relação do meio de transporte com o tempo de deslocamento ------------


# Selecionando os 5 meios de transportes mais utilizados

top5 <- base_final %>%
  count(meio_transporte, wt = peso_upt) %>%
  slice_max(n, n = 5) %>%
  pull(meio_transporte)

# Criando uma base filtrada com os meios de transporte mais utilizados e seus tempos

base_transporte_tempo <- base_final %>%
  filter(meio_transporte %in% top5) %>%
  group_by(meio_transporte, tempo_deslocamento) %>%
  summarise(n = sum(peso_upt, na.rm = TRUE), .groups = 'drop') %>%
  group_by(meio_transporte) %>%
  mutate(prop = n / sum(n))

# Gráfico de barras empilhadas da proporção do tempo de deslocamento por meio de transporte

ggplot(base_transporte_tempo, aes(x = meio_transporte, y = prop, fill = tempo_deslocamento)) +
  geom_col(position = "fill") +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(title = "Transporte x Tempo de Deslocamento",
       x = "Meio de Transporte",
       y = "Proporção",
       fill = "Tempo de Deslocamento") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



install.packages("matrixStats")
install.packages("ggdist")

library(tidyverse)
library(matrixStats)


# 4. Correlação do tempo de deslocamento com a renda -------------------------


# Pacotes e bibliotecas ---------------------------------------------------


#install.packages("Hmisc")

library(Hmisc)
library(scales)


#Mostrar graficamente a relação entre renda e o tempo de deslocamento para o trabalho mediante boxplot

#Ao usar a escala contínua para a renda, a posição relativa das caixas é quase nula,
#devido a presença de valores extremamente discrepantes que deturpam a escala do gráfico.

#SOLUÇÃO: Usar escala log10

#Boxplot: Tempo de deslocamento X Renda


ggplot(base_final, aes (x = tempo_deslocamento, y = renda, weight = peso_upt, fill = tempo_deslocamento)) +
  geom_boxplot(outlier.size = 1) +
  scale_y_log10(
    labels = label_currency(
      prefix = "R$",
      suffix = "",
      big.mark = ".",
      decimal.mark = ","
    )
  ) +
  scale_fill_manual(
    values = c("#D7C1A8", "#A28C75", "#825b45", "#6C5141")
  ) +
  guides(
    fill = FALSE
  ) +
  labs(
    title = "Renda X Tempo de deslocamento",
    subtitle = "Renda representada pela escala logaritmica de base 10",
    x = "Tempo de deslocamento",
    y = "Renda"
  ) +
  theme_classic()


#Medida descritiva: R²

#Variância da renda (com os pesos)

?wtd.var

var_total <- wtd.var(base_filtrada_renda$renda, weights = base_filtrada_renda$peso_upt)

#Variância dentro de cada grupo (com os pesos)

var_dentro <- base_filtrada_renda %>% 
  group_by(tempo_deslocamento) %>% 
  summarise(
    n_ponderado = sum(peso_upt),
    var_grupo = wtd.var(renda, weights = peso_upt)
  ) %>% 
  summarise(
    var_media = (sum(n_ponderado * var_grupo) / sum(n_ponderado))
  ) %>% 
  pull(var_media)

#R²

r2 <- 1 - (var_dentro/var_total)



# 5 . Relação do tempo de deslocamento com características sociodemográficas --------


# Pacotes e bibliotecas ---------------------------------------------------

#install.packages("DescTools")

library(DescTools)

# Reagrupamento de variáveis (certas categorias tem poucas observações)


base_reagrupada <- base_final %>%
  mutate(
    # Tempo agrupado
    tempo_agrupado = case_when(
      tempo_deslocamento %in% c("Até 30 minutos") ~ "Até 30 min",
      tempo_deslocamento %in% c("Entre 30 minutos e 1 hora") ~ "30-60 min",
      tempo_deslocamento %in% c("Entre 1 hora e 1 hora e 30 minutos", 
                                "Entre 1 hora e 30 minutos e 2 horas",
                                "Acima de 2 horas") ~ "Acima de 1 hora"
    ),
    # Cor/raça agrupada
    cor_agrupada = case_when(
      cor_raca %in% c("Branca", "Preta", "Parda") ~ cor_raca,
      TRUE ~ "Outras"
    ),
    # Escolaridade agrupada
    escolaridade_agrupada = case_when(
      escolaridade %in% c("Sem instrução", "Fundamental incompleto", "Fundamental completo") ~ "Até fundamental",
      escolaridade %in% c("Médio incompleto", "Médio completo") ~ "Ensino médio",
      escolaridade %in% c("Superior incompleto", "Superior completo") ~ "Superior"
    )
  ) %>% 
  mutate(
    tempo_agrupado = factor(
      tempo_agrupado,
      levels = c(
        "Até 30 min",
        "30-60 min",
        "Acima de 1 hora"
      ),
      ordered = TRUE
    )
  )

?xtabs

# Tabelas de contingência (com os pesos)
tab_sexo <- xtabs(peso_upt ~ tempo_deslocamento + sexo, data = base_reagrupada)
tab_cor <- xtabs(peso_upt ~ tempo_agrupado + cor_agrupada, data = base_reagrupada)
tab_escolaridade <- xtabs(peso_upt ~ tempo_agrupado + escolaridade_agrupada, data = base_reagrupada)

# Qui-quadrado
chisq.test(tab_sexo)
chisq.test(tab_cor)
chisq.test(tab_escolaridade)

# Coeficiente Gamma para Tempo X Escolaridade

gamma <- GoodmanKruskalGamma(tab_escolaridade)

#Gamma ≃ -0.12
