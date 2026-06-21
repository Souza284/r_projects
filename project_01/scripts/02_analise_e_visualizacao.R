source("scripts/01_tratamento_de_dados.R")

# Pacotes e bibliotecas ---------------------------------------------------

install.packages("matrixStats")
install.packages("ggdist")
install.packages("Hmisc")

library(scales)
library(matrixStats)
library(ggdist)
library(Hmisc)


# UPT X Tempo de deslocamento ---------------------------------------------

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


View(base_mapa_final)

#Desenhando o mapa

?geom_sf

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

# Tempo de deslocamento X UPT ---------------------------------------------

#Gráfico coroplético: Mapa com regiões coloridas ou hachuradas proporcionalmente
#ao valor de uma variável. A cor de cada região é gerado por meio de uma média ponderada
#das cores atribuídas as proporções de cada faixa de tempo de cada UPT.

View(base_mapa)


# Tempo de deslocamento X Renda -------------------------------------------

#Mostrar graficamente a relação entre renda e o tempo de deslocamento para o trabalho mediante boxplot

base_filtrada_renda <- base_final %>% 
  filter(renda > 0)


#Ao usar a escala contínua para a renda, a posição relativa das caixas é quase nula,
#devido a presença de valores extremamente discrepantes que deturpam a escala do gráfico.

#Solução: Usar escala log10

ggplot(base_filtrada_renda, aes (x = tempo_deslocamento, y = renda, weight = peso_upt, fill = tempo_deslocamento)) +
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


