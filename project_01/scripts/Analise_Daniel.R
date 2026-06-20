# Pacotes necessários
library(tidyverse)

# 1. Carregamento dos dados
base_final <- read.csv2("base_final.csv")
# Corrige o formato decimal e garante tipagem numérica
base_final <- base_final %>%
  mutate(
    peso_upt = as.numeric(gsub(",", ".", peso_upt)),
    renda = as.numeric(gsub(",", ".", renda))
  )


# Onde as pessoas trabalham por UPT de residência
dados_bolhas <- base_final %>%
  group_by(unidade_planejamento, local_trabalho) %>%
  summarise(n = sum(peso_upt, na.rm = TRUE), .groups = 'drop')

ggplot(dados_bolhas, aes(x = unidade_planejamento, y = local_trabalho, size = n)) +
  geom_point(alpha = 0.6, color = "#2c3e50") +
  scale_size_continuous(range = c(2, 15)) +
  theme_minimal() +
  labs(title = "Onde as pessoas trabalham",
       subtitle = "Relação entre UPT de residência e local de trabalho",
       x = "Unidade de Planejamento (Residência)",
       y = "Local de Trabalho",
       size = "Pop. Ponderada") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Relação meio de transporte x tempo (Top 5 meios)
top5 <- base_final %>%
  count(meio_transporte, wt = peso_upt) %>%
  slice_max(n, n = 5) %>%
  pull(meio_transporte)

base_final %>%
  filter(meio_transporte %in% top5) %>%
  group_by(meio_transporte, tempo_deslocamento) %>%
  summarise(n = sum(peso_upt, na.rm = TRUE), .groups = 'drop') %>%
  group_by(meio_transporte) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = meio_transporte, y = prop, fill = tempo_deslocamento)) +
  geom_col(position = "fill") +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(title = "Transporte x Tempo de Deslocamento",
       x = "Meio de Transporte",
       y = "Proporção",
       fill = "Tempo de Deslocamento") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))