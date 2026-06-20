source("scripts/01_tratamento_de_dados.R")

# Tempo de deslocamento X UPT ---------------------------------------------

#Gráfico coroplético: Mapa com regiões coloridas ou hachuradas proporcionalmente
#ao valor de uma variável. A cor de cada região é gerado por meio de uma média ponderada
#das cores atribuídas as proporções de cada faixa de tempo de cada UPT.

View(base_mapa)


# Tempo de deslocamento X Renda -------------------------------------------

#Mostrar graficamente a relação entre renda e o tempo de deslocamento para o trabalho mediante boxplot

base_filtrada <- base_final %>% 
  filter(renda > 0)

library(scales)

#Ao usar a escala contínua para a renda, a posição relativa das caixas é quase nula,
#devido a presença de valores extremamente discrepantes que deturpam a escala do gráfico.

#Solução: Usar escala log10

ggplot(base_filtrada, aes (x = tempo_deslocamento, y = renda, weight = peso_upt, fill = tempo_deslocamento)) +
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
