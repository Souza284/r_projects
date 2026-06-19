source("scripts/01_tratamento_de_dados.R")

# Tempo de deslocamento X UPT ---------------------------------------------

#Gráfico coroplético: Mapa com regiões coloridas ou hachuradas proporcionalmente
#ao valor de uma variável.

View(base_mapa)

?geom_sf
str(base_mapa, max.level = 2)

ggplot(base_mapa) +
  geom_sf(aes(fill = tempo_deslocamento))
