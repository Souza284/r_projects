# Tempo de deslocamento x características sociodemográficas

library(tidyverse)

# Importar base
base_final <- read.csv2("base_final.csv")

# Conferir variáveis
str(base_final)
names(base_final)

# Ordenar tempo de deslocamento original
base_final$tempo_deslocamento <- factor(
  base_final$tempo_deslocamento,
  levels = c(
    "Até 30 minutos",
    "Entre 30 minutos e 1 hora",
    "Entre 1 hora e 1 hora e 30 minutos",
    "Entre 1 hora e 30 minutos e 2 horas",
    "Acima de 2 horas"
  ),
  ordered = TRUE
)

# Agrupar tempo em 3 categorias
base_final$tempo_deslocamento3 <- as.character(base_final$tempo_deslocamento)

base_final$tempo_deslocamento3[
  base_final$tempo_deslocamento %in% c(
    "Entre 1 hora e 1 hora e 30 minutos",
    "Entre 1 hora e 30 minutos e 2 horas",
    "Acima de 2 horas"
  )
] <- "Acima de 1 hora"

base_final$tempo_deslocamento3 <- factor(
  base_final$tempo_deslocamento3,
  levels = c(
    "Até 30 minutos",
    "Entre 30 minutos e 1 hora",
    "Acima de 1 hora"
  ),
  ordered = TRUE
)

# Agrupar cor/raça
base_final$cor_raca2 <- base_final$cor_raca

base_final$cor_raca2[base_final$cor_raca == "Amarela"] <- "Outras"
base_final$cor_raca2[base_final$cor_raca == "Indígena"] <- "Outras"

# Agrupar escolaridade
base_final$escolaridade2 <- base_final$escolaridade

base_final$escolaridade2[
  base_final$escolaridade %in% c(
    "Sem instrução",
    "Fundamental incompleto",
    "Fundamental completo"
  )
] <- "Até fundamental"

base_final$escolaridade2[
  base_final$escolaridade %in% c(
    "Médio incompleto",
    "Médio completo"
  )
] <- "Ensino médio"

base_final$escolaridade2[
  base_final$escolaridade %in% c(
    "Superior incompleto",
    "Superior completo"
  )
] <- "Superior"

# Frequências simples
table(base_final$tempo_deslocamento)
table(base_final$tempo_deslocamento3)
table(base_final$sexo)
table(base_final$cor_raca2)
table(base_final$escolaridade2)

# Tabelas de contingência
tab_sexo <- table(base_final$tempo_deslocamento, base_final$sexo)

tab_cor <- table(base_final$tempo_deslocamento3, base_final$cor_raca2)

tab_escolaridade <- table(base_final$tempo_deslocamento3,
                          base_final$escolaridade2)

# Visualizar tabelas
tab_sexo
tab_cor
tab_escolaridade

# Percentuais por coluna
prop.table(tab_sexo, 2) * 100
prop.table(tab_cor, 2) * 100
prop.table(tab_escolaridade, 2) * 100

# Testes qui-quadrado
chisq.test(tab_sexo)
chisq.test(tab_cor)
chisq.test(tab_escolaridade)

# Frequências esperadas
chisq.test(tab_cor)$expected
chisq.test(tab_escolaridade)$expected

