
## Script para elaboração de figuras do artigo "Como mensurar a retenção de profissionais médicos pelas regiões de saúde no Brasil"

# carregando pacotes
library(tidyverse)
library(readxl)

# Importando dados (já manipulados no excel)

Medico_dfs_geral <- read_excel("0_dados/Medico_dfs_geral.xlsx")

# Calculadno medidas resumo da variável "retencao_geral"

summary(Medico_dfs_geral[,7])

desv_p <- lapply(Medico_dfs_geral[,7], sd)
desv_p

# Construindo boxplot por região

Medico_dfs_geral |> 
  rename(Região = regiao) |> 
  ggplot(aes(x=fct_reorder(Região, retencao_geral, 
                           .desc = TRUE), y=retencao_geral, 
             fill=Região) , las=5) +
    geom_boxplot() + theme_minimal() + xlab("Região") + 
    ylab("Taxa de retenção")

# Calculando medidas resumo da variável "retencao_geral" por Região

tapply(Medico_dfs_geral$retencao_geral, Medico_dfs_geral$regiao, summary)

# Construindo boxplot por UF

Medico_dfs_geral <- Medico_dfs_geral |> 
                      mutate(regiao_order = 
                               case_when(regiao == "Sul" ~ 1,
                                         regiao == "Sudeste" ~ 2,
                                         regiao == "Centro-Oeste" ~ 3,
                                         regiao == "Norte" ~ 4,
                                         regiao == "Nordeste" ~ 5))

Medico_dfs_geral |> 
  rename(região = regiao) |> 
  ggplot(aes(x = fct_reorder(UF, regiao_order, .desc = TRUE), 
             y = retencao_geral, fill=região)) +
  geom_boxplot() + coord_flip() + theme_minimal() + 
  xlab("UF") + ylab("Retenção") 

# Calculadno medidas resumo da variável "retencao_geral" por UF
tapply(Medico_dfs_geral$retencao_geral, Medico_dfs_geral$UF, summary)


# Criando mapa de regiões de saúde considerando os percentis de retenção. 

teste <- Medico_dfs_geral |> 
  mutate(percentil = case_when(retencao_geral <= quantile(retencao_geral, probs = 0.10) ~ 1,
                               retencao_geral > quantile(retencao_geral, probs = 0.10) &
                               retencao_geral <= quantile(retencao_geral, probs = 0.20) ~ 2,
                               retencao_geral > quantile(retencao_geral, probs = 0.20) &
                               retencao_geral <= quantile(retencao_geral, probs = 0.30) ~ 3,
                               retencao_geral > quantile(retencao_geral, probs = 0.30) &
                               retencao_geral <= quantile(retencao_geral, probs = 0.40) ~ 4,
                               retencao_geral > quantile(retencao_geral, probs = 0.40) &
                               retencao_geral <= quantile(retencao_geral, probs = 0.50) ~ 5,
                               retencao_geral > quantile(retencao_geral, probs = 0.50) &
                               retencao_geral <= quantile(retencao_geral, probs = 0.60) ~ 6,
                               retencao_geral > quantile(retencao_geral, probs = 0.60) &
                               retencao_geral <= quantile(retencao_geral, probs = 0.70) ~ 7,
                               retencao_geral > quantile(retencao_geral, probs = 0.70) &
                                 retencao_geral <= quantile(retencao_geral, probs = 0.80) ~ 8,
                               retencao_geral > quantile(retencao_geral, probs = 0.80) &
                                 retencao_geral <= quantile(retencao_geral, probs = 0.90) ~ 9,
                               retencao_geral > quantile(retencao_geral, probs = 0.90) ~ 10))
  



