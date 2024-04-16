
## Script para elaboração de figuras do artigo "Como mensurar a retenção de profissionais médicos pelas regiões de saúde no Brasil"

# carregando pacotes
library(tidyverse)
library(readxl)
library(geojsonio)


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

spdf <- geojson_read("1_scripts/shape file regioes saude.json",  what = "sp")

spdf_region <- spdf[ spdf@data$est_id == "52" , ]

spdf_fortified <- sf::st_as_sf(spdf)

spdf_fortified |>
  left_join(retencao, by = 
              c("reg_id"="cod_regiao_saude")) |>
  rename(Retenção = retencao_geral) |> 
  ggplot() +
  geom_sf(aes(fill = Retenção)) +
  theme_minimal() +
  scale_fill_gradient(low = "#d43621",
                      high = "#91e17c", 
                      n.breaks = 5) +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) 
