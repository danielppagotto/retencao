
## Script para elaboração de figuras do artigo "Como mensurar a retenção de profissionais médicos pelas regiões de saúde no Brasil"

# carregando pacotes
library(tidyverse)
library(readxl)
library(geojsonio)
library(RODBC)
library(tmap)
library(ggspatial)
library(ggrepel)


# Importando dados

#Medico_dfs_geral <- read_excel("0_dados/Medico_dfs_geral.xlsx")

Medico_dfs_geral <- read_delim("0_dados/Medico_retencao_geral.csv", 
                               delim = ";", 
                               escape_double = FALSE, 
                               trim_ws = TRUE) |> 
                    select(-`...1`) |> 
                    mutate(regiao_saude = as.character(regiao_saude))

# Pegando dados da hierarquia

hierarquia_completa <- read_csv("0_dados/hierarquia_atualizada.csv") 


hierarquia_atualizada <- read_csv("0_dados/hierarquia_atualizada.csv") |> 
                              select(regiao, cod_uf, uf, cod_regsaud, regiao_saude) |> 
                              mutate(cod_regsaud = as.character(cod_regsaud)) |> 
                              distinct(regiao, cod_uf, uf, cod_regsaud, regiao_saude)

# juntando tudo 

Medico_dfs_geral <- Medico_dfs_geral |> 
                      left_join(hierarquia_atualizada, 
                                by = c("regiao_saude"="cod_regsaud")) |> 
                      rename(cod_regiao_saude = regiao_saude,
                             nome_regiao_saude = regiao_saude.y)

# Calculadno medidas resumo da variável "retencao_geral"

summary(Medico_dfs_geral[,7])

desv_p <- lapply(Medico_dfs_geral[,7], sd)
desv_p

# Construindo boxplot por região

Medico_dfs_geral |> 
  rename(Região = regiao) |> 
  ggplot(aes(x= fct_reorder(Região, retencao_geral, 
                           .desc = TRUE), y=retencao_geral, 
             fill=Região) , las=5) +
    geom_boxplot() + theme_minimal() + xlab("Região") + 
    ylab("Taxa de retenção") + 
    theme(
      axis.text.x = element_text(size = 16),  
      axis.text.y = element_text(size = 16),
      legend.text = element_text(size = 16),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16)
    )

# Calculando medidas resumo da variável "retencao_geral" por Região

tapply(Medico_dfs_geral$retencao_geral, Medico_dfs_geral$regiao, summary)

# Construindo boxplot por UF

Medico_dfs_geral <- Medico_dfs_geral |> 
                      mutate(regiao_order = 
                               case_when(regiao == "Região Sul" ~ 1,
                                         regiao == "Região Sudeste" ~ 2,
                                         regiao == "Região Centro-Oeste" ~ 3,
                                         regiao == "Região Norte" ~ 4,
                                         regiao == "Região Nordeste" ~ 5))


Medico_dfs_geral |>
  rename(Região = regiao) |>
  ggplot(aes(x = fct_reorder(uf, regiao_order, .desc = TRUE), 
             y = retencao_geral, fill = Região)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal() +
  xlab("UF") +
  ylab("Retenção") +
  theme(
    axis.text.x = element_text(size = 12),  
    axis.text.y = element_text(size = 12)   
  )


# Calculando medidas resumo da variável "retencao_geral" por UF
tapply(Medico_dfs_geral$retencao_geral, Medico_dfs_geral$UF, summary)


# Criando mapa de regiões de saúde considerando os percentis de retenção. 


# preparando os shapes dos mapas

data("World")
latam <- World |> filter(continent == "South America")

latam <- sf::st_as_sf(latam)
latam <- st_set_crs(latam, st_crs(spdf_fortified))

Medico_dfs_geral$cod_regiao_saude <- as.integer(Medico_dfs_geral$cod_regiao_saude)

spdf <- geojson_read("1_scripts/shape file regioes saude.json",  what = "sp")

spdf_fortified <- sf::st_as_sf(spdf)


# Pegando as coordenadas de capitais 

## Adicionando uma última camada das capitais 

capitais <- c("1100205","1302603","1200401","5002704","1600303","5300108",
              "1400100","5103403","1721000","3550308","2211001","3304557",
              "1501402","5208707","2927408","4205407","2111300","2704302",
              "4314902","4106902","3106200","2304400","2611606","2507507",
              "2800308","2408102","3205309")



capitais_coord <- 
  hierarquia_completa |>
  mutate(cod_municipiodv = as.character(cod_municipiodv)) |> 
  filter(cod_municipiodv %in% capitais) |> 
  select(cod_municipio, municipio, longitude, latitude) |> 
  mutate(latitude = as.numeric(latitude))

#



library(ggplot2)

# Defina os limites de longitude e latitude para focar no Brasil
limite_long <- c(-75, -28)  # limites de longitude
limite_lat <- c(-33, 4)     # limites de latitude

mapa <- spdf_fortified |>
  left_join(Medico_dfs_geral, by = 
              c("reg_id"="cod_regiao_saude")) |>
  rename(Retenção = retencao_geral) |> 
  ggplot() +
  geom_sf(data = latam, 
          fill = "lightgrey", color = "black") + 
  geom_sf(aes(fill = Retenção)) +
  geom_point(data = capitais_coord, 
             aes(x = longitude, y = latitude), color = "blue", size = 1) +
  geom_text_repel(data = capitais_coord,
                  aes(label = municipio, longitude, y = latitude)) +
  xlab("Longitude") + ylab("Latitude") +
  theme_minimal() +
  scale_fill_gradient(low = "#d43621",
                      high = "#91e17c", 
                      n.breaks = 5) +
  coord_sf(xlim = limite_long, ylim = limite_lat)

mapa + 
#  ggspatial::annotation_scale(
#    location = "tr",
#    bar_cols = c("grey60", "white")http://127.0.0.1:46997/graphics/d580ee10-96dd-4193-b40c-14a0e0f05c17.png
#  )  + 
  ggspatial::annotation_north_arrow(
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20")
  )



