
## Script para elaboração de figuras do artigo "Como mensurar a retenção de profissionais médicos pelas regiões de saúde no Brasil"


# Carregando Pacotes ------------------------------------------------------

library(tidyverse)
library(readxl)
library(geojsonio)
library(RODBC)
library(tmap)
library(ggspatial)
library(ggrepel)
library(sf)

# Importando dados --------------------------------------------------------

# Dados de retencao
Medico_dfs_geral <- 
  read_delim("0_dados/Medico_retencao_geral.csv", 
             delim = ";", 
             escape_double = FALSE, 
             trim_ws = TRUE) |> 
  select(-`...1`) |> 
  mutate(regiao_saude = as.character(regiao_saude))

# Dados da hierarquia

hierarquia_completa <- 
  read_csv("0_dados/hierarquia_atualizada.csv") 


hierarquia_atualizada <- 
  read_csv("0_dados/hierarquia_atualizada.csv") |> 
      select(regiao, cod_uf, uf, 
             cod_regsaud, regiao_saude) |> 
      mutate(cod_regsaud = as.character(cod_regsaud)) |> 
      distinct(regiao, cod_uf, uf, 
               cod_regsaud, regiao_saude)

# juntando as bases

Medico_dfs_geral <- 
          Medico_dfs_geral |> 
              left_join(hierarquia_atualizada, 
                        by = c("regiao_saude"="cod_regsaud")) |> 
              rename(cod_regiao_saude = regiao_saude,
                     nome_regiao_saude = regiao_saude.y)

# Analises ----------------------------------------------------------------
# 1) Boxplot por regiao ------------------------------------------------------

medianas_regiao <- Medico_dfs_geral %>%
  rename(Região = regiao) %>%
  mutate(Região = str_replace(Região, "^Região ", "")) %>%
  group_by(Região) %>%
  summarize(mediana = median(retencao_geral, na.rm = TRUE)) %>%
  ungroup()

Medico_dfs_geral |> 
  rename(Região = regiao) |>
  mutate(Região = str_replace(Região, "^Região ", "")) |> 
  ggplot(aes(x= fct_reorder(Região, retencao_geral, 
                           .desc = TRUE), y=retencao_geral), 
         las=5) +
    geom_boxplot(aes(fill=Região)) + theme_minimal() + xlab("Região") + 
    ylab("Taxa de retenção") + 
    scale_y_continuous(limits = c(0, 1), 
                       breaks = seq(0, 1, by = 0.25)) + 
  
    theme(
      axis.text.x = element_text(size = 16),  
      axis.text.y = element_text(size = 16),
      legend.text = element_text(size = 16),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      legend.position = "none"
    ) +
  geom_text(data = medianas_regiao, 
            aes(x = Região, y = mediana, label = round(mediana, 2)),
            position = position_nudge(x = 0.2, 
                                      y = -0.02), 
            size = 5)

# Calculando medidas resumo da variável "retencao_geral" por Região

media_uf <- 
  Medico_dfs_geral |> 
  group_by(uf) |> 
  summarise(media = mean(retencao_geral))


mean(Medico_dfs_geral$retencao_geral)

# Construindo boxplot por UF ----------------------------------------------

Medico_dfs_geral <- Medico_dfs_geral |> 
                      mutate(regiao_order = 
                               case_when(regiao == "Região Sul" ~ 1,
                                         regiao == "Região Sudeste" ~ 2,
                                         regiao == "Região Centro-Oeste" ~ 3,
                                         regiao == "Região Norte" ~ 4,
                                         regiao == "Região Nordeste" ~ 5))

# por uf

median(Medico_dfs_geral$retencao_geral)

# Calculando a mediana para cada grupo
medianas <- Medico_dfs_geral %>%
  rename(Região = regiao) %>%
  group_by(uf, Região) %>%
  summarize(mediana = median(retencao_geral), .groups = 'drop') |> 
  filter(uf != "Distrito Federal")

# Criando o gráfico por UF
Medicos_regioes <- 
  Medico_dfs_geral |>
  rename(Região = regiao) |>
  mutate(Região = str_replace(Região, "^Região ", "")) 

Medicos_regioes |> 
  filter(uf != "Distrito Federal") |> 
  ggplot(aes(x = fct_reorder(uf, regiao_order, .desc = TRUE), 
             y = retencao_geral)) +
  geom_boxplot(aes(fill = Região)) +
  coord_flip() +
  geom_hline(yintercept = 0.510, 
             linetype = "dashed", 
             color = "red") +
  theme_minimal() +
  xlab("UF") +
  ylab("Taxa de Retenção") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) + 
  geom_text(data = medianas, aes(x = uf, y = mediana, label = round(mediana, 2)), 
            hjust = -0.3, size = 4) +  # Ajuste hjust e size conforme necessário
  theme(
    axis.text.x = element_text(size = 16),  
    axis.text.y = element_text(size = 16),
    legend.position = "bottom"
  )


# Retencao vs densidade ---------------------------------------------------


retencao_uf <- Medico_dfs_geral |> 
                    group_by(cod_uf, uf, regiao) |> 
                    summarise(media_retencao = mean(retencao_geral))

razao <- read_excel("0_dados/razao_medicos.xlsx")


tbl_uf <- razao |> 
  inner_join(retencao_uf, by = "cod_uf")

# Calcular o coeficiente de correlação e o valor p
cor_test <- cor.test(tbl_uf$media_retencao, tbl_uf$Razão)

# Extraindo o coeficiente de correlação e o valor p
r <- round(cor_test$estimate, 3)
p_value <- cor_test$p.value
p_text <- ifelse(p_value < 0.01, "p < 0.01", paste("p =", round(p_value, 3)))

# Criar o gráfico com a anotação do coeficiente de correlação
tbl_uf |> 
  rename(Região = regiao) |> 
  mutate(Região = str_replace(Região, "^Região ", "")) |> 
  ggplot(aes(x = media_retencao, y = Razão)) + 
  geom_point() + 
  geom_label(aes(label = UF, fill = Região)) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  xlab("Retenção") + 
  ylab("Razão de médicos por 1000 habitantes") +
  scale_x_continuous(limits = c(0, 1)) +  
  scale_y_continuous(limits = c(0, 7)) +  
  theme(
    text = element_text(size = 16),          
    axis.title = element_text(size = 14),    
    axis.text = element_text(size = 14),     
    legend.title = element_text(size = 16),  
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  ) +
  annotate("text", x = 0.8, y = 6.5, 
           label = paste("r =", r, ",", p_text),
           size = 6, hjust = 1)


r <- cor.test(tbl_uf$Razão, tbl_uf$media_retencao)
r


# Mapa de regiões de saúde ------------------------------------------------

# Carregar shapefile GeoJSON e converter para sf
spdf <- geojson_read("1_scripts/shape file regioes saude.json", what = "sp")
spdf_fortified <- sf::st_as_sf(spdf)

# Definir o CRS de latam para ser o mesmo que o do shapefile carregado
sf::st_crs(latam) <- sf::st_crs(spdf_fortified)

# Converter códigos de região de saúde para inteiros
Medico_dfs_geral$cod_regiao_saude <- as.integer(Medico_dfs_geral$cod_regiao_saude)

# Coordenadas das capitais
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

# Definir limites de longitude e latitude para focar no Brasil
limite_long <- c(-75, -28)  # limites de longitude
limite_lat <- c(-33, 4)     # limites de latitude

# Criar o mapa
mapa <- spdf_fortified |>
  left_join(Medico_dfs_geral, by = c("reg_id"="cod_regiao_saude")) |>
  rename(Retenção = retencao_geral) |> 
  ggplot() +
  geom_sf(data = latam, fill = "lightgrey", color = "black") + 
  geom_sf(aes(fill = Retenção)) +
  geom_point(data = capitais_coord, aes(x = longitude, y = latitude), color = "blue", size = 1) +
  geom_text_repel(data = capitais_coord, aes(label = municipio, longitude, y = latitude)) +
  xlab("Longitude") + ylab("Latitude") +
  theme_minimal() +
  scale_fill_gradient(low = "#d43621", high = "#91e17c", n.breaks = 5) +
  coord_sf(xlim = limite_long, ylim = limite_lat)

mapa + 
  ggspatial::annotation_north_arrow(
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"))
