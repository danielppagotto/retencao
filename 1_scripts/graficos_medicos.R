
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
library(arrow)
library(patchwork)
library(geojsonsf)
library(scales)
library(leaflet)

# Importando dados --------------------------------------------------------

# codigo para acessar dados de datalake proprio 
dremio_host <- Sys.getenv("endereco")
dremio_port <- Sys.getenv("port")
dremio_uid <- Sys.getenv("uid")
dremio_pwd <- Sys.getenv("datalake")
channel <- odbcDriverConnect(
  sprintf("DRIVER=Dremio Connector;
                             HOST=%s;
                             PORT=%s;
                             UID=%s;
                             PWD=%s;
                                     AUTHENTICATIONTYPE=Basic Authentication;
                                     CONNECTIONTYPE=Direct", 
          dremio_host, 
          dremio_port, 
          dremio_uid, 
          dremio_pwd))
query <- 'SELECT * FROM Dados.retencao."Médico_retencao_geral.parquet"'
Medico_dfs_geral <- sqlQuery(channel, query, 
                     as.is = TRUE)

# Dados da hierarquia

hierarquia_completa <- 
  read_csv("0_dados/hierarquia_atualizada.csv")


hierarquia_atualizada <- 
  hierarquia_completa |> 
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

ranking_10maiores <- Medico_dfs_geral |> 
  select(regiao,uf, nome_regiao_saude, retencao_geral) |> 
  mutate(retencao = round(retencao_geral*100,0)) |> 
  slice_max(retencao_geral, n = 10)

ranking_10menores <- Medico_dfs_geral |> 
  select(regiao, uf, nome_regiao_saude, retencao_geral) |> 
  mutate(retencao = round(retencao_geral*100,0)) |> 
  slice_min(retencao_geral, n = 10)

  

# Analises ----------------------------------------------------------------
# 1) Boxplot por regiao ------------------------------------------------------

medianas_regiao <- Medico_dfs_geral %>%
  rename(Região = regiao) %>%
  mutate(Região = str_replace(Região, "^Região ", "")) %>%
  group_by(Região) %>%
  summarize(mediana = median(retencao_geral, na.rm = TRUE)) %>%
  ungroup()

grafico_regiao <- 
  Medico_dfs_geral |> 
  rename(Região = regiao) |>
  mutate(Região = str_replace(Região, "^Região ", "")) |> 
  ggplot(aes(x= fct_reorder(Região, retencao_geral, 
                           .desc = TRUE), y=retencao_geral), 
         las=5) +
  geom_boxplot(fill= 	"#5DADE2", color = "#595959") + 
  theme_minimal() + 
  xlab("") + 
    ylab("Taxa de retenção") + 
    scale_y_continuous(limits = c(0, 1), 
                       breaks = seq(0, 1, by = 0.25)) + 
  
    theme(
      axis.text.x = element_text(size = 16),  
      axis.text.y = element_text(size = 16),
      legend.text = element_text(size = 16),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16, face = "bold", color = "#595959"),
      legend.position = "none"
    ) +
  scale_y_continuous(
    limits = c(0,1),
    breaks = seq(0, 1, by = 0.2),
    label = scales::label_percent(accuracy = 1)
  ) +
  geom_text(data = medianas_regiao, 
            aes(x = Região,
                y = mediana, 
                label = paste0(round(mediana*100, 0), "%")),
            position = position_nudge(x = 0.2, 
                                      y = -0.02), 
            size = 4,
            fontface = "bold")


ggsave(grafico_regiao, filename = "retencao_boxplot_medicos_regiao.svg",
       width = 4300, height = 3500, units = "px", dpi = 500)


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

grafico_uf <- 
  Medicos_regioes |> 
  filter(uf != "Distrito Federal") |> 
  ggplot(aes(x = fct_reorder(uf, regiao_order, .desc = TRUE), 
             y = retencao_geral)) +
  geom_boxplot(aes(fill = Região), color = "#595959") +
  coord_flip() +
  geom_hline(yintercept = 0.510, 
             linetype = "dashed", 
             color = "red") +
  scale_fill_discrete(name = NULL) +
  theme_minimal() +
  xlab("") +
  ylab("Taxa de Retenção") +
  scale_y_continuous(limits = c(0, 1), 
                     breaks = seq(0, 1, by = 0.25),
                     label = scales::label_percent(accuracy = 1)) + 
  geom_text(data = medianas, aes(x = uf, y = mediana, label = paste0(round(mediana*100,0), "%")), 
            hjust = -0.3, 
            size = 4,
            fontface = "bold") +  # Ajuste hjust e size conforme necessário
  theme(
    axis.text.x = element_text(size = 16),  
    axis.text.y = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.position = "bottom"
  )

ggsave(grafico_uf, filename = "retencao_boxplot_medicos_uf.svg",
       width = 5500, height = 5000, units = "px", dpi = 500)

# Retencao vs densidade ---------------------------------------------------


retencao_uf <- Medico_dfs_geral |> 
                    group_by(cod_uf, uf, regiao) |> 
                    summarise(media_retencao = mean(retencao_geral))

razao <- read_excel("~/GitHub/retencao/0_dados/razao_medicos.xlsx")


tbl_uf <- razao |> 
  inner_join(retencao_uf, by = "cod_uf")

tbl_uf <- tbl_uf |> 
  filter(uf != "Distrito Federal")

# Calcular o coeficiente de correlação e o valor p
cor_test <- cor.test(tbl_uf$media_retencao, tbl_uf$Razão)
cor_test

# Extraindo o coeficiente de correlação e o valor p
r <- round(cor_test$estimate, 3)
p_value <- cor_test$p.value
p_text <- ifelse(p_value < 0.01, "p < 0.01", paste("p =", round(p_value, 3)))

# Criar o gráfico com a anotação do coeficiente de correlação




grafico_razao <- 
  tbl_uf |> 
  rename(Região = regiao) |> 
  mutate(Região = str_replace(Região, "^Região ", "")) |> 
  ggplot(aes(x = media_retencao, y = Razão)) + 
  geom_point() + 
  geom_label(aes(label = UF, fill = Região)) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  labs(fill = "") +
  xlab("Taxa de retenção") + 
  ylab("Razão de médicos por 1000 habitantes") +
  scale_x_continuous(limits = c(0.35, 0.7),
                     labels = scales::percent_format(accuracy = 1)) +  
  scale_y_continuous(limits = c(0, 4)) +  
  theme(
    text = element_text(size = 16),          
    axis.title = element_text(size = 14),    
    axis.text = element_text(size = 14),     
    legend.title = element_text(size = 16),  
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  ) +
  annotate("text", x = 0.65, y = 1, 
           label = paste("r =", r, ",", p_text),
           size = 6, hjust = 1)
grafico_razao

r <- cor.test(tbl_uf$Razão, tbl_uf$media_retencao)
r

ggsave(grafico_razao, filename = "correlacao_medicos.svg",
       width = 3000, height = 2500, units = "px", dpi = 300)

# Mapa de regiões de saúde ------------------------------------------------

# Carregar shapefile GeoJSON e converter para sf
spdf <- geojson_read("1_scripts/shape file regioes saude.json", what = "sp")
spdf_fortified <- sf::st_as_sf(spdf)


# Definir o CRS de latam para ser o mesmo que o do shapefile carregado
st_crs(spdf_fortified) <- 4326

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
  left_join(Medico_dfs_geral, by = c("reg_id" = "cod_regiao_saude")) |>
  rename(Retenção = retencao_geral) |> 
  ggplot() +
  geom_sf(data = spdf_fortified, fill = "lightgrey", color = "#bbbbbb", alpha = 0.8) + 
  geom_sf(aes(fill = Retenção)) +
  geom_point(data = capitais_coord, aes(x = longitude, y = latitude), color = "blue", size = 1) +
  geom_text_repel(
    data = capitais_coord,
    aes(label = municipio, x = longitude, y = latitude),
    size = 4.5,
    fontface = "bold"
  ) +
  xlab("Longitude") + ylab("Latitude") +
  theme_minimal() +
  scale_fill_gradientn(
    colours = c("#fee391", "#fee391", "#a6bddb", "#2b8cbe"),
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    labels = scales::percent_format(accuracy = 1),
    name = "Taxa de retenção",
  ) +
  coord_sf(xlim = limite_long, ylim = limite_lat) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"
    )
  ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  theme(
    legend.position = c(0.95, 0.05),
    legend.justification = c(1, 0),
    legend.box = "horizontal",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold" ,margin = margin(b = 10)),
    plot.title = element_text(size = 14),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.margin = margin(10, 10, 10, 10)
  )




ggsave(mapa, filename = "retencao_mapa_medicos.svg",
       width = 3000, height = 2500, units = "px", dpi = 300)
