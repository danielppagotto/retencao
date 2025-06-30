library(tidyverse)
library(readxl)
library(geojsonio)
library(ggrepel)
library(ggspatial)
library(sf)


retencao_anestesiologista <- read.csv("C:/Users/alefs/OneDrive/Documentos/LAPEI-CIGETS/GitHub/retencao/0_dados/Anestesiologista_retencao_geral.csv",
                                 sep = ";") %>% 
  select(-X)

# Dados da hierarquia

hierarquia_completa <- 
  read_csv("0_dados/hierarquia_atualizada.csv") %>% 
  mutate(cod_regsaud = as.integer(cod_regsaud))

hierarquia_atualizada <- 
  hierarquia_completa |> 
  select(regiao, cod_uf, uf, 
         cod_regsaud, regiao_saude) |> 
  distinct(regiao, cod_uf, uf, 
           cod_regsaud, regiao_saude)

data_anestesiologista <- hierarquia_atualizada %>% 
  left_join(retencao_anestesiologista, by = c("cod_regsaud" = "regiao_saude")) %>% 
  mutate(retencao_geral = if_else(is.na(retencao_geral), 0, retencao_geral))

spdf <- geojson_read("1_scripts/shape file regioes saude.json", what = "sp")
spdf_fortified <- sf::st_as_sf(spdf) %>% 
  select(reg_id, geometry)

st_crs(spdf_fortified) <- 4326


# Coordenadas das capitais
capitais <- c("1100205","1302603","1200401","5002704","1600303","5300108",
              "1400100","5103403","1721000","3550308","2211001","3304557",
              "1501402","5208707","2927408","4205407","2111300","2704302",
              "4314902","4106902","3106200","2304400","2611606","2507507",
              "2800308","2408102","3205309")

# Definir limites de longitude e latitude para focar no Brasil
limite_long <- c(-75, -28)  # limites de longitude
limite_lat <- c(-33, 4)     # limites de latitude

capitais_coord <- 
  hierarquia_completa |>
  mutate(cod_municipiodv = as.character(cod_municipiodv)) |> 
  filter(cod_municipiodv %in% capitais) |> 
  select(cod_municipio, municipio, longitude, latitude) |> 
  mutate(latitude = as.numeric(latitude))

cloropetico <- spdf_fortified |>
  left_join(data_anestesiologista, by = c("reg_id" = "cod_regsaud")) %>% 
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
    colours = c("grey100","#fee391", "#2b8cbe"),
    values = scales::rescale(c(0, 0.1, 1)),
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

ggsave(cloropetico, filename = "cloropetico_retencao_anestesiologista.svg",
       width = 3000, height = 2500, units = "px", dpi = 300)
