library(readxl)
library(tidyverse)
library(ggplot2)
library(GGally)

data_retencao <- read_excel("~/GitHub/retencao/0_dados/retencao_geral_completo.xlsx")

#Gerando gráfico
grafico <- 
  ggpairs(data_retencao,
        columns = c("retencao_medicos", "retencao_enfermeiros", 
                    "retencao_dentistas", "retencao_tec_aux_enf"),
        upper = list(continuous = "cor"),
        lower = list(continuous = "points"),
        mapping = aes(color = regiao),
        title = "Gráficos de Dispersão: Taxas de Retenção por Ocupação",
        xlab = "Taxa de Retenção",
        ylab = "Taxa de Retenção") + theme_minimal()

grafico

ggsave(grafico, filename = "grafico.jpeg",
       width = 3000, height = 3000, units = "px", dpi = 300)
