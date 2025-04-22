library(readxl)
library(tidyverse)
library(ggplot2)
library(GGally)
library(patchwork)

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
query <- 'SELECT * FROM Dados.retencao."Cirurgiões-dentistas_retencao_geral.parquet"'
Dentistas_dfs_geral <- sqlQuery(channel, query, 
                                as.is = TRUE)


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
query <- 'SELECT * FROM Dados.retencao."Enfermeiro_retencao_geral.parquet"'
Enfermeiros_dfs_geral <- sqlQuery(channel, query, 
                                  as.is = TRUE)

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
query <- 'SELECT * FROM Dados.retencao."Técnicos e auxiliares de enfermagem_retencao_geral.parquet"'
Tec_aux_enf_dfs_geral <- sqlQuery(channel, query, 
                                  as.is = TRUE)

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
query <- 'SELECT * FROM Dados.retencao."Técnico ou Auxiliar de Saúde Bucal_retencao_geral.parquet"'
Tec_aux_sb_dfs_geral <- sqlQuery(channel, query, 
                                 as.is = TRUE)


Medico_dfs_geral <- Medico_dfs_geral |> 
  rename(retencao_medicos = retencao_geral)

Dentistas_dfs_geral <- Dentistas_dfs_geral |> 
  rename(retencao_dentistas = retencao_geral)

Enfermeiros_dfs_geral <- Enfermeiros_dfs_geral |> 
  rename(retencao_enfermeiros = retencao_geral)

Tec_aux_enf_dfs_geral <- Tec_aux_enf_dfs_geral |> 
  rename(retencao_tec_aux_enf = retencao_geral)

Tec_aux_sb_dfs_geral <- Tec_aux_sb_dfs_geral |> 
  rename(retencao_tec_aux_sb = retencao_geral)

lista_retencao <- list(Medico_dfs_geral, Dentistas_dfs_geral, Enfermeiros_dfs_geral,
                       Tec_aux_enf_dfs_geral, Tec_aux_sb_dfs_geral)

data_retencao <- reduce(lista_retencao, left_join, by = "regiao_saude")

hierarquia_completa <- 
  read_csv("0_dados/hierarquia_atualizada.csv") |> 
  select(cod_regsaud, regiao) |> 
  mutate(cod_regsaud = as.character(cod_regsaud))

data_retencao <- data_retencao |> 
  left_join(hierarquia_completa, by = c("regiao_saude" = "cod_regsaud"))


#Gerando gráfico
grafico <- 
  ggpairs(
    data_retencao,
    columns = c("retencao_medicos", "retencao_enfermeiros", 
                "retencao_dentistas", "retencao_tec_aux_enf", "retencao_tec_aux_sb"),
    upper = list(continuous = "cor"),
    lower = list(continuous = "points"),
    mapping = aes(color = regiao),
    labeller = as_labeller(c(
      retencao_medicos = "Médicos",
      retencao_enfermeiros = "Enfermeiros",
      retencao_dentistas = "Dentistas",
      retencao_tec_aux_enf = "Téc./Aux. Enfermagem",
      retencao_tec_aux_sb = "Téc./Aux. Saúde Bucal"
    )),
    title = "Gráficos de Dispersão: Taxas de Retenção por Ocupação"
  ) + theme_minimal()

grafico

ggsave(grafico, filename = "grafico_matriz_dispersao.svg",
       width = 6000, height = 4000, units = "px", dpi = 500)

#Baixando a base
profissionais_uf <- read_excel("~/GitHub/retencao/0_dados/profissionais_conselhos.xlsx")

profissionais_uf <- profissionais_uf %>%
  mutate(UF = recode(uf_sigla, 
                     "AC" = "Acre",
                     "AL" = "Alagoas",
                     "AP" = "Amapá",
                     "AM" = "Amazonas",
                     "BA" = "Bahia",
                     "CE" = "Ceará",
                     "DF" = "Distrito Federal",
                     "ES" = "Espírito Santo",
                     "GO" = "Goiás",
                     "MA" = "Maranhão",
                     "MT" = "Mato Grosso",
                     "MS" = "Mato Grosso do Sul",
                     "MG" = "Minas Gerais",
                     "PA" = "Pará",
                     "PB" = "Paraíba",
                     "PR" = "Paraná",
                     "PE" = "Pernambuco",
                     "PI" = "Piauí",
                     "RJ" = "Rio de Janeiro",
                     "RN" = "Rio Grande do Norte",
                     "RS" = "Rio Grande do Sul",
                     "RO" = "Rondônia",
                     "RR" = "Roraima",
                     "SC" = "Santa Catarina",
                     "SP" = "São Paulo",
                     "SE" = "Sergipe",
                     "TO" = "Tocantins")) |> 
  select(-uf_sigla) |> 
  relocate(UF, .before = profissionais)


#Baixando a base
pop_uf <- read_excel("~/GitHub/retencao/0_dados/censo_uf.xlsx")

#Juntando as bases

profissionais_hab <-  profissionais_uf |> 
  left_join(pop_uf, by = c("UF" = "UF"))

#Calculando a quantidade de profissionais por mil habitantes
profissionais_hab <- profissionais_hab |> 
  mutate(razao_prof_hab = (qntd_profissionais/populacao)*1000) |> 
  select(UF, profissionais, razao_prof_hab)


#Mudando a estrutura de longo para largo
profissionais_hab <- profissionais_hab %>%
  pivot_wider(names_from = profissionais, values_from = razao_prof_hab,
              names_prefix = "razao_")

#Calculandando a mediana da taxa de retenção por UF para cada uma das categorias
retencao_UF <- data_retencao %>%
  group_by(uf.x, regiao) %>%
  summarize(retencao_medicos = median(retencao_medicos), 
            retencao_enfermeiros = median(retencao_enfermeiros),
            retencao_dentista = median(retencao_dentistas),
            retencao_tec_aux_enf = median(retencao_tec_aux_enf)
            , .groups = 'drop')

#Juntando as medianas de retenção por UF com a razao de profissionais por habitantes

ret_prof_hab <- retencao_UF |> 
  left_join(profissionais_hab, by = c("uf.x" = "UF"))

ret_prof_hab <- ret_prof_hab |> 
  rename(razao_dentistas = `razao_Cirurgiões-Dentistas`) |> 
  rename(razao_tec_aux_enf = `razao_Técnicos e Auxiliares de Enfermagem`)

ret_prof_hab <- ret_prof_hab %>%
  mutate(uf.x = recode(uf.x, 
                     "Acre" = "AC",
                     "Alagoas" = "AL",
                     "Amapá" = "AP",
                     "Amazonas" = "AM",
                     "Bahia" = "BA",
                     "Ceará" = "CE",
                     "Distrito Federal" = "DF",
                     "Espírito Santo" = "ES",
                     "Goiás" = "GO",
                     "Maranhão" = "MA",
                     "Mato Grosso" = "MT",
                     "Mato Grosso do Sul" = "MS",
                     "Minas Gerais" = "MG",
                     "Pará" = "PA",
                     "Paraíba" = "PB",
                     "Paraná" = "PR",
                     "Pernambuco" = "PE",
                     "Piauí" = "PI",
                     "Rio de Janeiro" = "RJ",
                     "Rio Grande do Norte" = "RN",
                     "Rio Grande do Sul" = "RS",
                     "Rondônia" = "RO",
                     "Roraima" = "RR",
                     "Santa Catarina" = "SC",
                     "São Paulo" = "SP",
                     "Sergipe" = "SE",
                     "Tocantins" = "TO"))

#Gerando gráfico de dispersão (Médicos)

g1 <- ggplot(ret_prof_hab, aes(x = retencao_medicos, y = razao_Médicos, label = uf.x, color = regiao)) +
  geom_point(size = 5) +
  geom_text(vjust = -1.5, hjust = 0.5, check_overlap = TRUE, color = "black", fontface = "bold") +  # Cor preta e negrito para as etiquetas
  scale_color_manual(values = c("Região Centro-Oeste" = "red", "Região Nordeste" = "green", "Região Norte" = "cyan",
                                "Região Sudeste" = "blue", "Região Sul" = "purple")) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Retenção", y = "Médicos por 1000 habitantes", color = "Região") +
  theme_minimal() +
  theme(legend.position = "right")

#Gerando gráfico de dispersão (Enfermeiros)

g2 <- ggplot(ret_prof_hab, aes(x = retencao_enfermeiros, y = razao_Enfermeiros, label = uf.x, color = regiao)) +
  geom_point(size = 5) +
  geom_text(vjust = -1.5, hjust = 0.5, check_overlap = TRUE, color = "black", fontface = "bold") +  # Cor preta e negrito para as etiquetas
  scale_color_manual(values = c("Região Centro-Oeste" = "red", "Região Nordeste" = "green", "Região Norte" = "cyan",
                                "Região Sudeste" = "blue", "Região Sul" = "purple")) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Retenção", y = "Enfermeiros por 1000 habitantes", color = "Região") +
  theme_minimal() +
  theme(legend.position = "right")


#Gerando gráfico de dispersão (Cirugiões-dentistas)

g3 <- ggplot(ret_prof_hab, aes(x = retencao_dentista, y = razao_dentistas, label = uf.x, color = regiao)) +
  geom_point(size = 5) +
  geom_text(vjust = -1.5, hjust = 0.5, check_overlap = TRUE, color = "black", fontface = "bold") +  # Cor preta e negrito para as etiquetas
  scale_color_manual(values = c("Região Centro-Oeste" = "red", "Região Nordeste" = "green", "Região Norte" = "cyan",
                                "Região Sudeste" = "blue", "Região Sul" = "purple")) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Retenção", y = "Cirurgiões-dentista por 1000 habitantes", color = "Região") +
  theme_minimal() +
  theme(legend.position = "right")

#Gerando gráfico de dispersão (Téc e Aux de Enfermagem

g4 <- ggplot(ret_prof_hab, aes(x = retencao_tec_aux_enf, y = razao_tec_aux_enf, label = uf.x, color = regiao)) +
  geom_point(size = 5) +
  geom_text(vjust = -1.5, hjust = 0.5, check_overlap = TRUE, color = "black", fontface = "bold") +  # Cor preta e negrito para as etiquetas
  scale_color_manual(values = c("Região Centro-Oeste" = "red", "Região Nordeste" = "green", "Região Norte" = "cyan",
                                "Região Sudeste" = "blue", "Região Sul" = "purple")) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Retenção", y = "Téc. e Aux. de Enfermagem por 1000 habitantes", color = "Região") +
  theme_minimal() +
  theme(legend.position = "right")

g1 <- g1 + ggtitle("Médicos")
g2 <- g2 + ggtitle("Enfermeiros")
g3 <- g3 + ggtitle("Cirurgiões-dentistas")
g4 <- g4 + ggtitle("Técnicos e Auxiliares de Enfermagem")

combined_plot <- (g1 + g2) / (g3 + g4) +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

ggsave("combined_plot.png", combined_plot, width = 18, height = 14)
print(combined_plot)

#MQO

modelo <- lm(retencao_medicos ~ retencao_enfermeiros + retencao_dentista
             + retencao_tec_aux_enf, data = ret_prof_hab)
summary(modelo)

modelo2 <- lm(retencao_enfermeiros ~ retencao_medicos + retencao_enfermeiros + retencao_dentista
             + retencao_tec_aux_enf, data = ret_prof_hab)
summary(modelo2)


modelo <- lm(razao_Médicos ~ retencao_medicos, data = ret_prof_hab)

# Ver os coeficientes do modelo
coeficientes <- coef(modelo)
coeficientes

# Extrair a inclinação da reta (coeficiente de retencao_medicos)
inclinacao <- coeficientes["retencao_medicos"]
inclinacao