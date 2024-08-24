# Carregar os pacotes necessários
library(tidyverse)


# Ler e preparar os dados
praca_sp <- readr::read_csv("c:\\Users\\italo\\OneDrive\\Área de Trabalho\\GitHub\\Cepea-Esalq\\.github\\.vscode\\Dataset cepea\\Cepea-Boi-Gordo.csv") |>
  dplyr::select(data, boi_sp = a_vista_r) 

# Convertendo a data em mês e ano
praca_sp <- praca_sp |> 
  mutate(mes = month(data, label = TRUE),
         ano = year(data))

# Calculando a média mensal
media_mensal <- praca_sp |> 
  group_by(ano, mes) |> 
  summarise(media_mensal = mean(boi_sp, na.rm = TRUE))

# Calculando a média anual por ano
media_anual <- praca_sp |> 
  group_by(ano) |> 
  summarise(media_anual = mean(boi_sp, na.rm = TRUE))


# Juntando as médias mensais com as médias anuais
sazonalidade <- media_mensal |> 
  left_join(media_anual, by = "ano") |> 
  mutate(sazonalidade = 100 * (media_mensal - media_anual) / media_anual)


# Calculando a sazonalidade média por mês
sazonalidade_media <- sazonalidade |> 
  group_by(mes) |> 
  summarise(sazonalidade_media = mean(sazonalidade, na.rm = TRUE))


library(ggplot2)

# Plotando a sazonalidade média
ggplot(sazonalidade_media, aes(x = mes, y = sazonalidade_media)) +
  geom_col(fill = "orange") +
  labs(title = "Sazonalidade média dos preços em São Paulo – 1997 a 2024 (%)",
       x = "Mês",
       y = "Sazonalidade (%)") +
  theme_minimal()
