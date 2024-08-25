library(tidyverse)  # Carrega pacotes para manipulação de dados e visualização

sazonalidade_media <- readr::read_csv("c:\\Users\\italo\\OneDrive\\Área de Trabalho\\GitHub\\Cepea-Esalq\\.github\\.vscode\\Dataset cepea\\Cepea-Boi-Gordo.csv") |> 
  select(data, boi_sp = a_vista_r) |> # Seleciona as colunas `data` e `boi_sp`, renomeando `a_vista_r` para `boi_sp`
  #filter(year(data) >= 2014 & year(data) <= 2019) |> # Filtra os dados para incluir apenas os anos entre 2014 e 2019
  mutate(mes = month(data, label = TRUE), ano = year(data)) |> # Cria colunas `mes` e `ano` extraídas da coluna `data`
  group_by(ano, mes) |> # Agrupa os dados por `ano` e `mes`
  summarise(media_mensal = mean(boi_sp, na.rm = TRUE)) |> # Calcula a média mensal dos preços por mês e ano
  group_by(ano) |> # Reagrupa os dados apenas por `ano`
  mutate(media_anual = mean(media_mensal, na.rm = TRUE)) |> # Calcula a média anual dos preços por ano
  mutate(sazonalidade = 100 * (media_mensal - media_anual) / media_anual) |># Calcula a sazonalidade como a diferença percentual entre a média mensal e a média anual
  group_by(mes) |> # Agrupa os dados por `mes`
  summarise(sazonalidade_media = mean(sazonalidade, na.rm = TRUE)) # Calcula a média da sazonalidade por mês

# Gera o gráfico da sazonalidade média
ggplot(sazonalidade_media, aes(x = mes, y = sazonalidade_media)) +
  geom_col(fill = "orange") +
  labs(
    title = "Sazonalidade média dos preços em São Paulo – 1997 a 2024 (%)",
    x = "Mês",
    y = "Sazonalidade (%)") +
  theme_minimal()