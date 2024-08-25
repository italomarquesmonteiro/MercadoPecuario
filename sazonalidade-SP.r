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


# Adicionar fontes
font <- "Josefin Sans"
font2 <- "Open Sans"
font3 <- "Permanent Marker"
sysfonts::font_add_google(family=font, font, db_cache = FALSE)
sysfonts::font_add_google(family=font2, font2, db_cache = FALSE)
sysfonts::font_add_google(family=font3, font3, db_cache = FALSE)
sysfonts::font_add(family = "Font Awesome 6 Brands", regular = "C:/Users/italo/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf") # nolint
showtext::showtext_opts(dpi = 300)
showtext::showtext_auto(enable = TRUE)

github_icon <- "&#xf09b"
linkedin_icon <- "&#xf0e1"
x_icon <- "&#xf099"
instagram_icon <- "&#xf16d"
github_username <- "italomarquesmonteiro"
linkedin_username <- "italomarquesmonteiro"
x_username <- "italommonteiro"
instagram_username <- "italo.m.m"

bg <- "white"
txt_col <- "black"
colors <- "gray40"
fundo <- "white"

title_text <- glue::glue('**Sazonalidade:**') # nolint
subtitle_text <- glue::glue("**Média dos preços do boi em São Paulo – 1997 a 2024 (%).**")
caption_text <- glue::glue(
  "**Dados:** Centro de Estudos Avançados em Economia Aplicada (Cepea-Esalq)<br>", # nolint
  #"**Nota:** As Unidades da Federação, Amapá, Distrito Federal e Paraíba não apresentaram dados<br>",
  "**Plot:** Ítalo Marques-Monteiro <br><br>",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: black;'>{github_icon};</span> 
  <span style='color: black'>{github_username}</span><br>",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: dodgerblue4;'>{linkedin_icon};</span> 
  <span style='color: black'>{linkedin_username}</span><br>",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: steelblue;'>{x_icon};</span>
  <span style='color: black'>{x_username}</span><br>",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: red;'>{instagram_icon};</span>
  <span style='color: black'>{instagram_username}</span>"
)
# Gera o gráfico da sazonalidade média
plot_sazonalidade <- sazonalidade_media |>
  #geom_col(fill = "orange") +
    ggplot( aes(x = mes, y = sazonalidade_media)) +
    ggchicklet::geom_chicklet(
    fill = "orange",
    radius = unit(5, "pt"),
    color = "grey20",
    alpha = 0.7,
    size = 1.5) +
    scale_y_continuous(breaks = seq(-4, 8, 2), labels = scales::dollar_format(
    prefix = "", suffix = "%")) +
  geom_text(aes(label = round(sazonalidade_media, 2)), 
            vjust = -0.5,
            family = "Open Sans",
            fontface = "bold",
            size = 5,
            color = "grey20") +
  labs(
       title = title_text,
       subtitle = subtitle_text,
       caption = caption_text,
       y = "Percentual (%)"
   ) +
   theme(
        plot.title = ggtext::element_markdown(face = "bold", family = font3, size = 35, hjust = 0, color = "grey30"),
        plot.subtitle = ggtext::element_markdown(face = "italic", family = font, size = 25, hjust = 0, color = "grey30"),
        plot.caption = ggtext::element_markdown(family = font, hjust = 0, margin = margin(10,0,0,0), size = 12, color = txt_col, lineheight = 1.2),
        panel.background = element_rect(fill = "white", color = "grey90"),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "grey90"),
        legend.position = "none",
        legend.text = element_text(color = "grey40", size = 12),
        legend.title = element_text(face = "bold", color = "grey40"),
        axis.title.x = ggtext::element_markdown(face = "bold", family = font2, size = 13, color = "gray40"),
        axis.title.y = element_blank(),
        axis.text.y = ggtext::element_markdown(face = "bold", family = font2, size = 13, color = "gray40"),
        axis.text.x = ggtext::element_markdown(face = "bold", family = font2, size = 13, color = "gray40")
    )

ggsave(
  ".github\\.vscode\\Graph index\\Sazonalidade-preço-boi-SP.png",
  plot = plot_sazonalidade,
  width = 14,
  height = 10.4,
  dpi = 300)