library(tidyverse)
library(lubridate)
library(ggchicklet)


praca_rs <- readr::read_csv(
    "c:\\Users\\italo\\OneDrive\\Área de Trabalho\\GitHub\\NESPro\\.vscode\\Dataset/GadoGordo.csv"
  ) |>
  dplyr::filter(preco == "Médio") |>
  dplyr::select(!c(pv_macho, pv_femea, pc_femea), boi_rs = pc_macho) |>
  dplyr::mutate(
    boi_rs = boi_rs * 15,
    #pc_femea = pc_femea * 15
  ) |> dplyr::select(-preco)

praca_sp <- readr::read_csv("c:\\Users\\italo\\OneDrive\\Área de Trabalho\\GitHub\\Cepea-Esalq\\.github\\.vscode\\Dataset cepea\\Cepea-Boi-Gordo.csv" # nolint
  ) |>
  dplyr::select(data, boi_sp = a_vista_r)


# Realizar o join entre os datasets de RS e SP
dados_join <- praca_rs |>
  dplyr::inner_join(praca_sp, by = "data")


# Calcular o diferencial de base
dados_join <- dados_join |>
  dplyr::mutate(
    diferencial_base_nominal = boi_sp - boi_rs,
    diferencial_base_percentual = ((boi_rs / boi_sp) - 1) * 100
    #diferencial_base_percentual = ((boi_rs - boi_sp) /boi_sp) * 100
    )


# Adicionar uma coluna com o mês
dados_join <- dados_join |>
  mutate(mes = month(data, label = TRUE, abbr = TRUE))

# Calcular a média histórica por mês
media_historica <- dados_join |>
  group_by(mes) |>
  summarise(media_diferencial = mean(diferencial_base_percentual, na.rm = TRUE))


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

title_text <- glue::glue('**Diferencial de base:**') # nolint
subtitle_text <- glue::glue("**Percentual histórico do diferencial de base para cada mês do preços do boi gordo<br>entre as praças do Rio Grande do Sul (RS) e São Paulo (SP) de 2015 a 2024.**")
caption_text <- glue::glue(
  "**Dados:** Núcleo de Estudos em Sistemas de Produção de Bovinos de Corte e Cadeia Produtiva (NESPro) e Centro de Estudos Avançados em Economia Aplicada (Cepea-Esalq)<br>", # nolint
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


# Criar o gráfico
plot_historico <- media_historica |>
  ggplot(aes(x = mes, y = media_diferencial, fill = media_diferencial > 0)) +
  geom_chicklet(aes(fill = media_diferencial > 0), radius = unit(5, "pt"),
    color = "grey20",
    alpha = 0.7,
    size = 1.5) +
  #geom_hline(yintercept = 0, color = "grey20", linewidth = 1.5) +
  scale_y_continuous(breaks = seq(-3, 9.3, 3), labels = scales::dollar_format(
    prefix = "", suffix = "%")) +
  geom_text(aes(label = round(media_diferencial, 2)), 
            vjust = -0.5,
            family = "Open Sans",
            fontface = "bold",
            size = 5,
            color = "grey20") +
  scale_fill_manual(values = c("#c2671f", "#3995ab")) +
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
  ".github\\.vscode\\Graph index\\Diferencial de base.png",
  plot = plot_historico,
  width = 14,
  height = 10.4,
  dpi = 300)