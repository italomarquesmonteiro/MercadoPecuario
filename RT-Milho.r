#
library(tidyverse)

dados_boi <- readr::read_csv(
    "c:\\Users\\italo\\OneDrive\\Área de Trabalho\\GitHub\\NESPro\\.vscode\\Dataset\\GadoGordo.csv") |>
    dplyr::filter(preco == "Médio") |>
    dplyr::select(data, arroba_boi = pc_macho) |>
    dplyr::mutate(arroba_boi = arroba_boi * 15) |>
    dplyr::mutate(mes_ano = floor_date(data, "month")) |>  # Criar coluna de mês e ano
    dplyr::group_by(mes_ano) |>  # Agrupar por mês e ano
    dplyr::summarise(arroba = mean(arroba_boi, na.rm = TRUE))  # Calcular média por mês

dados_milho <- readr::read_csv(
    "c:\\Users\\italo\\OneDrive\\Área de Trabalho\\GitHub\\Emater\\.github\\.vscode\\Dataset\\Emater-Cotações.csv") |>
    dplyr::filter(Produtos %in% c("Arroz", "Soja", "Milho", "Sorgo")) |>
    dplyr::select(data = Data, produto = Produtos, medio = Médio) |>  # Criar coluna de mês e ano
    dplyr::distinct(data, produto, .keep_all = TRUE) |>  # Remove duplicatas
    tidyr::pivot_wider(names_from = "produto", values_from = "medio") |>
    dplyr::mutate(mes_ano = floor_date(data, "month")) |>  # Criar coluna de mês e ano
    dplyr::group_by(mes_ano) |>  # Agrupar por mês e ano
    dplyr::summarise(
        milho = mean(Milho, na.rm = TRUE),
        arroz = mean(Arroz, na.rm = TRUE),
        soja = mean(Soja, na.rm = TRUE),
        sorgo = mean(Sorgo, na.rm = TRUE),
    )  # Calcular média por mês


 rt_com_boi <- dados_boi |>
    dplyr::inner_join(dados_milho) |>
    dplyr::mutate(
        rt_milho = arroba / milho,
        rt_arroz = arroba / arroz,
        rt_soja =  arroba / soja,
        rt_sorgo = arroba / sorgo
    ) |>
    dplyr::arrange(desc(mes_ano)) 

test <- rt_com_boi |>
    dplyr::select(mes_ano, rt_milho, rt_arroz, rt_soja, rt_sorgo) |>
    tidyr::pivot_longer(!mes_ano, names_to = "produto", values_to = "valor") |>
    dplyr::mutate(
        produto = case_when(
            produto == "rt_milho" ~ "Milho (saca 60 kg)",
            produto == "rt_arroz" ~ "Arroz (saca 50 kg)",
            produto == "rt_soja" ~ "Soja (saca 60 kg)",
            produto == "rt_sorgo" ~ "Sorgo (saca 60 kg)",
            TRUE ~ NA_character_
        ),
        cor_produto = case_when(
            produto == "Milho (60 kg)" ~ "#dc9f4a",
            produto == "Arroz (50 kg)" ~ "#fbeea9",
            produto ==  "Soja (60 kg)" ~ "#ae7438",
            produto == "Sorgo (60 kg)" ~ "#ecc26b",
            TRUE ~ NA_character_
        )
    ) #|> dplyr::filter(mes_ano >= "2023-10-01")
    
 # Calculando a média para cada produto
media_per <- test  |>
    dplyr::group_by(produto) |>
    dplyr::summarise(media_periodo = mean(valor, na.rm = TRUE))

# Unindo os dados originais com as médias por produto
test_com_media <- test |>
    dplyr::left_join(media_per, by = "produto")

# Adicionar fontes
font <- "Josefin Sans"
font2 <- "Open Sans"
font3 <- "Permanent Marker"
sysfonts::font_add_google(family = font, font, db_cache = FALSE)
sysfonts::font_add_google(family = font2, font2, db_cache = FALSE)
sysfonts::font_add_google(family = font3, font3, db_cache = FALSE)
sysfonts::font_add(family = "Font Awesome 6 Brands", regular = "C:/Users/italo/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")
showtext::showtext_opts(dpi = 300)
showtext::showtext_auto(enable = TRUE)

# Adicionar captions
whatsapp <- "&#xf232"
youtube <- "&#xf16a"
github_icon <- "&#xf09b"
linkedin_icon <- "&#xf0e1"
twitter_icon <- "&#xf099"
x_icon <- "&#xe61b"
instagram_icon <- "&#xf16d"
github_username <- "italomarquesmonteiro"
linkedin_username <- "italomarquesmonteiro"
x_username <- "italommonteiro"
instagram_username <- "italo.m.m"

bg <- "white"
txt_col <- "grey70"
colors <- "gray40"
title_text <- glue::glue("Relaçao de troca")
subtitle_text <- glue::glue("**Sacas de grãos que podem ser compradas no Rio Grande do Sul por cada arroba de boi gordo vendida**")
caption_text <- glue::glue(
  "**Dados:**  Empresa de Assistência Técnica e Extensão Rural (Emater). Núcleo de Estudos em Sistemas de Produção de Bovinos de Corte e Cadeia Produtiva (NESPro).<br>",
  "**Nota:** As linhas contínuas nos gráficos representam a média do período para cada tipo de grão..<br>",
  "**Plot:** Ítalo Marques-Monteiro <br><br>",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: white;'>&#xf09b;</span> 
   <span style='color: grey70'>{github_username}</span> | ",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: steelblue;'>&#xf0e1;</span> 
   <span style='color: grey70'>{linkedin_username}</span> | ",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: white;'>&#xe61b;</span> 
   <span style='color: grey70'>{x_username}</span> | ",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: brown1;'>&#xf16d;</span> 
   <span style='color: grey70'>{instagram_username}</span>"
)


# Plotando o gráfico com facet_wrap e geom_hline
grafico <- test_com_media |>
    ggplot(aes(mes_ano, valor)) +  # Usando a variável categórica 'produto' para o 'fill'
    geom_col(fill = "gray60") +
    #guides(fill = "none") +  # Remover a legenda de cores, se desejado
    #geom_text(aes(label = round(valor, 1)), vjust = -0.5, color = "grey40", size = 3) +  # Adicionando os rótulos dos valores
    scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, 1)) +
    geom_hline(aes(yintercept = media_periodo), color = "grey80", lty = 1) +
    facet_wrap(~produto) +
    labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        y = "n",
        x = "Período") +
    theme(
    plot.title = ggtext::element_markdown(face = "bold", family = font3, size = 30, color = "gray80", hjust = 0.5),
    plot.subtitle = ggtext::element_markdown(face = "italic", family = font2, size = 13, color = "gray70", hjust = 0.5),
    plot.caption = ggtext::element_markdown(face = "bold", family = font, size = 8, color = txt_col, hjust = 0, margin = margin(10, 20, 20, 20),  lineheight = 1.2),
    panel.background = element_rect(fill = "grey25", color = "grey25"),
    panel.grid = element_line(color = "grey25"),
    panel.grid.major.y = element_line(color = "grey27", size = 0.5, linetype = 1),
    panel.grid.minor.y = element_line(color = "grey27", size = 0.5, linetype = 1),
    plot.background = element_rect(fill = "grey25"),
    legend.position = "none",
    
    axis.title.x = ggtext::element_markdown(face = "bold", family = font2, size = 12, color = "gray40"),
    axis.title.y = ggtext::element_markdown(face = "bold", family = font2, size = 12, color = "gray40"),
    
    axis.text.y = ggtext::element_markdown(face = "bold", family = font2, size = 10, color = "gray40"),
    axis.text.x = ggtext::element_markdown(face = "bold", family = font2, size = 4, color = "gray40", angle = 0),
    axis.ticks.length.x = unit(-0.3, "cm"),
    axis.ticks.y = element_line(color = "#FFFFFF00", size = 0.3),
    axis.ticks.x = element_line(color = "grey40", size = 0.3),
    strip.text = ggtext::element_markdown(color = "grey20", size = 14, face = "bold", family = font)
  )

ggsave(
    ".github\\.vscode\\Graph index\\RT-Arroba-Grãos.png",
    plot = grafico,
    width = 10,
    height = 8,
    dpi = 300 )