# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(extrafont)
library(ggtext)
Sys.setlocale(category = "LC_TIME", "pt_BR")


# importar base -----------------------------------------------------------

gnews_2019_2020 <- read_rds('dados/gnews/gnews_2019_2020_clean.rds')


# gráfico jornal globonews ------------------------------------------------

gnews_2019_2020 %>%
  
  # manipular os dados
  filter(pgm == 'Jornal Globonews') %>%
  mutate(
    data = floor_date(data, unit = "weeks"),
    ano = year(data) %>% as_factor(),
    data = if_else(year(data) == 2019, data, data - years(1) + days(1))
  ) %>%
  filter(year(data) >= 2019) %>%
  group_by(data, ano) %>%
  summarise(tempo_no_ar = sum(duracao_segundos) / 60 / 60) %>%
  
  # o plot básico
  ggplot(aes(data, tempo_no_ar)) +
  geom_line(aes(color = ano), size = 1.2) +
  
  # texto e linha pra marcar o dia fatídico da pandeminha
  geom_vline(
    xintercept = ymd('2019-03-11'),
    linetype = 2,
    size = .35,
    color = rgb(170, 42, 35, maxColorValue = 255),
    alpha = .65
  ) +
  geom_richtext(
    aes(
      x = ymd('2019-03-13'),
      y = 30,
      label = "<b>11/03/2020</b><br>OMS declara pandemia"
    ),
    vjust = 1,
    hjust = 0,
    family = 'Commissioner',
    color = rgb(170, 42, 35, maxColorValue = 255),
    size = 2.8,
    nudge_y = 0,
    label.size = 0,
    fill = NA,
    label.padding = grid::unit(rep(0, 4), "pt")
  ) +
  
  # escalas e coordenadas
  scale_y_continuous(breaks = seq(0, 90, 10),
                     minor_breaks = 5) +
  scale_x_date(
    date_breaks = 'months',
    limits = c(ymd('2018-12-30'), ymd('2019-12-25')),
    date_labels = '%b',
    date_minor_breaks = 'weeks'
  ) +
  coord_cartesian(ylim = c(25, 85),
                  expand = FALSE) +
  scale_color_manual(values = c(rgb(170, 42, 35, maxColorValue = 255),
                                rgb(.75, .75, .75))[2:1]) +
  
  # labels e tema
  labs(
    title = "Cobertura ao vivo na pandemia",
    subtitle = "Variação no tempo de exibição semanal do Jornal da Globonews",
    x = 'Mês',
    y = 'Horas no ar semanais',
    caption = 'Gráfico por @brunocbreis | Fonte: Globo'
  ) +
  theme_bw(base_family = 'Commissioner') +
  theme(
    plot.title = element_text(face = 'bold', size = 16),
    plot.title.position = 'plot',
    plot.margin = margin(20, 30, 20, 20),
    legend.title = element_blank(),
    legend.position = 'top',
    legend.margin = margin(t = 0, b = -10),
    axis.title.x = element_blank(),
    plot.caption = element_text(color = rgb(.6, .6, .6)),
    panel.grid = element_blank()
  )

# salvar o png
ggsave(
  paste0(
    'graficos/',
    today(),
    '_',
    now() %>% hour,
    '-',
    now() %>% minute(),
    '_gnews_pandemia.png'
  ),
  width = 7,
  height = 5,
  dpi = 500
)
