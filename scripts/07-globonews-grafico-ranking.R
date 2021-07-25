# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(extrafont)
library(ggtext)
Sys.setlocale(category = "LC_TIME", "pt_BR") 

# importar base -----------------------------------------------------------

gnews_2019_2020 <- read_rds('dados/gnews/gnews_2019_2020_clean.rds')
transmissao <- read_csv2('dados/gnews/pgms_ao_vivo_pedro_atualizado.csv')


top_pgms_2020 <- gnews_2019_2020 %>% 
  mutate(ano = year(data)) %>% 
  filter(categoria == 'pgm',
         ano == 2020) %>%
  group_by(pgm) %>% 
  summarise(tempo_de_tela = sum(duracao_segundos)/60/60) %>% 
  slice_max(tempo_de_tela, n = 10) %>% 
  pull(pgm)

top_pgms_2019 <- gnews_2019_2020 %>% 
  mutate(ano = year(data)) %>% 
  filter(categoria == 'pgm',
         ano == 2019) %>%
  group_by(pgm) %>% 
  summarise(tempo_de_tela = sum(duracao_segundos)/60/60) %>% 
  slice_max(tempo_de_tela, n = 10) %>% 
  pull(pgm)

  
# plot absoluto ruim ------------------------------------------------------


gnews_2019_2020 %>% 
  mutate(
         categoria = replace_na(categoria, 'vht'),
         ano = year(data)
         ) %>% 
  filter(
    pgm %in% top_pgms_2020
    ) %>% 
  select(pgm, ano, duracao_segundos) %>% 
  group_by(ano, pgm) %>% 
  summarise(tempo_de_tela = sum(duracao_segundos)/60/60) %>% 
  slice_max(order_by = tempo_de_tela, n=5) %>% 
  left_join(transmissao) %>% 
  mutate(transmissao = str_to_lower(transmissao),
         ano = as_factor(ano)) %>% 
  
  ggplot() +
  geom_point(aes(x = reorder(pgm, tempo_de_tela), y = tempo_de_tela, color = ano, shape = transmissao),
             size = 3) +
  coord_flip()





# plot relativo ruim ---------------------------------------------------------------


gnews_2019_2020 %>%
  mutate(categoria = replace_na(categoria, 'vht'),
         ano = year(data)) %>%
  filter(pgm %in% top_pgms_2020) %>%
  select(pgm, ano, duracao_segundos) %>%
  group_by(ano, pgm) %>%
  summarise(tempo_de_tela = sum(duracao_segundos) / 60 / 60) %>%
  slice_max(order_by = tempo_de_tela, n = 10) %>%
  left_join(transmissao) %>%
  mutate(transmissao = str_to_lower(transmissao),
         ano = as_factor(ano)) %>%
  pivot_wider(names_from = ano, values_from = tempo_de_tela) %>% 
  janitor::clean_names() %>% 
  mutate(x2020 = x2020/x2019,
         x2019 = 1) %>% 
  pivot_longer(cols = 3:4, names_to = 'ano', values_to = 'tempo_de_tela') %>%
  mutate(ano = str_remove(ano, 'x'),
         ano = as_factor(ano),
         transmissao = str_to_title(transmissao)) %>% 
  
  ggplot() +
  geom_point(aes(x = reorder(pgm, tempo_de_tela), y = tempo_de_tela, color = ano, shape = transmissao),
             size = 4) +
  coord_flip() +
  scale_color_manual(values = c(rgb(170, 42, 35, maxColorValue = 255),
                                rgb(.75, .75, .75))[2:1]) +
  guides(color = 'none') +
  
  labs(
    title = 'Tempo de tela relativo dos programas da Globonews em <span style = "color: #B9B9B9;">2019</span> e <span style = "color: #AA2A23;">2020</span>'
  ) +
  
  theme_bw(base_family = 'Commissioner') +
  theme(
    plot.title = element_markdown(face = 'bold', size = 16),
    plot.title.position = 'plot',
    plot.margin = margin(20, 30, 20, 20),
    legend.title = element_blank(),
    legend.position = 'top',
    legend.margin = margin(t = 0, b = 0),
    axis.title = element_blank(),
    axis.text.y = element_text(face = 'bold'),
    plot.caption = element_text(color = rgb(.6, .6, .6)),
    panel.grid = element_blank()
  )





# dumbbell plot -----------------------------------------------------------


library(ggalt)


gnews_2019_2020 %>%
  mutate(categoria = replace_na(categoria, 'vht'),
         ano = year(data)) %>%
  filter(pgm %in% top_pgms_2020) %>%
  select(pgm, ano, duracao_segundos) %>%
  group_by(ano, pgm) %>%
  summarise(tempo_de_tela = sum(duracao_segundos) / 60 / 60) %>%
  slice_max(order_by = tempo_de_tela, n = 10) %>%
  left_join(transmissao) %>%
  mutate(transmissao = str_to_lower(transmissao),
         ano = as_factor(ano)) %>%
  pivot_wider(names_from = ano, values_from = tempo_de_tela) %>% 
  janitor::clean_names() %>% 
  mutate(x2020 = x2020/x2019,
         x2019 = 1) %>% 
  mutate(transmissao = if_else(transmissao == 'vivo', 'Ao Vivo', 'Gravado')) %>% 
  
  ggplot() +
  geom_vline(aes(xintercept = 1), size = .25, alpha = 1, color = rgb(.8,.8,.8)) +
  geom_dumbbell(aes(y = reorder(pgm, x2020), x = x2019, xend = x2020, shape = transmissao),
             size = .5,
             size_x = 3.5,
             colour = rgb(.15,.15,.15),
             size_xend = 3.5,
             dot_guide_size = .5,
             dot_guide_colour = rgb(.8,.8,.8),
             dot_guide = TRUE,
             colour_x = '#B9B9B9',
             colour_xend = '#AA2A23',
             show.legend = T) +
  scale_color_manual(values = c(rgb(170, 42, 35, maxColorValue = 255),
                                rgb(.75, .75, .75))[2:1]) +
  scale_shape_manual(values = c(15,19)) +
  scale_y_discrete(labels = function(x) {if_else(x %in% top_pgms_2019, paste0('<b>',x,'</b>'),x)}) +
  scale_x_continuous(labels = function(x) { (x * 100) %>% format(digits = 0) %>% paste0('%')}) +
  guides(color = 'none',
         shape = guide_legend(override.aes = list(size = 3, color = '#B9B9B9'))) +
  
  labs(
    title = 'Tempo de tela relativo dos programas da Globonews em <span style = "color: #B9B9B9;">2019</span> e <span style = "color: #AA2A23;">2020</span>',
    subtitle = 'Pandemia alterou o padrão de transmissão dos programas mais exibidos do canal',
    caption = 'Em <b style = "color:black">negrito</b>, programas entre os 10 mais exibidos em 2019<br>Gráfico por @brunocbreis | Fonte: Globo'
  ) +
  
  theme_bw(base_family = 'Commissioner') +
  theme(
    plot.title = element_markdown(face = 'bold', size = 15),
    plot.title.position = 'plot',
    plot.margin = margin(20, 30, 20, 20),
    plot.subtitle = element_text(margin = margin(0,0,20,0)),
    legend.title = element_blank(),
    legend.position = c(.9,0.1),
    legend.background = element_blank(),
    legend.margin = margin(t = 0, b = 0),
    axis.title = element_blank(),
    axis.text.y = element_markdown(margin = margin(0,5,0,0)),
    axis.ticks.y = element_blank(),
    plot.caption = element_markdown(color = rgb(.6, .6, .6)),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(color = rgb(.9,.9,.9), size = .25)
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
    '_gnews_top_programas.png'
  ),
  width = 7.5,
  height = 5,
  dpi = 500
)
