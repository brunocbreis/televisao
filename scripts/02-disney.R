
# setup -------------------------------------------------------------------

source('scripts/00-setup.R')


# disney ------------------------------------------------------------------

fim_de_semana <- c("Saturday", "Sunday")

disney %>% 
  filter(data %>% month() == 1) %>% 
  mutate(categoria = str_to_title(categoria),
         categoria = if_else(categoria == "Serie", "Série", categoria)
         ) %>% 
  group_by(data, categoria) %>% 
  summarise(insercoes = n(),
            tempo_de_tela = sum(duracao %>% period_to_seconds(), na.rm = TRUE) / 60) %>%
  mutate(fds = data %>% weekdays() %in% fim_de_semana,
         fds = if_else(fds == TRUE, 1/0, 0)) %>% 
  ggplot(aes(x = data, y = tempo_de_tela, color = categoria)) +

  labs(title = "Filme ou série?",
       subtitle = "O que estava na tela do Disney Channel em jan/2018",
       caption = "Fonte: Disney",
       y = "Tempo de tela (minutos)") +
  scale_y_continuous(breaks = seq(0,1500,250)) +
  # scale_x_date(date_breaks = "weeks",
  #              date_minor_breaks = "days") +
  geom_line(size = 1,
            alpha = .75) +
  theme_fivethirtyeight(base_family = "Commissioner") +
  theme(legend.title = element_blank(),
        line = element_line(size = .15),
        axis.title.y = element_text(size = 8),
        plot.margin = margin(50,25,10,10))


disney %>% 
  filter(data %>% month() == 1,
         categoria == "filme") %>%
  # mutate(pgm = fct_lump_min(pgm, 3, other_level = "Outros")) %>% 
  count(pgm) %>% 
  slice_max(n, n = 10) %>%
  mutate( pgm = fct_reorder(pgm, n)) %>% 
  ggplot() +
  geom_col(aes(pgm, n), width = .05, alpha = .5) +
  geom_point(aes(pgm, n, color = pgm), show.legend = FALSE) +
  scale_y_continuous(breaks = seq(0,10,2)) +
  coord_flip()+
  theme_fivethirtyeight(base_family = "Commissioner") +
  theme(legend.title = element_blank(),
        line = element_line(size = .15),
        # axis.title.y = element_text(size = 8),
        plot.margin = margin(50,25,10,10))
  
  


disney %>% 
  group_by(pgm, data) %>% 
  summarise(tempo_de_tela = sum(duracao %>% period_to_seconds(), na.rm = TRUE) / 60) %>% 
  arrange(data) %>% view()
  ggplot() + 
  geom_line(aes(
    color = pgm,
    x = data,
    y = tempo_de_tela
    )
            )


andi_mack <- disney %>% 
  filter(pgm == "Andi Mack") %>% 
  select(-titulo_episodio, -sinopse_1, -sinopse_2) %>% 
  mutate(duracao = difftime(hora_saida, hora_entrada) %>% as.period(),
         .after = episodio)

view(andi_mack)

disney$data %>% weekdays()


