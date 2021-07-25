# carregar setup ----------------------------------------------------------

source('scripts/00-setup.R')

# programas por inserções -------------------------------------------------
nick %>% 
  group_by(pgm) %>% 
  summarise(insercoes = n()) %>% 
  arrange(desc(insercoes)) 

# pgms por tempo no ar ----------------------------------------------------

nick %>%
  #wrangle
  mutate(
    tempo_insercao = difftime(hora_saida, hora_entrada, units = 'mins'),
    .after = hora_saida
  ) %>%
  group_by(pgm) %>%
  mutate(pgm = str_replace(pgm, "ICarly", "iCarly"),
         pgm = str_remove(pgm, "HD"),
         pgm = str_trim(pgm),
         pgm = str_replace(pgm,"Dicky and", "Dicky\n and")) %>% 
  summarise(n_insercoes = n(),
            tempo_no_ar = sum(tempo_insercao)) %>%
  arrange(desc(tempo_no_ar)) %>%
  slice_max(order_by = tempo_no_ar, n = 8) %>%
  
  #plot
  ggplot(aes(reorder(pgm, tempo_no_ar),
             as.double(tempo_no_ar),
             fill = pgm)) +
  geom_col(alpha = .7,
           width = .55) +
  coord_flip() +
  labs(
    title = "Os programas da Nick com mais tempo de tela",
    subtitle = "Maio de 2018",
    fill = "Programa",
    y = "minutos no ar"
  ) +
  
  #theme
  theme_fivethirtyeight(base_family = "Commissioner") +
  theme(
    title = element_text(margin = unit(c(1,1,1,1), "cm")),
    line =  element_line(size = .15),
    strip.background = element_rect(color = "white"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.title.y = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = "#FFFFFF"),
    plot.margin = margin(30,30,15,15),
    plot.title.position = "plot")
  
  ggsave(paste0("graficos/", today(), "-tempo_no_ar_nick.png"))

nick %>% 
  view()
