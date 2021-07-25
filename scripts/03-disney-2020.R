


# carregar pacotes --------------------------------------------------------

library(tidyverse) # pra vida
library(lubridate) # pra lidar com datas
library(extrafont) # pra usar fontes
library(ggthemes) # pra gráficos bonitos
library(prismatic) # pra ver as cores


# importar bases ----------------------------------------------------------



meses <- 1:12 
meses <- if_else(meses < 10, paste0("0", meses), as.character(meses))



url_base <- "https://programacao.disney.com.br/br/resources/historic/"


# funções de importação ---------------------------------------------------


# local
importar_base_disney <- function(ano, mes) {
  read_delim(file = paste0("dados/", ano, "_", meses[mes], "_dchd.csv"),
           delim = "\\", trim_ws = TRUE,
           escape_double = FALSE,
           col_names = FALSE,
           col_types = cols(
             X1 = col_date(format = "%Y%m%d"),
             X2 = col_time(format = "%H%M%S"),
             X3 = col_time(format = "%H%M%S"),
             X9 = col_integer()
           )
)
}

#web
importar_base_disney_web <- function(ano,mes) {
  read_delim(file = paste0(url_base, ano, "_", meses[mes], "_dchd.csv"),
             delim = "\\", trim_ws = TRUE,
             escape_double = FALSE,
             col_names = FALSE,
             col_types = cols(
               X1 = col_date(format = "%Y%m%d"),
               X2 = col_time(format = "%H%M%S"),
               X3 = col_time(format = "%H%M%S"),
               X9 = col_integer()
             )
  )
  }

# disney_2019_raw <- map2_dfr(2019,1:12, importar_base_disney_web)

# write_rds(disney_2019_raw, "dados/disney_2019.rds")

disney_2019_raw <- read_rds("dados/disney_2019.rds")


meses[1:5]
disney_2020 <- map2_dfr(2020, 1:5, importar_base_disney)

disney <- disney_2020 %>% 
  rename(data = X1,
         hora_entrada = X2,
         hora_saida = X3,
         pgm = X4,
         titulo_episodio = X5,
         temporada = X6,
         episodio = X7,
         criadores = X8,
         ano_producao = X9,
         sinopse_1 = X10,
         sinopse_2 = X11) %>% 
  drop_na(hora_entrada, hora_saida) %>% 
  mutate(temporada = as.numeric(temporada),
         hora_entrada = ymd_hms(paste(data,hora_entrada)),
         hora_saida = ymd_hms(paste(data, hora_saida)), 
         duracao =
           difftime(hora_saida,hora_entrada) %>% as.period(),
         hora_saida = if_else(
           duracao < 0,
           hora_saida + days(1),
           hora_saida
         ),
         duracao = difftime(hora_saida,hora_entrada) %>% as.period() %>% floor(),
         categoria = 
           if_else(
             is.na(episodio),
             "filme",
             "serie"
           ),
         criadores = case_when(
           pgm == "Andi Mack" ~ "Terri Minsky",
           TRUE ~ criadores
         )
  ) %>% 
  select(data, hora_entrada, hora_saida, 
         duracao, pgm, categoria, titulo_episodio, 
         temporada, episodio, criadores, 
         ano_producao)

# view(disney)

disney_2019 <- disney_2019_raw %>% 
  rename(data = X1,
         hora_entrada = X2,
         hora_saida = X3,
         pgm = X4,
         titulo_episodio = X5,
         temporada = X6,
         episodio = X7,
         criadores = X8,
         ano_producao = X9,
         sinopse_1 = X10,
         sinopse_2 = X11) %>% 
  drop_na(hora_entrada, hora_saida) %>% 
  mutate(temporada = as.numeric(temporada),
         hora_entrada = ymd_hms(paste(data,hora_entrada)),
         hora_saida = ymd_hms(paste(data, hora_saida)), 
         duracao =
           difftime(hora_saida,hora_entrada) %>% as.period(),
         hora_saida = if_else(
           duracao < 0,
           hora_saida + days(1),
           hora_saida
         ),
         duracao = difftime(hora_saida,hora_entrada) %>% as.period() %>% floor(),
         categoria = 
           if_else(
             is.na(episodio),
             "filme",
             "serie"
           ),
         criadores = case_when(
           pgm == "Andi Mack" ~ "Terri Minsky",
           TRUE ~ criadores
         )
  ) %>% 
  select(data, hora_entrada, hora_saida, 
         duracao, pgm, categoria, titulo_episodio, 
         temporada, episodio, criadores, 
         ano_producao)




# filmes e séries ---------------------------------------------------------

fim_de_semana <- c("Saturday", "Sunday")

disney %>% 
  filter(data %>% month() < 6) %>% 
  mutate(categoria = str_to_title(categoria),
         categoria = if_else(categoria == "Serie", "Série", categoria)
  ) %>% 
  group_by(data, categoria) %>% 
  summarise(insercoes = n(),
            tempo_de_tela = sum(duracao %>% period_to_seconds(), na.rm = TRUE) / 60) %>%
  mutate(fds = data %>% weekdays() %in% fim_de_semana,
         fds = if_else(fds == TRUE, 1/0, 0)) %>% 
  ggplot(aes(x = data, y = tempo_de_tela, fill = categoria)) +
  
  labs(title = "Filme ou série?",
       subtitle = "O que estava na tela do Disney Channel de janeiro a maio de 2020",
       caption = "Fonte: Disney",
       y = "Tempo de tela (minutos)") +
  # scale_y_continuous(breaks = seq(0,1500,250)) +
  # scale_x_date(date_breaks = "weeks",
  #              date_minor_breaks = "days") +
  geom_col(size = 1,
            alpha = .75, position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  theme_fivethirtyeight(base_family = "Commissioner") +
  theme(legend.title = element_blank(),
        line = element_line(size = .15),
        # axis.title.y = element_text(size = 8),
        plot.margin = margin(50,25,10,10))           
           


# andi mack ---------------------------------------------------------------

# primeiro listar todas as semanas e temporadas da base
temporadas_andi_mack <- disney %>% 
  filter(pgm == "Andi Mack") %>% 
  mutate(temporada = if_else(episodio %in% 13:38, 2, temporada),
         temporada = as.character(temporada) %>% paste0("Temporada ", .),
         semana = floor_date(data, "week")) %>% 
  distinct(temporada) %>% 
  arrange(temporada) %>% 
  pull(temporada)

semanas_andi_mack <- disney %>% 
  filter(pgm == "Andi Mack") %>% 
  mutate(temporada = if_else(episodio %in% 13:38, 2, temporada),
         temporada = as.character(temporada) %>% paste0("Temporada ", .),
         semana = floor_date(data, "week")) %>% 
  distinct(semana) %>% 
  arrange(semana) %>% 
  pull()

semanas_e_temporadas <- NULL

for (i in 1:3) {
  semanas_e_temporadas <- bind_rows(semanas_e_temporadas,
                                    tibble(semana = semanas_andi_mack, temporada = temporadas_andi_mack[i]))
}


semanas_e_temporadas %>% view()
# tudo isso pra poder manter os zeros no gráfico

cores_andi_mack <- paste0("#",c("3b88af","e4988a","69e1d4"))

color(cores_andi_mack)

disney %>%
  filter(pgm == "Andi Mack") %>%
  mutate(
    temporada = if_else(episodio %in% 13:38, 2, temporada),
    temporada = as.character(temporada) %>% paste0("Temporada ", .),
    semana = floor_date(data, "week")
  ) %>%
  group_by(semana, temporada) %>% 
summarise(insercoes = n()) %>% 
  # complete(temporada = c("Temporada 1", "Temporada 2", "Temporada 3")) %>%
  # complete(insercoes, nesting(temporada, semana)) %>%  view()
  # full_join(semanas_e_temporadas) %>%
  replace_na(list(insercoes = 0)) %>%
  ggplot(aes(semana + days(1), insercoes, fill = temporada)) +
  geom_col(size = 0.5,
            alpha = 1,
            show.legend = TRUE,
           position = "stack") +
  # geom_point(size = 2) +
  scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 9)) +
  scale_x_date(date_breaks = "weeks",
               date_labels = '%V',) +
  scale_fill_manual(values = cores_andi_mack) +
  coord_cartesian(xlim = c(semanas_andi_mack[2] - days(2), semanas_andi_mack[length(semanas_andi_mack)] - days(3))) +
  labs(
    title = "Qual temporada de Andi Mack estava passando?",
    subtitle = "Exibição de episódios de Andi Mack no Disney Channel de janeiro a maio de 2020",
    x = "Semana",
    y = "Episódios no ar"
  ) +
  theme_fivethirtyeight(base_family = "Commissioner") +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.title.position = "plot",
    legend.title = element_blank(),
    line = element_line(size = .15),
    axis.title = element_text(size = 10, color = 128),
    axis.text = element_text(size = 10),
    plot.margin = margin(20, 20, 10, 20),
    legend.margin = margin(0,0,0,0)
  )

ggsave(paste0('graficos/', today(), '_temporadas_andi_mack.png'))


# episódios de andi mack 2020 sem divisão por temporada -------------------------


disney %>% 
  filter(pgm == "Andi Mack") %>% 
  mutate(semana = floor_date(data, "week")) %>% 
  group_by(semana) %>% 
  summarise(insercoes = n()) %>% 
  ggplot(aes(x = semana + days(1),
             y = insercoes)) +
  geom_line(size = 0.5, color = "#3b88af",
            alpha = .5) +
  geom_point(size = 2, color = "#3b88af") +
  scale_y_continuous(breaks = seq(0, 10, 1), limits = c(2.9, 10)) +
  scale_x_date(date_breaks = "weeks",
               date_labels = '%V') +
  coord_cartesian(xlim = c(semanas_andi_mack[2] - days(2), semanas_andi_mack[length(semanas_andi_mack)] - days(3))) +
  labs(
    title = "Quantos episódios de Andi Mack foram ao ar?",
    subtitle = "Exibição de episódios de Andi Mack no Disney Channel de janeiro a maio de 2020",
    x = "Semana",
    y = "Episódios no ar"
  ) +
  theme_fivethirtyeight(base_family = "Commissioner") +
  theme(
    plot.subtitle = element_text(size = 10),
    legend.title = element_blank(),
    line = element_line(size = .15),
    axis.title = element_text(size = 9, color = 128),
    # axis.text = element_text(color= 128),
    plot.margin = margin(50, 25, 20, 20)
  )




semanas_andi_mack_2019 <- disney_2019 %>% 
  # filter(pgm == "Andi Mack") %>% 
  mutate(semana = floor_date(data, "week")) %>% 
  distinct(semana) %>% 
  arrange(semana) %>% 
  pull()

semanas_e_temporadas_2019 <- NULL

for (i in 1:3) {
  semanas_e_temporadas_2019 <- bind_rows(semanas_e_temporadas_2019,
                                    tibble(semana = semanas_andi_mack_2019, temporada = temporadas_andi_mack[i]))
}


disney_2019 %>% 
  filter(pgm == "Andi Mack") %>% 
  mutate(semana = floor_date(data, "week")) %>% 
  group_by(semana) %>% 
  summarise(insercoes = n()) %>% 
  ggplot(aes(x = semana + days(1),
             y = insercoes)) +
  geom_line(size = 0.5, color = "#3b88af",
            alpha = .5) +
  geom_point(size = 2, color = "#3b88af") +
  # scale_y_continuous(breaks = seq(0, 10, 1), limits = c(2.9, 10)) +
  # scale_x_date(date_breaks = "4 weeks",
  #              date_labels = '%V') +
  # coord_cartesian(xlim = c(semanas_andi_mack_2019[2] - days(2), semanas_andi_mack_2019[length(semanas_andi_mack_2019)] - days(3))) +
  labs(
    title = "Quantos episódios de Andi Mack foram ao ar?",
    subtitle = "Exibição de episódios de Andi Mack no Disney Channel em 2019",
    x = "Semana",
    y = "Episódios no ar"
  ) +
  theme_fivethirtyeight(base_family = "Commissioner") +
  theme(
    plot.subtitle = element_text(size = 10),
    legend.title = element_blank(),
    line = element_line(size = .15),
    axis.title = element_text(size = 9, color = 128),
    # axis.text = element_text(color= 128),
    plot.margin = margin(50, 25, 20, 20)
  )



disney_2019 %>%
  filter(pgm == "Andi Mack") %>%
  mutate(
    temporada = if_else(episodio %in% 13:39, 2, temporada),
    temporada = if_else(ano_producao == 2019, 3, temporada),
    temporada = as.character(temporada) %>% paste0("Temporada ", .),
    semana = floor_date(data, "week")
  ) %>% 
  group_by(semana, temporada) %>%
  summarise(insercoes = n()) %>%
  full_join(semanas_e_temporadas_2019) %>% 
  replace_na(list(insercoes = 0)) %>% 
  # view()
  ggplot(aes(semana + days(1), insercoes, color = temporada)) +
  geom_line(size = 0.5,
            alpha = .5,
            show.legend = FALSE) +
  geom_point(size = 2, alpha = .8) +
  scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 9)) +
  scale_x_date(date_breaks = "months",
               date_labels = '%V',) +
  scale_color_manual(values = cores_andi_mack) +
  coord_cartesian(xlim = c(semanas_andi_mack_2019[2] - days(2), semanas_andi_mack_2019[length(semanas_andi_mack_2019)] - days(3))) +
  labs(
    title = "Qual temporada de Andi Mack estava passando?",
    subtitle = "Exibição de episódios de Andi Mack no Disney Channel em 2019",
    x = "Semana",
    y = "Episódios no ar"
  ) +
  theme_fivethirtyeight(base_family = "Commissioner") +
  theme(
    plot.subtitle = element_text(size = 10),
    legend.title = element_blank(),
    line = element_line(size = .15),
    axis.title = element_text(size = 9, color = 128),
    # axis.text = element_text(color= 128),
    plot.margin = margin(50, 25, 20, 20)
  )

ggsave('graficos/temporadas_andimack_2019.png', width = 8, height = 5)





disney %>%
  filter(categoria == "serie") %>% 
  distinct(pgm) %>% 
  mutate(pgm = str_to_title(pgm)) %>% 
  view()
  





write_rds(disney_2019, "dados/disney_2019_clean.rds")





disney %>% 
  filter(pgm == "Andi Mack") %>% 
  mutate(temporada = if_else(episodio %in% 13:38, 2, temporada),
         semana = floor_date(data, "week") %>% as.character() %>% as_factor()) 

class(disney$temporada)

disney[disney$pgm == "Andi Mack",]$episodio %in% 13:38

disney %>% 
  filter(pgm == "Andi Mack") %>% 
  select(temporada, episodio, titulo_episodio) %>% 
  arrange(episodio) %>% 
  distinct() %>% view()
