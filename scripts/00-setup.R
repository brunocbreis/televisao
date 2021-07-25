# carregar pacotes --------------------------------------------------------

library(tidyverse) # pra vida
library(lubridate) # pra lidar com datas
library(extrafont) # pra usar fontes
library(ggthemes) # pra gráficos bonitos
library(prismatic) # pra ver as cores

# funções úteis -----------------------------------------------------------

hex <- function(palette) {
  paste0('#',
         palette)
}

# importar bases ----------------------------------------------------------


nick_raw <- read_delim(
  "dados/nick_maio_2018.csv",
  "\\",
  escape_double = FALSE,
  col_names = FALSE,
  col_types = cols(
    X2 = col_date(format = "%Y%m%d"),
    X3 = col_time(format = "%H%M%S"),
    X4 = col_time(format = "%H%M%S"),
    X7 = col_character()
  ),
  trim_ws = TRUE
) 

disney_raw <- read_delim(
  'dados/disney_jan_2018.csv',
  '\\',
  escape_double = FALSE,
  col_names = FALSE,
  col_types = cols(
    X1 = col_date(format = "%Y%m%d"),
    X2 = col_time(format = "%H%M%S"),
    X3 = col_time(format = "%H%M%S"),
    X9 = col_integer()
  ),
  trim_ws = TRUE
) 

# limpando a base da nick -------------------------------------------------

nick <- nick_raw %>% 
  select(2:12) %>% 
  rename(data = X2,
         hora_entrada = X3,
         hora_saida = X4,
         pgm = X5,
         criadores = X6,
         crt = X7,
         pgm_2 = X8,
         episodio = X9,
         ano = X10,
         sinopse = X11,
         pais = X12) %>% 
  select(-pgm_2) %>% 
  mutate(pais = case_when(pais == "01" ~ "Brasil",
                          pais == "04" ~ "EUA"))

# limpando a base da disney -----------------------------------------------

disney <- disney_raw %>% 
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
  mutate(hora_entrada = ymd_hms(paste(data,hora_entrada)),
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


         







