# setup -------------------------------------------------------------------

library(tidyverse)
library(rvest)
library(lubridate)


# scraping para pegar os links --------------------------------------------

# url_globo <- 'http://especial1.g1.globo.com/globo-news/exibido.php'
# pagina <- read_html(url_globo)
#
# tabelas_para_baixar <- pagina %>%
#   html_nodes('.linkmes') %>%
#   html_attr('href') %>%
#   paste0('http://especial1.g1.globo.com/globo-news/',.)


# importar tabelas da web --------------------------------------------------------

# gnews_raw <- map_dfr(
#   tabelas_para_baixar,
#   function(x) {
#     read_delim(
#       file = x,
#       delim = '\\',
#       escape_double = T,
#       col_names = F,
#       col_types = cols(
#         X1 = col_skip(),
#         X3 = col_skip(),
#         X4 = col_date(format = "%Y%m%d"),
#         X5 = col_time(format = '%H%M%S'),
#         X6 = col_time(format = '%H%M%S'),
#         X8 = col_integer(),
#         X10 = col_factor(),
#         X11 = col_factor()
#       )
#     ) }
# )
#
# write_rds(gnews_raw, 'dados/gnews/gnews_raw.rds')


# importar localmente -----------------------------------------------------

gnews_raw <- read_rds('dados/gnews/gnews_raw.rds')

gnews_2019_2020 <- gnews_raw %>%
  rename(
    pgm = X2,
    data = X4,
    hora_entrada = X5,
    hora_saida = X6,
    diretor = X7,
    ano_producao = X8,
    sinopse = X9,
    codigo_ancine = X10,
    codigo_regiao = X11
  ) %>%
  filter(year(data) %in% 2019:2020) %>%
  mutate(
    duracao = difftime(hora_saida, hora_entrada) %>% seconds(),
    duracao = if_else(duracao < 0, duracao + seconds(24 * 60 * 60), duracao),
    duracao_segundos = duracao %>% as.integer(),
    diretor = str_to_title(diretor),
    categoria = if_else(str_detect(pgm, 'VIN'), 'vinheta', 'pgm'),
    vht = if_else(categoria == 'vinheta', pgm, NA_character_) %>% str_remove('VIN ') %>% as_factor(),
    pgm = if_else(categoria == 'pgm', pgm, NA_character_),
    
    pgm = pgm %>%
      # limpeza intensa de strings
      str_to_title() %>%
      str_trim() %>%
      str_replace('  ', ' ') %>%
      str_replace('Edicao', 'Edição') %>%
      str_replace('Gnews', 'Globonews') %>%
      str_replace('Globo News', 'Globonews') %>%
      str_replace('Gn', 'Globonews') %>%
      str_replace('Jornal Globonews Edi', 'Jornal Globonews - Edi') %>%
      str_replace('Edição Meio-Dia', 'Edição Do Meio-Dia') %>%
      str_replace('Edição Meia-Noite', 'Edição Da Meia-Noite') %>%
      str_replace('Ii', 'II') %>%
      str_replace('Estudio', 'Estúdio') %>%
      str_replace('Plantao', 'Plantão') %>%
      str_replace('Globonewsewas', 'Globonews') %>%
      str_replace('Em Foco An', 'Em Foco Com An') %>%
      str_replace('Andreia', 'Andréia') %>%
      str_replace('Miriam Leitao', 'Miriam Leitão') %>%
      str_replace('Fantastico', 'Fantástico') %>%
      str_replace('Documentario', 'Documentário') %>%
      str_replace('Reporter', 'Repórter') %>%
      str_replace('Coronavirus', 'Coronavírus') %>%
      str_replace('Politica', 'Política') %>%
      str_replace('Eleicoes', 'Eleições'),
    
    # separar pgm de episódios / edições
    edicao_pgm = case_when(
      str_detect(pgm, 'Jornal Globonews') ~ str_match(pgm, '(?<=Jornal Globonews - ).*'),
      str_detect(pgm, 'Em Casa Nelson') ~ str_match(pgm, '(?<=Em Casa Nelson Motta ).*'),
      str_detect(pgm, 'Cidades E Solucoes') ~ str_match(pgm, '(?<=Cidades E Solucoes).*'),
      str_detect(pgm, 'Que Mundo ') ~ str_match(pgm, '(?<=Que Mundo E Esse - ).*'),
      str_detect(pgm, 'Globonews Documentário') ~ str_match(pgm, '(?<=Globonews Documentário - ).*'),
      str_detect(pgm, 'Globonews Espec') ~ str_match(pgm, '(?<=Globonews Especial) .*') %>% str_trim(),
      str_detect(pgm, 'Plantão') ~ str_match(pgm, '(?<=Plantão ).*') %>% str_trim(),
      str_detect(pgm, 'Retrospectiva') ~ str_match(pgm, '(?<=Retrospectiva ).*'),
      TRUE ~ NA_character_
    ),
    
    # completar a edição faltante
    edicao_pgm = case_when(
      edicao_pgm == 'Edição Das' & year(data) == 2019 ~ 'Edição Das 07h',
      edicao_pgm == 'Edição Das' &
        year(data) == 2020 ~ 'Edição Das 14h',
      TRUE ~ edicao_pgm
    ),
    
    # limpando factors das edições do jornal globonews
    edicao_pgm =
      as_factor(edicao_pgm) %>%
      fct_recode(
        # Jornal Globonews...
        'Edição Das 06h' = 'Edição Das 06',
        'Edição Da 01h' = 'Edição 01h',
        'Edição Da 01h' = 'Edição De 01h',
        'Edição Das 05h' = 'Edição 5h',
        'Edição Das 04h' = 'Edição 04h',
        'Edição Das 02h' = 'Edição 02h'
      ),
    
    # limpando o nome dos programas episódicos
    pgm = case_when(
      str_detect(pgm, 'Jornal Globonews') ~ 'Jornal Globonews',
      str_detect(pgm, 'Em Casa Nelson') |
        str_detect(pgm, 'Nelson Motta') ~ 'Em Casa Com Nelson Motta',
      # str_detect(pgm, 'Nelson Motta') ~ 'Em Casa Com Nelson Motta',
      str_detect(pgm, 'Cidades E Solu') ~ 'Cidades E Soluções',
      str_detect(pgm, 'Melhor Do Brasil') ~ 'O Melhor do Brasil é o Brasileiro',
      str_detect(pgm, 'Fatos E Vers') ~ 'Fatos E Versões',
      str_detect(pgm, 'Que Mundo *') ~ 'Que Mundo É Esse?',
      str_detect(pgm, 'Globonews Documentário') ~ 'Globonews Documentário',
      str_detect(pgm, 'Globonews Documento') ~ 'Globonews Documento',
      str_detect(pgm, 'Globonews Especial') ~ 'Globonews Especial',
      str_detect(pgm, 'Plantão') ~ 'Plantão',
      pgm == 'Amozonidas' | pgm == 'Amazonidas' ~ 'Amazônidas',
      pgm == 'Milenio' ~ 'Milênio',
      str_detect(pgm, 'Roberto D') ~ 'Roberto D’Ávila',
      str_detect(pgm, 'Gabeira') ~ 'Fernando Gabeira',
      str_detect(pgm, 'Di.logos') ~ 'Diálogos Com Mario Sergio Conti',
      str_detect(pgm, 'Carnaval') ~ 'Carnaval Blocos',
      str_detect(pgm, 'Boletim') ~ 'Boletim de Notícias',
      str_detect(pgm, 'Grandes Neg') ~ 'Pequenas Empresas & Grandes Negócios',
      str_detect(pgm, 'Arquivo N') ~ 'Arquivo N',
      pgm == 'Movimento' | pgm == 'Em Movimento' ~ 'Globonews Em Movimento',
      str_detect(pgm, 'Retrospectiva') ~ 'Retrospectiva',
      str_detect(pgm, 'Grandes Nomes') ~ 'Grandes Nomes',
      TRUE ~ pgm
    ),
    
    
    
    categoria = if_else(str_detect(pgm, 'Chamada'),
                        'chamada',
                        categoria),
    
    chamada = if_else(categoria == 'chamada',
                      pgm,
                      NA_character_),
    
    pgm = if_else(categoria == 'chamada',
                  NA_character_,
                  pgm),
    
    pgm = as_factor(pgm)
    
  ) %>%
  select(
    pgm,
    edicao_pgm,
    vht,
    chamada,
    data:hora_saida,
    duracao,
    duracao_segundos,
    everything()
  )

write_rds(gnews_2019_2020, 'dados/gnews/gnews_2019_2020_clean.rds')

# glimpse(gnews_2019_2020)


# gnews_2019_2020$pgm %>% unique() %>% levels() %>% sort() %>%
#   tibble(pgm = .,
#          transmissao = NA) %>%
#   write_excel_csv('dados/gnews/pgms_ao_vivo_pedro.csv')
# 
# gnews_2019_2020$edicao_pgm %>% levels() %>% sort()
