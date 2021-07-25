library(tidyverse)
library(lubridate)
library(prismatic)
library(extrafont)
library(ggthemes)

disney <- read_rds("dados/disney_2019_clean.rds") %>% 
  mutate(semana = floor_date(data, "week"))

semanas <- disney %>% 
  distinct(semana) %>% 
  arrange(semana) %>% 
  pull()



series <- disney %>% 
  filter(categoria == "serie") %>% 
  count(pgm) %>% 
  arrange(desc(n)) %>% 
  pull(pgm) 

top <- function(vector,x=10) {
vector[1:x]
}

n_series <- 4

disney %>% 
  filter(categoria == "serie")  %>% 
  group_by(semana, pgm) %>% 
  summarise(insercoes = n()) %>% 
  filter(pgm %in% top(series,n_series)) %>% 
  ggplot(aes(x = semana + days(1),
             y = insercoes,
             color = pgm)) +
  geom_line(size = .35,
            alpha = .5,
            show.legend = FALSE) +
  geom_point(size = 1) +
  scale_x_date(date_breaks = "4 weeks",
               date_minor_breaks = "weeks",
               date_labels = "%V") +
  scale_y_continuous(breaks = seq(0,40,5)) +
  coord_cartesian(xlim = c(semanas[2] - days(2), semanas[length(semanas)] - days(6)),
                  ylim = c(0, NA)) + 
  labs(title = "As séries preferidas do Disney Channel",
       subtitle = paste("Episódios exibidos por semana das top", n_series, "séries em 2019"),
       y = 'Episódios no ar',
       x = 'Semana') +
  theme_bw(base_family = "Commissioner") +
  theme(plot.title = element_text(face = "bold"),
        axis.title = element_text(size = 8, color = 128),
        legend.title = element_blank(),
        legend.text = element_text(size =8),
        line = element_line(size = .5),
        plot.margin = margin(rep(20,4)),
        legend.position = c(.15,.15),
        legend.background = element_rect(fill = alpha("white", alpha = .25))
        )

ggsave("graficos/series_preferidas_2019.png",width = 8,height = 4.5)  


view(disney)

disney %>% 
  count(ano_producao) %>% 
  mutate(is_2019 = ano_producao == 2019) %>%
  drop_na() %>% 
  ggplot() +
  geom_point(
    aes(x = ano_producao,
             y = n,
             color = is_2019))
