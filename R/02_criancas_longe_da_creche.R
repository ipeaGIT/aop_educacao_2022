# load support functions -------------------------------------------------

source("R/fun/setup.R")

library(scales)

# load data ---------------------------------------------------------------

# access_tp_car <- read_rds("../../data/acesso_oport/output_base_final/2019/dados2019_AcessOport_access_tpcar_v1.0.rds")
# access_active <- read_rds("../../data/acesso_oport/output_base_final/2019/dados2019_AcessOport_access_active_v1.0.rds")
# landuse <- read_rds("../../data/acesso_oport/output_base_final/2019/dados2019_AcessOport_landuse_v1.0.rds")
# 
# access_tp_car_df <- st_set_geometry(access_tp_car, NULL)
# access_active_df <- st_set_geometry(access_active, NULL)
# landuse_df <- st_set_geometry(landuse, NULL)


access_active_df <- aopdata::read_access(city="all", year = 2019)

# figura - % de crianças de 0 a 5 anos e de baixa renda a mais de 15/30 min de caminhada da escola mais próxima -----------------------------------------------

## preparação dos dados ----

geral_df <- access_active_df %>%
  filter(P010 > 0, R003 > 0, R003 <= 5, mode == "walk") %>%
  select(id_hex, abbrev_muni, name_muni, 
         populacao = P001, 
         pop_criancas = P010, 
         renda_decil = R003, 
         TMIEI) %>%
  mutate(tempo_viagem = case_when(TMIEI <= 15 ~ "15 min",
                                  TMIEI <= 30 ~ "30 min",
                                  TRUE ~ "-")) %>%
  mutate(renda_decil = factor(renda_decil),
         tempo_viagem = factor(tempo_viagem, levels = c("-", "30 min", "15 min"))) %>%
  drop_na()

glimpse(geral_df)

summary_df <- geral_df %>%
  group_by(abbrev_muni, name_muni) %>%
  summarise(children_above_15m = sum(ifelse(TMIEI > 15, pop_criancas, 0)),
            children_above_30m = sum(ifelse(TMIEI > 30, pop_criancas, 0)),
            children = sum(pop_criancas)) %>%
  mutate(p_children_above_15m = children_above_15m / children,
         p_children_above_30m = children_above_30m / children)

summary_df <- geral_df %>%
  count(abbrev_muni, name_muni, tempo_viagem, wt = pop_criancas) %>%
  group_by(abbrev_muni, name_muni) %>%
  mutate(p = n / sum(n)) %>%
  filter(tempo_viagem != "-") %>%
  arrange(name_muni, desc(tempo_viagem)) %>%
  mutate(cum_p = p + lag(p, default = 0))

glimpse(summary_df)

summary_order <- summary_df %>%
  arrange(desc(p_children_above_15m))

## plot ----

summary_df %>%
  mutate(name_muni = factor(name_muni, levels = summary_order$name_muni)) %>%
  ggplot(aes(y=name_muni)) +
  ggalt::geom_dumbbell(aes(x = p_children_above_15m, xend = p_children_above_30m), 
                       size=3, color="gray80", alpha=.8, colour_x = "steelblue4", colour_xend = "springgreen4")+
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels=paste0(seq(0, 100, 10), "%")) +
  geom_text(data = filter(summary_df, abbrev_muni == "slz"),
            aes(x = p_children_above_30m, y = name_muni),
            label = "30 minutos", fontface = "bold",
            color = "springgreen4",
            vjust = -1) +
  geom_text(data = filter(summary_df, abbrev_muni == "slz"),
            aes(x = p_children_above_15m, y = name_muni),
            label = "15 minutos", fontface = "bold",
            color = "steelblue4",
            vjust = -1) +
  labs(title = "% de crianças de 0 a 5 anos de idade a mais de 15 e 30 min de\ncaminhada de uma escola de educação infantil",
       caption = "(50% das famílias mais pobres em cada cidade)",
       y=NULL,
       x=NULL, # "% de crianças atendidas",
       color = NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA, color = "grey40"),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom")


## save plot

ggsave(plot = last_plot(), filename = here::here("figures", "criancas_longe_da_creche.png"),
       width = 16, height = 13, units = "cm", dpi = 300, scale=1.3)

