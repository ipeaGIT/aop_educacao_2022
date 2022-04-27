# load support functions -------------------------------------------------

source("R/fun/setup.R")

library(scales)

# load data ---------------------------------------------------------------

access_tp_car <- read_rds("../../data/acesso_oport/output_base_final/2019/dados2019_AcessOport_access_tpcar_v1.0.rds")
access_active <- read_rds("../../data/acesso_oport/output_base_final/2019/dados2019_AcessOport_access_active_v1.0.rds")
landuse <- read_rds("../../data/acesso_oport/output_base_final/2019/dados2019_AcessOport_landuse_v1.0.rds")

access_tp_car_df <- st_set_geometry(access_tp_car, NULL)
access_active_df <- st_set_geometry(access_active, NULL)
landuse_df <- st_set_geometry(landuse, NULL)


full_df <- rbind(
  left_join(landuse_df, access_active_df, by = c("id_hex", "sigla_muni")), 
  left_join(landuse_df, access_tp_car_df, by = c("id_hex", "sigla_muni")),
  fill = TRUE
)


# figura - % de crianças de 0 a 5 anos e de baixa renda a mais de 15/30 min de caminhada da escola mais próxima -----------------------------------------------

## preparação dos dados ----

geral_df <- full_df %>%
  filter(P010 > 0, R003 > 0, modo == "caminhada") %>%
  select(id_hex, sigla_muni, nome_muni, 
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
  count(sigla_muni, nome_muni, renda_decil, tempo_viagem, wt = pop_criancas) %>%
  group_by(sigla_muni, nome_muni, renda_decil) %>%
  mutate(p = n / sum(n)) %>%
  filter(tempo_viagem != "-") %>%
  arrange(nome_muni, renda_decil, desc(tempo_viagem)) %>%
  mutate(cum_p = p + lag(p, default = 0))

glimpse(summary_df)

summary_order <- summary_df %>%
  filter(renda_decil %in% c(1, 2, 3, 4), tempo_viagem == "15 min") %>%
  group_by(nome_muni) %>%
  summarise(p = mean(p)) %>%
  arrange(desc(p))

## plot ----

summary_df %>%
  filter(renda_decil %in% c(1, 2, 3, 4), tempo_viagem != "-") %>%
  # filter(tempo_viagem != "-") %>%
  mutate(nome_muni = factor(nome_muni, levels = summary_order$nome_muni)) %>%
  ggplot(aes(x=renda_decil, y=cum_p, color=tempo_viagem, group=tempo_viagem)) +
  geom_point() +
  geom_path() +
  geom_hline(yintercept = c(0.5, 0.9), color = "grey40") +
  # geom_hline(data=vagas_por_crianca, aes(yintercept = vagas_percent)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), labels=paste0(seq(0, 100, 20), "%")) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~nome_muni) +
  labs(title = "% de crianças de 0 a 5 anos de idade a menos de 15 e 30 min de\ncaminhada de uma escola de educação infantil",
       x = "Decil de renda (1 a 4: 40% das famílias mais pobres em cada cidade)",
       y = "% de crianças atendidas",
       color = NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA, color = "grey40"),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom")


## save plot

ggsave(plot = last_plot(), filename = here::here("figures", "criancas_perto_da_creche.png"),
       width = 16, height = 13, units = "cm", dpi = 300, scale=1.3)

