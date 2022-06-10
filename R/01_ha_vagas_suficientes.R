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


# prepare dataset ---------------------------------------------------------

## select number of students in each age group and school places by education level

vagas_por_estudante_df <- landuse_df %>% 
  select(id_hex, sigla_muni, nome_muni, 
         pop_total = P001, 
         pop_0a5   = P010, 
         pop_6a14  = P011, 
         pop_15a18 = P012,
         mat_total = M001,
         mat_infantil    = M002,
         mat_fundamental = M003,
         mat_medio       = M004) %>%
  group_by(sigla_muni, nome_muni) %>%
  summarise(across(.cols = where(is.numeric), .fns = sum), .groups = "drop") %>%
  mutate(ratio_infantil = mat_infantil / pop_0a5,
         ratio_fundamental = mat_fundamental / pop_6a14,
         ratio_medio = mat_medio / pop_15a18)

vagas_por_estudante_df

vagas_tidy <- vagas_por_estudante_df %>%
  pivot_longer(cols = starts_with("ratio"), names_to = "nivel", values_to = "proporcao_atendidos") %>%
  mutate(nivel_ensino = factor(nivel, 
                               levels = c("ratio_infantil", "ratio_fundamental", "ratio_medio"),
                               labels = c("infantil", "fundamental", "médio")))

## proportion of students per income decile

estudantes_por_decil_df <- landuse_df %>% 
  select(id_hex, sigla_muni, nome_muni, 
         renda_decil = R003,
         pop_total = P001, 
         pop_0a5   = P010, 
         pop_6a14  = P011, 
         pop_15a18 = P012)  %>%
  filter(renda_decil >=1, renda_decil <= 10) %>%
  group_by(sigla_muni, nome_muni, renda_decil) %>%
  summarise(across(.cols = starts_with("pop"), .fns = sum), .groups = "drop_last") %>%
  mutate(pop_0a5_p = pop_0a5 / sum(pop_0a5),
         pop_6a14_p = pop_6a14 / sum(pop_6a14),
         pop_15a18_p = pop_15a18 / sum(pop_15a18),
         renda_decil = factor(renda_decil, levels = 10:1))
  
estudantes_tidy <- estudantes_por_decil_df %>%
  pivot_longer(cols = pop_0a5_p:pop_15a18_p, names_to = "idade", values_to = "proporcao_decil") %>%
  mutate(nivel_ensino = factor(idade, 
                               levels = c("pop_0a5_p", "pop_6a14_p", "pop_15a18_p"),
                               labels = c("infantil", "fundamental", "médio")))



# plot --------------------------------------------------------------------

cidades_factor <- vagas_tidy %>%
  select(sigla_muni, nome_muni, nivel_ensino, proporcao_atendidos) %>%
  filter(nivel_ensino == "fundamental") %>%
  arrange(proporcao_atendidos)

estudantes_tidy %>% 
  mutate(nome_muni = factor(nome_muni, levels = cidades_factor$nome_muni)) %>%
  ggplot(aes(x=nome_muni, y=proporcao_decil)) +
  geom_col(aes(fill=renda_decil)) +
  # geom_point(data = vagas_tidy, aes(y=proporcao_atendidos)) +
  geom_errorbar(data = vagas_tidy, aes(ymin=0, ymax=proporcao_atendidos, y=NULL), width = 0.5) +
  # geom_linerange(aes(ymin=0, ymax=vagas_infantil_p), size=0.2) +
  coord_flip() +
  scale_fill_viridis_d(option="E") +
  scale_y_percent(breaks = seq(0.2, 1, 0.2)) +
  labs(x = NULL, y = "% de estudantes em cada decil de renda",
       fill = "Decil de renda",
       subtitle = "% de vagas em escolas em relação à população em idade escolar") +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.4, "cm")) +
  guides(fill = guide_legend(nrow = 1)) +
  facet_wrap(~nivel_ensino, nrow = 1)


# save plot ---------------------------------------------------------------

ggsave(plot = last_plot(), filename = here::here("figures", "vagas_por_estudante_new.png"),
       width = 16, height = 12, units = "cm", dpi = 300)

