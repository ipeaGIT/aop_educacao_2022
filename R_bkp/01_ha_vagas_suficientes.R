# load support functions -------------------------------------------------

source("R/fun/setup.R")

library(scales)

# load data ---------------------------------------------------------------

landuse_df <- aopdata::read_landuse(city="all", year = 2019)

# prepare dataset ---------------------------------------------------------

## select number of students in each age group and school places by education level

vagas_por_estudante_df <- landuse_df %>% 
  dplyr::select(id_hex, abbrev_muni, name_muni, 
         pop_total = P001, 
         pop_0a5   = P010, 
         pop_6a14  = P011, 
         pop_15a18 = P012,
         mat_total = M001,
         mat_infantil    = M002,
         mat_fundamental = M003,
         mat_medio       = M004) %>%
  group_by(abbrev_muni, name_muni) %>%
  summarise(across(.cols = where(is.numeric), .fns = sum, na.rm = TRUE), .groups = "drop") %>%
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
  dplyr::select(id_hex, abbrev_muni, name_muni, 
         renda_decil = R003,
         pop_total = P001, 
         pop_0a5   = P010, 
         pop_6a14  = P011, 
         pop_15a18 = P012)  %>%
  filter(renda_decil >=1, renda_decil <= 10) %>%
  group_by(abbrev_muni, name_muni, renda_decil) %>%
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
  dplyr::select(abbrev_muni, name_muni, nivel_ensino, proporcao_atendidos) %>%
  filter(nivel_ensino == "fundamental") %>%
  arrange(proporcao_atendidos)

estudantes_tidy %>% 
  mutate(name_muni = factor(name_muni, levels = cidades_factor$name_muni)) %>%
  ggplot(aes(x=name_muni, y=proporcao_decil)) +
  geom_col(aes(fill=renda_decil)) +
  geom_errorbar(data = vagas_tidy, aes(ymin=0, ymax=proporcao_atendidos, y=NULL), width = 0.5) +
  coord_flip() +
  scale_fill_viridis_d(option="E", direction = 1) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), labels = seq(0, 100, 20)) +
  labs(x = NULL, y = "% de crianças em cada decil de renda",
       fill = "Decil de renda",
       subtitle = "% de vagas em escolas em relação à população em idade escolar") +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.4, "cm")) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE)) +
  facet_wrap(~nivel_ensino, nrow = 1)


# save plot ---------------------------------------------------------------

ggsave(plot = last_plot(), filename = here::here("figures", "vagas_por_estudante.png"),
       width = 16, height = 12, units = "cm", dpi = 300)

