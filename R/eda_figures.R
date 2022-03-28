


# load support functions -------------------------------------------------

source("R/fun/setup.R")



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



# figura 1 - panorama geral -----------------------------------------------

geral_df <- full_df %>%
  filter(P001 > 0, R002 > 0, modo == "caminhada") %>%
  select(id_hex, sigla_muni, nome_muni, P001, R002, M001, M002, M003, M004, CMAMT30) %>%
  drop_na()

geral_df %>%
  sample_n(size = 1000000, weight = P001, replace = T) %>%
  mutate(R002 = factor(R002)) %>%
  ggplot(aes(x=R002, y = CMAMT30, color = R002, group=R002)) +
  geom_boxplot() +
  facet_wrap(~nome_muni) +
  labs(title = "Total de matrículas acessíveis em 30min de caminhada",
       subtitle = "por quintil de renda")
  

ggsave("figures/cma_mat_tp_30m_qt.png", width = 210, height = 210, dpi = 300, units = "mm")


# figura 2 - criancas atendidas por escola em 15 minutos ------------------

criancas_df <- full_df %>%
  filter(P006 > 0, R002 > 0, modo == "caminhada") %>%
  select(id_hex, sigla_muni, nome_muni, P001, P010, R002, M001, M002, M003, M004, TMIEI) %>%
  mutate(perto = TMIEI <= 15) %>%
  drop_na() 

criancas_atendidas_df <- criancas_df %>%
  group_by(sigla_muni, R002) %>%
  mutate(criancas_quintil = sum(P010)) %>%
  group_by(sigla_muni, nome_muni, perto, R002) %>%
  summarise(criancas = sum(P010), criancas_quintil = mean(criancas_quintil), .groups = "drop") %>%
  filter(perto == TRUE) %>%
  mutate(p = criancas / criancas_quintil)
  

criancas_atendidas_df %>%
  mutate(R002 = factor(R002)) %>%
  mutate(nome_muni = fct_reorder(nome_muni, p)) %>%
  filter(R002 %in% c(1, 5)) %>%
  ggplot(aes(x=nome_muni, y=p, color=R002)) +
  geom_path(aes(group=nome_muni)) +
  geom_point() +
  scale_y_continuous(limits=c(0, 1), labels = scales::percent) +
  coord_flip() +
  labs(title = "% de crianças de 0 a 9 anos a menos de 15 min de caminhada\nde uma escola de educação infantil")

ggsave("figures/tmi_inf_walk_15m_qt.png", width = 210, height = 210, dpi = 300, units = "mm")

