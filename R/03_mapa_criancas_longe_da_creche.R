# load support functions -------------------------------------------------

source("R/fun/setup.R")

library(scales)
library(patchwork)
library(ggspatial)

# load data ---------------------------------------------------------------

access_tp_car <- read_rds("../../data/acesso_oport/output_base_final/2019/dados2019_AcessOport_access_tpcar_v1.0.rds")
access_active <- read_rds("../../data/acesso_oport/output_base_final/2019/dados2019_AcessOport_access_active_v1.0.rds")
landuse <- read_rds("../../data/acesso_oport/output_base_final/2019/dados2019_AcessOport_landuse_v1.0.rds")

access_tp_car_df <- st_set_geometry(access_tp_car, NULL)
access_active_df <- st_set_geometry(access_active, NULL)
landuse_df <- st_set_geometry(landuse, NULL)


full_df <- rbind(
  left_join(landuse_df, access_active_df, by = c("id_hex", "sigla_muni", "nome_muni", "code_muni")), 
  left_join(landuse_df, access_tp_car_df, by = c("id_hex", "sigla_muni", "nome_muni", "code_muni")),
  fill = TRUE
)

# figura - localização das crianças de 0 a 5 anos e de baixa renda a mais de 15/30 min de caminhada da escola mais próxima -----------------------------------------------

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





# localização das crianças longe da escola

longe_df <- geral_df %>%
  filter(tempo_viagem != "15 min", renda_decil %in% c(1, 2, 3, 4)) 

longe_df$geometry <- h3jsr::h3_to_polygon(longe_df$id_hex)
longe_sf <- st_as_sf(longe_df, crs=4326)


mapa_criancas_sem_escola <- function(muni, longe = "30 min", size = 16, title) {

  # centroide da cidade
  centroid_sf <- landuse %>%
    filter(sigla_muni == muni, P001 > 0) %>%
    group_by(nome_muni) %>%
    summarise() %>%
    st_centroid()
  
  units(size) <- "km"
  b_box <- centroid_sf %>%
    st_buffer(dist = size) %>%
    st_bbox()
  
  # borda dos hexagonos a 30 min de caminhada
  # longe_30min <- filter(longe_sf, sigla_muni == muni, tempo_viagem == "30 min") %>%
  #   summarise()
  
  longe_sf %>%
    filter(sigla_muni == muni, pop_criancas > 0, tempo_viagem %in% longe) %>%
    ggplot() +
    geom_sf(data=filter(landuse, sigla_muni == muni, P001 > 0, R003 > 4), fill="grey90", color="grey85", size = 0.1) +
    geom_sf(data=filter(landuse, sigla_muni == muni, P001 > 0, R003 <= 4), fill="grey80", color="grey70", size = 0.1) +
    geom_sf(data=filter(landuse, sigla_muni == muni, E002 > 0), fill=muted("blue"), color="grey70", size = 0.05) +
    geom_sf(aes(fill=pop_criancas), color=NA) +
    # geom_sf(data=longe_30min, fill=NA, color="grey40", size = 0.3) +
    # geom_sf(data=filter(longe_sf, sigla_muni == muni, tempo_viagem == "-"), 
    #         fill=NA, color=muted("blue"), size = 0.1) +
    coord_sf(datum = NA,
             xlim = c(b_box["xmin"], b_box["xmax"]),
             ylim = c(b_box["ymin"], b_box["ymax"])) +
    scale_fill_distiller(palette = "Reds", direction = 1, limits = c(0, 250)) +
    labs(fill = "Número\nabsoluto") +
    theme_void() +
    theme(legend.position = "right",
          panel.border = element_rect(fill = NA, color = "grey20")) +
    annotate(geom = "text", x = b_box["xmin"], y = b_box["ymax"], hjust = 0,
             label = centroid_sf$nome_muni[[1]]) +
    annotation_scale(location = "br", width_hint = 0.5, style = "ticks")
  
}

map_and_save <- function(muni) {
  mapa <- mapa_criancas_sem_escola(muni)
  
  ggsave(plot = mapa, filename = paste(muni, ".png"), width = 21, height = 21, 
         units = "cm", dpi = 300)
}

m_poa_15 <- mapa_criancas_sem_escola("poa", longe = c("30 min", "-"))
m_poa_30 <- mapa_criancas_sem_escola("poa", longe = "-")

m_poa_15 + m_poa_30 + plot_layout(guides = "collect")

m_cur <- mapa_criancas_sem_escola("cur")
m_rec <- mapa_criancas_sem_escola("rec")
m_poa <- mapa_criancas_sem_escola("poa")
m_sgo <- mapa_criancas_sem_escola("sgo")
m_slz <- mapa_criancas_sem_escola("slz")
m_cam <- mapa_criancas_sem_escola("cam")
m_for <- mapa_criancas_sem_escola("for")

plot_mapas <- m_rec + m_poa + m_slz + m_sgo + 
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(title = "Crianças de 0 a 5 anos de idade a mais de 15 ou 30 minutos\nde caminhada da creche mais próxima",
                  caption = "hexágonos contornados em cinza: tempo de viagem entre 15 min e 30 minutos")

plot_mapas

ggsave(plot = plot_mapas, filename = here::here("figures", "mapa_criancas_longe_da_creche_new.png"),
       width = 16, height = 14, units = "cm", dpi = 300, scale=1.4)

map_and_save("poa")


munis <- unique(landuse_df$sigla_muni)
mapas <- map(munis, mapa_criancas_sem_escola)

walk(munis, map_and_save)

wrap_plots(mapas, guides = "collect")

# densidade de crianças fora da escola

densidade_df <- longe_df %>% 
  group_by(sigla_muni, nome_muni) %>%
  summarise(densidade = sum(pop_criancas) / n(), .groups = "drop")

densidade_df %>%
  mutate(nome_muni = fct_reorder(nome_muni, densidade)) %>%
  ggplot(aes(x=nome_muni, y=densidade)) +
  geom_col() +
  coord_flip()

escolas <- filter(landuse, sigla_muni == muni, E002 > 0)

longe_sf %>%
  filter(sigla_muni == muni) %>%
  ggplot() +
  geom_sf(data=filter(landuse, sigla_muni == muni, P001 > 0), fill="grey80", color="grey70", size = 0.1) +
  geom_sf(data=filter(landuse, sigla_muni == muni, E002 > 0), fill=muted("blue"), color="grey70", size = 0.05) +
  geom_sf(aes(fill=pop_criancas), color=NA) +
  coord_sf(datum = NA) +
  scale_fill_distiller(palette = "Reds", direction = 1, limits=c(0, 250))

longe_sf %>%
  filter(sigla_muni == muni) %>%
  mapview(zcol="pop_criancas") + escolas

# % da area nao atendida

populated_df <- filter(landuse_df, P001 > 0, R003 %in% c(1, 2, 3, 4)) %>%
  count(nome_muni) %>%
  left_join(count(longe_df, nome_muni, name = "longe")) %>%
  mutate(p = longe / n)

# vagas por criança

# vagas_por_crianca <-
landuse_df %>%
  filter(R003 >=1, R003 <= 10) %>%
  mutate(R003=factor(R003)) %>%
  group_by(sigla_muni, nome_muni, R003) %>%
  summarise(criancas_0a5 = sum(P010), vagas_infantil = sum(M002), .groups = "drop") %>%
  group_by(sigla_muni, nome_muni) %>%
  mutate(vagas_cidade = sum(vagas_infantil)) %>%
  mutate(criancas_acc = cumsum(criancas_0a5),
         vagas_decc = cumsum(vagas_cidade - criancas_acc)) %>% View()
mutate(vagas_decil = vagas_cidade / criancas_acc) %>%
  View()

summarise()
mutate(vagas_percent = vagas_infantil / criancas_0a5)

vagas_ensino_infantil <- landuse_df %>%
  count(sigla_muni, nome_muni, wt = M002, name = "vagas_infantil") # matrículas ensino infantil

criancas <- landuse_df %>%
  count(sigla_muni, nome_muni, R003,  wt = P010, name = "criancas_0a5") %>%
  filter(R003 >=1, R003 <= 10) %>%
  mutate(renda_decil = factor(R003, levels = 10:1))

criancas_vagas <- left_join(criancas, vagas_ensino_infantil, by = c("sigla_muni", "nome_muni")) %>%
  group_by(sigla_muni, nome_muni) %>%
  mutate(criancas_0a5_p = criancas_0a5 / sum(criancas_0a5),
         vagas_infantil_p = vagas_infantil / sum(criancas_0a5)) %>%
  ungroup() %>%
  mutate(nome_muni = fct_reorder(nome_muni, vagas_infantil_p))

criancas_vagas %>%
  ggplot(aes(x=nome_muni, y=criancas_0a5_p)) +
  geom_col(aes(fill=renda_decil)) +
  geom_point(aes(y=vagas_infantil_p), shape="l") +
  geom_linerange(aes(ymin=0, ymax=vagas_infantil_p), size=0.2) +
  coord_flip() +
  scale_fill_viridis_d(option="E") +
  scale_y_percent(breaks = seq(0, 1, 0.1)) +
  labs(x = NULL, y = "% de crianças (por decil de renda)",
       fill = "Decil de renda",
       subtitle = "% de vagas em creches em relação à população infantil de 0 a 5 anos de idade") +
  theme(legend.position = "bottom")

access_active %>%
  filter(sigla_muni == "poa") %>%
  ggplot(aes(fill=CMAMI15)) +
  geom_sf(color=NA) +
  scale_fill_distiller(palette = "Spectral")


landuse %>%
  filter(sigla_muni == "poa") %>%
  ggplot() +
  geom_sf(aes(fill=factor(R003)), color=NA) +
  scale_fill_brewer(palette = "RdBu")

landuse %>%
  filter(sigla_muni == "poa", R003 >= 6) %>%
  mapview(zcol="R003")
  