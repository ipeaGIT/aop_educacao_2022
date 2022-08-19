# hexgrid <- tar_read(hexgrid)
# insuficiencia_ens_infantil_por_hex <- tar_read(insuficiencia_ens_infantil_por_hex)
# limites_municipais <- tar_read(limites_municipais)
mapear_insuficiencia_ens_infantil <- function(hexgrid, limites_municipais, insuficiencia_ens_infantil_por_hex) {
  
  # número máximo de crianças em situação de insuficiência de acessibilidade nas 3 
  # cidades, usado para uniformizar as legendas
  max_pop <- insuficiencia_ens_infantil_por_hex |> 
    filter(abbrev_muni %in% c("rec", "poa", "slz")) |> 
    summarise(max_pop = max(pop_li_15)) |> 
    pull(max_pop)
  
  m_rec_15 <- mapear_cidade(cidade = "rec", hexgrid = hexgrid, limite = limites_municipais,
                            insuf_hex = insuficiencia_ens_infantil_por_hex, 
                            li = "pop_li_15", max_pop = max_pop,
                            titulo = "Linha de Insuficiência: 15 minutos")
  
  m_rec_30 <- mapear_cidade(cidade = "rec", hexgrid = hexgrid, limite = limites_municipais,
                            insuf_hex = insuficiencia_ens_infantil_por_hex, 
                            li = "pop_li_30", max_pop = max_pop,
                            titulo = "Linha de Insuficiência: 30 minutos")
  
  m_poa_15 <- mapear_cidade(cidade = "poa", hexgrid = hexgrid, limite = limites_municipais,
                            insuf_hex = insuficiencia_ens_infantil_por_hex, 
                            li = "pop_li_15", max_pop = max_pop, titulo = NULL)
  
  m_poa_30 <- mapear_cidade(cidade = "poa", hexgrid = hexgrid, limite = limites_municipais,
                            insuf_hex = insuficiencia_ens_infantil_por_hex, 
                            li = "pop_li_30", max_pop = max_pop, titulo = NULL)
  
  m_slz_15 <- mapear_cidade(cidade = "slz", hexgrid = hexgrid, limite = limites_municipais,
                            insuf_hex = insuficiencia_ens_infantil_por_hex, 
                            li = "pop_li_15", max_pop = max_pop, titulo = NULL)
  
  m_slz_30 <- mapear_cidade(cidade = "slz", hexgrid = hexgrid, limite = limites_municipais,
                            insuf_hex = insuficiencia_ens_infantil_por_hex, 
                            li = "pop_li_30", max_pop = max_pop, titulo = NULL)
  
  p <- m_rec_15 + m_rec_30 + m_poa_15 + m_poa_30 + m_slz_15 + m_slz_30 +
    plot_annotation(title = "Crianças de 0 a 5 anos de idade a mais de 15 ou 30 minutos\nde caminhada da creche mais próxima") +
    plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "bottom")
  
  # save plot ---------------------------------------------------------------
  figura <- here::here("figuras", "fig_03_mapa_insuficiencia_ens_infantil.png")
  
  ggsave(plot = p, filename = figura, 
         width = 16, height = 17, units = "cm", dpi = 300, scale=1.4)

  return(figura)
}


# hexgrid <- tar_read(hexgrid)
# insuficiencia_ens_medio_por_hex <- tar_read(insuficiencia_ens_medio_por_hex)
# limites_municipais <- tar_read(limites_municipais)
mapear_insuficiencia_ens_medio <- function(hexgrid, limites_municipais, insuficiencia_ens_medio_por_hex) {
  
  # número máximo de jovens em situação de insuficiência de acessibilidade nas 3 
  # cidades, usado para uniformizar as legendas
  max_pop <- insuficiencia_ens_medio_por_hex |> 
    filter(abbrev_muni %in% c("for", "bho", "poa")) |> 
    summarise(max_pop = max(pop_li_3)) |> 
    pull(max_pop)
  
  m_for_0 <- mapear_cidade(cidade = "for", hexgrid = hexgrid, limite = limites_municipais,
                           insuf_hex = insuficiencia_ens_medio_por_hex, 
                           li = "pop_li_0", max_pop = max_pop,
                           titulo = "Linha de Insuficiência: 0 escolas")

  m_for_1 <- mapear_cidade(cidade = "for", hexgrid = hexgrid, limite = limites_municipais,
                           insuf_hex = insuficiencia_ens_medio_por_hex, 
                           li = "pop_li_1", max_pop = max_pop,
                           titulo = "Linha de Insuficiência: 1 escolas")

  m_for_3 <- mapear_cidade(cidade = "for", hexgrid = hexgrid, limite = limites_municipais,
                           insuf_hex = insuficiencia_ens_medio_por_hex, 
                           li = "pop_li_3", max_pop = max_pop,
                           titulo = "Linha de Insuficiência: 3 escolas")
  
  m_poa_0 <- mapear_cidade(cidade = "poa", hexgrid = hexgrid, limite = limites_municipais,
                           insuf_hex = insuficiencia_ens_medio_por_hex, 
                           li = "pop_li_0", max_pop = max_pop, titulo = NULL)
  
  m_poa_1 <- mapear_cidade(cidade = "poa", hexgrid = hexgrid, limite = limites_municipais,
                           insuf_hex = insuficiencia_ens_medio_por_hex, 
                           li = "pop_li_1", max_pop = max_pop, titulo = NULL)
  
  m_poa_3 <- mapear_cidade(cidade = "poa", hexgrid = hexgrid, limite = limites_municipais,
                           insuf_hex = insuficiencia_ens_medio_por_hex, 
                           li = "pop_li_3", max_pop = max_pop, titulo = NULL)
  
  m_bho_0 <- mapear_cidade(cidade = "bho", hexgrid = hexgrid, limite = limites_municipais,
                           insuf_hex = insuficiencia_ens_medio_por_hex, 
                           li = "pop_li_0", max_pop = max_pop, titulo = NULL)
  
  m_bho_1 <- mapear_cidade(cidade = "bho", hexgrid = hexgrid, limite = limites_municipais,
                           insuf_hex = insuficiencia_ens_medio_por_hex, 
                           li = "pop_li_1", max_pop = max_pop, titulo = NULL)
  
  m_bho_3 <- mapear_cidade(cidade = "bho", hexgrid = hexgrid, limite = limites_municipais,
                           insuf_hex = insuficiencia_ens_medio_por_hex, 
                           li = "pop_li_3", max_pop = max_pop, titulo = NULL)


  p <- m_for_0 + m_for_1 + m_for_3 + 
    m_bho_0 + m_bho_1 + m_bho_3 +
    m_poa_0 + m_poa_1 + m_poa_3 +
    plot_annotation(title = "Jovens de 15 a 18 anos de idade com acesso a até 0, 1 ou 3 escolas em 30 minutos de viagem por transporte público") +
    plot_layout(guides = "collect", ncol = 3) & theme(legend.position = "bottom")
  
  # save plot ---------------------------------------------------------------
  figura <- here::here("figuras", "fig_05_mapa_insuficiencia_ens_medio.png")
  
  ggsave(plot = p, filename = figura, 
         width = 16, height = 17, units = "cm", dpi = 300, scale=1.4)
  
  return(figura)
}


mapear_cidade <- function(cidade, hexgrid, limite,
                          insuf_hex, li = "pop_li_15", 
                          max_pop = 500, titulo = NULL) {
  # filtrar dados da cidade selecionada
  grid <- hexgrid |> filter(abbrev_muni == cidade)
  insuf_df <- insuf_hex |> filter(abbrev_muni == cidade)
  
  muni = insuf_df |> head(1) |> pull(code_muni)
  limite <- limite |> filter(code_muni == muni)
  
  # juntar grid espacial e dados de insuficiencia de acessibilidade
  pop_grid <- inner_join(grid, insuf_df, by = c("id_hex", "abbrev_muni", "name_muni", "code_muni")) |> 
    mutate(pop = get(li))
  
  # calcular centroide da cidade, para centralizar o mapa
  points <- h3_to_point(pop_grid$id_hex, simple = F)

  centroid_sf <- points |> 
    summarise(.groups = "drop") |> 
    st_centroid()
  
  centroid_sf$name_muni <- pop_grid$name_muni[[1]]
  
  # calcular área ao redor do centroide, para que todos os mapas tenham a mesma escala
  size = 16
  units(size) <- "km"
  b_box <- centroid_sf %>%
    st_buffer(dist = size) %>%
    st_bbox()
  
  # plotar o mapa
  p <- pop_grid |> 
    filter(pop > 0) |> 
    ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 12) +
    # geom_sf(data=filter(pop_grid, pop == 0), fill="grey95", color="grey70", size = 0.1) +
    geom_sf(aes(fill=pop), color=NA) +
    geom_sf(data = limite, fill = NA, color = "grey60", size = 0.5) +
    geom_sf(data=filter(pop_grid, matriculas > 0), fill=muted("blue"), color="grey70", size = 0.05) +
    coord_sf(datum = NA,
             xlim = c(b_box["xmin"], b_box["xmax"]),
             ylim = c(b_box["ymin"], b_box["ymax"])) +
    scale_fill_distiller(palette = "Reds", direction = 1, limits = c(0, max_pop)) +
    labs(fill = "Número\nde crianças",
         subtitle = titulo) +
    theme_void() +
    theme(legend.position = "bottom",
          panel.border = element_rect(fill = NA, color = "grey20")) +
    annotate(geom = "text", x = b_box["xmin"], y = b_box["ymax"], hjust = 0,
             label = centroid_sf$name_muni[[1]]) +
    annotation_scale(location = "br", width_hint = 0.5, style = "ticks")
  
  return(p)
}