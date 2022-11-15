
# Ensino Infantil ---------------------------------------------------------


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
    # plot_annotation(title = "Crianças de 0 a 5 anos de idade a mais de 15 ou 30 minutos\nde caminhada da creche mais próxima") +
    plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "bottom")
  
  # save plot
  figura <- here::here("output", "fig_03_mapa_insuficiencia_ens_infantil.png")
  
  ggsave(plot = p, filename = figura, 
         width = 16, height = 17, units = "cm", dpi = 300, scale=1.2)

  return(figura)
}


# Ensino Médio ------------------------------------------------------------


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
                           titulo = "Linha de Insuficiência: 1 escola")

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
    # plot_annotation(title = "Jovens de 15 a 18 anos de idade com acesso a até 0, 1 ou 3 escolas em 30 minutos de viagem por transporte público") +
    plot_layout(ncol = 3, guides = "collect") & theme(legend.position = "bottom")
  
  # save plot
  figura <- here::here("output", "fig_05_mapa_insuficiencia_ens_medio.png")
  
  ggsave(plot = p, filename = figura, 
         width = 16, height = 17, units = "cm", dpi = 300, scale=1.2)
  
  return(figura)
}


# Anexo, por cidade -------------------------------------------------------

# muni <- "slz"
# hexgrid <- tar_read(hexgrid)
# limites_municipais <- tar_read(limites_municipais)
# insuficiencia_ens_infantil_por_hex <- tar_read(insuficiencia_ens_infantil_por_hex)
# insuficiencia_ens_medio_por_hex <- tar_read(insuficiencia_ens_medio_por_hex)
mapear_insuficiencia_cidade <- function(muni, num_fig, hexgrid, limites_municipais, 
                                        insuficiencia_ens_infantil_por_hex, insuficiencia_ens_medio_por_hex) {
  
  # nome da cidade
  name_muni <- insuficiencia_ens_infantil_por_hex |> 
    filter(abbrev_muni == muni) |> 
    slice(1) |> 
    pull(name_muni)
  
  # número máximo de jovens em situação de insuficiência de acessibilidade, 
  # usado para uniformizar as legendas
  max_pop_inf <- insuficiencia_ens_infantil_por_hex |> 
    filter(abbrev_muni == muni) |> 
    summarise(max_pop = max(pop_li_15)) |> 
    pull(max_pop)

  max_pop_med <- insuficiencia_ens_medio_por_hex |> 
    filter(abbrev_muni == muni) |> 
    summarise(max_pop = max(pop_li_3)) |> 
    pull(max_pop)
  
  max_pop = max(max_pop_inf, max_pop_med, na.rm = TRUE)
  
  # Mapas ensino infantil
  m_inf_15 <- mapear_cidade_anx(cidade = muni, hexgrid = hexgrid, limite = limites_municipais,
                                insuf_hex = insuficiencia_ens_infantil_por_hex, 
                                li = "pop_li_15", max_pop = max_pop,
                                titulo = "Linha de Insuficiência: 15 minutos") +
    labs(subtitle = "Educação Infantil")
  
  m_inf_30 <- mapear_cidade_anx(cidade = muni, hexgrid = hexgrid, limite = limites_municipais,
                                insuf_hex = insuficiencia_ens_infantil_por_hex, 
                                li = "pop_li_30", max_pop = max_pop,
                                titulo = "Linha de Insuficiência: 30 minutos")
  
  # Mapas ensino médio
  m_med_0 = NULL
  m_med_0 <- mapear_cidade_anx(cidade = muni, hexgrid = hexgrid, limite = limites_municipais,
                               insuf_hex = insuficiencia_ens_medio_por_hex, 
                               li = "pop_li_0", max_pop = max_pop,
                               titulo = "Linha de Insuficiência: 0 escolas") 
  if (!is.null(m_med_0)) {
    m_med_0 = m_med_0 + labs(subtitle = "Ensino Médio")
  }
  
  m_med_1 <- mapear_cidade_anx(cidade = muni, hexgrid = hexgrid, limite = limites_municipais,
                               insuf_hex = insuficiencia_ens_medio_por_hex, 
                               li = "pop_li_1", max_pop = max_pop,
                               titulo = "Linha de Insuficiência: 1 escola")
  
  m_med_3 <- mapear_cidade_anx(cidade = muni, hexgrid = hexgrid, limite = limites_municipais,
                               insuf_hex = insuficiencia_ens_medio_por_hex, 
                               li = "pop_li_3", max_pop = max_pop,
                               titulo = "Linha de Insuficiência: 3 escolas")
  
  p <- m_inf_15 + m_inf_30 + plot_spacer() +
    m_med_0 + m_med_1 + m_med_3 +
    plot_annotation(title = name_muni) +
    plot_layout(ncol = 3, guides = "collect") & theme(legend.position = "bottom")
  

  p
  # save plot
  figura <- here::here("output/anexos", paste0("fig_anx_c", num_fig, "_mapa_insuficiencia_", muni, ".png"))
  
  ggsave(plot = p, filename = figura, 
         width = 18, height = 16, units = "cm", dpi = 300, scale=1.2)
  
  return(figura)
  
}


# Mapa por Cidade ---------------------------------------------------------

# cidade = "rec"
# hexgrid = hexgrid
# limite = limites_municipais
# insuf_hex = insuficiencia_ens_infantil_por_hex
# li = "pop_li_15"
# max_pop = 305L
# titulo = "Linha de Insuficiência: 15 minutos"
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
  
  # transformar escolas em pontos
  schools_points <- pop_grid |> 
    filter(matriculas > 0) |>
    mutate(escolas = "1") |> 
    st_centroid()
  
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
    # geom_sf(data=filter(pop_grid, matriculas > 0), fill="steelblue4", color="steelblue4", size = 0.05) +
    geom_sf(data = schools_points, aes(color = escolas), size = 0.8) +
    coord_sf(datum = NA,
             xlim = c(b_box["xmin"], b_box["xmax"]),
             ylim = c(b_box["ymin"], b_box["ymax"])) +
    scale_fill_viridis_c(option = "inferno", limits = c(0, max_pop), begin = 0.1, end = 0.9, direction = -1) +
    scale_color_manual(values = "steelblue4") +
    guides(color = guide_legend(label = FALSE)) +
    labs(fill = "Número\nde crianças",
         color = "Escolas",
         subtitle = titulo) +
    theme_void() +
    theme(legend.position = "bottom",
          panel.border = element_rect(fill = NA, color = "grey20")) +
    annotate(geom = "text", x = b_box["xmin"], y = b_box["ymax"], hjust = 0,
             label = centroid_sf$name_muni[[1]]) +
    annotation_scale(location = "br", width_hint = 0.5, style = "ticks")
  
  p
  return(p)
}

# cidade = "goi"
# hexgrid = tar_read(hexgrid)
# limite = tar_read(limites_municipais)
# insuf_hex = tar_read(insuficiencia_ens_medio_por_hex)
# li = "pop_li_3"
# max_pop = 305L
# titulo = "Linha de Insuficiência: 15 minutos"
mapear_cidade_anx <- function(cidade, hexgrid, limite,
                          insuf_hex, li = "pop_li_15", 
                          max_pop = 500, titulo = NULL) {
  # filtrar dados da cidade selecionada
  grid <- hexgrid |> filter(abbrev_muni == cidade)
  insuf_df <- insuf_hex |> filter(abbrev_muni == cidade)
  
  if (nrow(insuf_df) == 0) {
    return(NULL)
  }
  
  muni = insuf_df |> head(1) |> pull(code_muni)
  limite <- limite |> filter(code_muni == muni)
  
  # juntar grid espacial e dados de insuficiencia de acessibilidade
  pop_grid <- inner_join(grid, insuf_df, by = c("id_hex", "abbrev_muni", "name_muni", "code_muni")) |> 
    mutate(pop = get(li))
  
  # transformar escolas em pontos
  schools_points <- pop_grid |> 
    filter(matriculas > 0) |>
    mutate(escolas = "1") |> 
    st_centroid()
  
  # calcular centroide da cidade, para centralizar o mapa
  points <- h3jsr::cell_to_point(pop_grid$id_hex, simple = F)
  
  centroid_sf <- points |> 
    summarise(.groups = "drop") |> 
    st_centroid()
  
  centroid_sf$name_muni <- pop_grid$name_muni[[1]]
  
  # calcular área ao redor do centroide, para que todos os mapas tenham a mesma escala
  size <- get_layer_buffer_size(points)
  pt_size <- 0.8
  if (size >= 20) {
    pt_size <- 0.5
  }
  # size = 16
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
    # geom_sf(data=filter(pop_grid, matriculas > 0), fill="steelblue4", color="steelblue4", size = 0.05) +
    geom_sf(data = schools_points, aes(color = escolas), size = pt_size) +
    coord_sf(datum = NA,
             xlim = c(b_box["xmin"], b_box["xmax"]),
             ylim = c(b_box["ymin"], b_box["ymax"])) +
    scale_fill_viridis_c(option = "inferno", limits = c(0, max_pop), begin = 0.1, end = 0.9, direction = -1) +
    scale_color_manual(values = "steelblue4") +
    guides(color = guide_legend(label = FALSE)) +
    labs(fill = "Número\nde crianças",
         color = "Escolas") +
    theme_void() +
    theme(legend.position = "bottom",
          panel.border = element_rect(fill = NA, color = "grey20")) +
    annotate(geom = "text", x = b_box["xmin"], y = b_box["ymax"], hjust = 0,
             label = titulo) +
    annotation_scale(location = "br", width_hint = 0.5, style = "ticks")
  
  p
  return(p)
}
