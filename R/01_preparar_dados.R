
# Geral -------------------------------------------------------------------

## HexGrid -----------------------------------------------------------------

download_hexgrid <- function() {
  # baixar dados do aopdata
  data <- aopdata::read_grid(city="all", showProgress = FALSE)
  
  data_recoded <- data |> 
    mutate(name_muni = recode(name_muni, 
                              "Goiania" = "RM Goiânia",
                              "Belem" = "Belém",
                              "Brasilia" = "Brasília",
                              "Maceio" = "Maceió",
                              "Sao Goncalo" = "São Gonçalo",
                              "Sao Luis" = "São Luís",
                              "Sao Paulo" = "São Paulo"))

  return(data_recoded)
}


download_limites_municipais <- function() {
  # baixar dados do geobr
  data <- geobr::read_municipality()
  
  return(data)
}

## Uso do Solo -------------------------------------------------------------

download_uso_do_solo <- function() {
  # baixar dados do aopdata
  data <- aopdata::read_landuse(city="all", geometry = FALSE, year = 2019, showProgress = FALSE)
  
  # extrair dados dos hexágonos
  hex_data <- data |>  select(id_hex, abbrev_muni, name_muni, code_muni)
  
  # extrair dados da população em idade escolar
  pop_data <- data |>
    select(id_hex, 
           renda_decil = R003,
           pop_0a5   = P010, 
           pop_6a14  = P011, 
           pop_15a18 = P012) 

  ## converter para formato longo, com colunas separadas para faixa de idade e população
  pop_data <- pop_data |> 
    pivot_longer(cols = starts_with("pop"), names_to = "idade", values_to = "populacao") |> 
    mutate(idade = str_remove(idade, "pop_"))
  
  
  # extrair dados das matrículas por nível de ensino
  mat_data <- data |> 
    select(id_hex,
           mat_infantil    = M002,
           mat_fundamental = M003,
           mat_medio       = M004)
  
  ## converter para formato longo, com colunas separadas para nível de ensino e número de matrículas
  mat_data <- mat_data |> 
    pivot_longer(cols = starts_with("mat"), names_to = "nivel_ensino", values_to = "matriculas") |> 
    mutate(nivel_ensino = str_remove(nivel_ensino, "mat_"))
  
  ## adicionar faixa de idade referente a cada nível de ensino, para permitir o join com a tabela de população
  mat_data <- mat_data |> 
    mutate(idade = case_when(nivel_ensino == "infantil" ~ "0a5",
                             nivel_ensino == "fundamental" ~ "6a14",
                             nivel_ensino == "medio" ~ "15a18"))

  # join entre os dados dos hexágonos, população e matrículas
  data_processed <- hex_data |> 
    left_join(pop_data, by = "id_hex") |> 
    left_join(mat_data, by = c("id_hex", "idade")) |> 
    filter(populacao + matriculas > 0)
  
  data_recoded <- data_processed |> 
    mutate(name_muni = recode(name_muni, 
                              "Goiania" = "RM Goiânia",
                              "Belem" = "Belém",
                              "Brasilia" = "Brasília",
                              "Maceio" = "Maceió",
                              "Sao Goncalo" = "São Gonçalo",
                              "Sao Luis" = "São Luís",
                              "Sao Paulo" = "São Paulo"))

  # retorna o data.frame processado, em formato longo
  return(data_recoded)
}

# data <- tar_read(pop_mat_por_hex)
agregar_uso_do_solo_por_cidade <- function(data) {
  
  data_aggregated <- data |> 
    group_by(abbrev_muni, name_muni, code_muni, idade, nivel_ensino) |> 
    summarise(populacao = sum(populacao, na.rm = T),
              matriculas = sum(matriculas, na.rm = T),
              .groups = "drop") |> 
    group_by(abbrev_muni) |> 
    mutate(pop_total = sum(populacao), mat_total = sum(matriculas)) 
  
  data_aggregated$nivel_ensino <- factor(data_aggregated$nivel_ensino,
                                         levels = c("infantil", "fundamental", "medio"),
                                         labels = c("Infantil", "Fundamental", "Médio"))
  
  return(data_aggregated)
}

# data <- tar_read(pop_mat_por_hex)
agregar_populacao_por_decil <- function(data) {
  data_processed <- data |> 
    group_by(abbrev_muni, name_muni, code_muni, renda_decil, idade, nivel_ensino) |> 
    summarise(populacao = sum(populacao, na.rm = TRUE), .groups = "drop") |> 
    filter(renda_decil != 0) |> 
    mutate(renda_decil = factor(renda_decil)) |> 
    group_by(abbrev_muni, name_muni, code_muni, idade) |> 
    mutate(proporcao = populacao / sum(populacao)) 
    
  data_processed$nivel_ensino <- factor(data_processed$nivel_ensino,
                                        levels = c("infantil", "fundamental", "medio"),
                                        labels = c("Infantil", "Fundamental", "Médio"))
  
  return(data_processed)
}

# data <- tar_read(pop_mat_por_cidade)
calcular_cobertura_de_vagas <- function(data) {
  data_processed <- data |> 
    mutate(cobertura = matriculas / populacao) |>
    select(-ends_with("total"))
  
  factor_cidade <- data_processed |> 
    filter(nivel_ensino == "Fundamental") |> 
    arrange(cobertura) |> 
    pull(name_muni)
  
  data_processed$name_muni <- factor(data_processed$name_muni,
                                     levels = factor_cidade)
  
  return(data_processed)
   
}

# pop_mat_por_hex <- tar_read(pop_mat_por_hex)
lista_munis <- function(pop_mat_por_hex) {
  
  munis <- pop_mat_por_hex |> 
    select(abbrev_muni) |> 
    distinct() |> 
    arrange(abbrev_muni) |> 
    pull()
  
  return(munis)
  
}





# Acessibilidade ----------------------------------------------------------

download_acessibilidade <- function() {
  # baixar dados do aopdata - caminhada
  data_walk <- aopdata::read_access(city="all", mode = "walk",
                               geometry = FALSE, year = 2019, showProgress = FALSE)

  # extrair dados dos hexágonos
  hex_data <- data_walk |>  select(id_hex, abbrev_muni, name_muni, code_muni) |> 
    distinct()
  
  # escola de educação infantil mais próxima, a pé
  tmi_infantil_caminhada <- data_walk |> select(id_hex, TMIEI) |> 
    drop_na() |> 
    distinct()
  
  # baixar dados do aopdata - transporte público, horário de pico (6am - 8am)
  data_pt <- aopdata::read_access(city=c("bho", "cam", "for", "goi", "poa", "rec", "rio", "spo", "cur"), 
                                  mode = "public_transport", peak = TRUE,
                                  geometry = FALSE, year = 2019, showProgress = FALSE)
  
  cma_medio_tp <- data_pt |> select(id_hex, CMAEM30) |> 
    drop_na() |> 
    distinct()
  
  # join entre os dados dos hexágonos e de acessibilidade
  data_processed <- hex_data |> 
    left_join(tmi_infantil_caminhada, by = "id_hex") |> 
    left_join(cma_medio_tp, by = "id_hex") 

  data_recoded <- data_processed |> 
    mutate(name_muni = recode(name_muni, 
                              "Goiania" = "RM Goiânia",
                              "Belem" = "Belém",
                              "Brasilia" = "Brasília",
                              "Maceio" = "Maceió",
                              "Sao Goncalo" = "São Gonçalo",
                              "Sao Luis" = "São Luís",
                              "Sao Paulo" = "São Paulo"))
  
  return(data_recoded)
}

# Ensino Infantil ---------------------------------------------------------

# pop_mat_por_hex <- tar_read(pop_mat_por_hex)
# acessibilidade_por_hex <- tar_read(acessibilidade_por_hex)
calcular_insuficiencia_ens_infantil_por_hex <- function(pop_mat_por_hex, acessibilidade_por_hex) {
  pop_df <- pop_mat_por_hex |> filter(idade == "0a5", populacao > 0, renda_decil <= 5)
  acessibilidade_df <- left_join(pop_df, acessibilidade_por_hex, by = c("id_hex", "abbrev_muni", "name_muni", "code_muni"))
  
  acessibilidade_df <- acessibilidade_df |> 
    replace_na(list(TMIEI = Inf)) |> 
    mutate(pop_li_15 = if_else(TMIEI > 15, populacao, 0L),
           pop_li_30 = if_else(TMIEI > 30, populacao, 0L))
  
  
  return(acessibilidade_df)
}

# insuficiencia_ens_infantil_por_hex <- tar_read(insuficiencia_ens_infantil_por_hex)
calcular_insuficiencia_ens_infantil_por_cidade <- function(insuficiencia_ens_infantil_por_hex) {
  acessibilidade_df <- insuficiencia_ens_infantil_por_hex |> 
    group_by(abbrev_muni, name_muni, code_muni, idade, nivel_ensino) |> 
    summarise(populacao = sum(populacao),
              li_15 = sum(pop_li_15),
              li_30 = sum(pop_li_30),
              .groups = "drop") |> 
    mutate(li_15_pc = li_15 / populacao,
           li_30_pc = li_30 / populacao)
  
  return(acessibilidade_df)
}
  

# Ensino Fundamental ------------------------------------------------------

# pop_mat_por_hex <- tar_read(pop_mat_por_hex)
# acessibilidade_por_hex <- tar_read(acessibilidade_por_hex)
calcular_insuficiencia_ens_fundamental_por_hex <- function(pop_mat_por_hex, acessibilidade_por_hex) {
  pop_df <- pop_mat_por_hex |> filter(idade == "6a14", populacao > 0, renda_decil <= 5)
  acessibilidade_df <- left_join(pop_df, acessibilidade_por_hex, by = c("id_hex", "abbrev_muni", "name_muni", "code_muni"))
  
  acessibilidade_df <- acessibilidade_df |> 
    replace_na(list(TMIEI = Inf)) |> 
    mutate(pop_li_15 = if_else(TMIEI > 15, populacao, 0L),
           pop_li_30 = if_else(TMIEI > 30, populacao, 0L))
  
  
  return(acessibilidade_df)
}

# insuficiencia_ens_fundamental_por_hex <- tar_read(insuficiencia_ens_fundamental_por_hex)
calcular_insuficiencia_ens_fundamental_por_cidade <- function(insuficiencia_ens_fundamental_por_hex) {
  acessibilidade_df <- insuficiencia_ens_fundamental_por_hex |> 
    group_by(abbrev_muni, name_muni, code_muni, idade, nivel_ensino) |> 
    summarise(populacao = sum(populacao),
              li_15 = sum(pop_li_15),
              li_30 = sum(pop_li_30),
              .groups = "drop") |> 
    mutate(li_15_pc = li_15 / populacao,
           li_30_pc = li_30 / populacao)
  
  return(acessibilidade_df)
}

# Ensino Médio ------------------------------------------------------------

# pop_mat_por_hex <- tar_read(pop_mat_por_hex)
# acessibilidade_por_hex <- tar_read(acessibilidade_por_hex)
calcular_insuficiencia_ens_medio_por_hex <- function(pop_mat_por_hex, acessibilidade_por_hex) {
  pop_df <- pop_mat_por_hex |> filter(idade == "15a18", populacao > 0, renda_decil <= 5)
  acessibilidade_df <- left_join(pop_df, acessibilidade_por_hex, by = c("id_hex", "abbrev_muni", "name_muni", "code_muni"))
  
  # filtrar cidades com transporte público
  acessibilidade_df <- acessibilidade_df |> filter(abbrev_muni %in% c("bho", "cam", "for", "goi", "poa", "rec", "rio", "spo", "cur"))
  
  acessibilidade_df <- acessibilidade_df |> 
    select(-TMIEI) |> 
    drop_na() |> 
    mutate(pop_li_0 = if_else(CMAEM30 <= 0, populacao, 0L),
           pop_li_1 = if_else(CMAEM30 <= 1, populacao, 0L),
           pop_li_3 = if_else(CMAEM30 <= 3, populacao, 0L))

  return(acessibilidade_df)
}

# insuficiencia_ens_medio_por_hex <- tar_read(insuficiencia_ens_medio_por_hex)
calcular_insuficiencia_ens_medio_por_cidade <- function(insuficiencia_ens_medio_por_hex) {
  acessibilidade_df <- insuficiencia_ens_medio_por_hex |> 
    group_by(abbrev_muni, name_muni, code_muni, idade, nivel_ensino) |> 
    summarise(populacao = sum(populacao),
              li_0 = sum(pop_li_0),
              li_1 = sum(pop_li_1),
              li_3 = sum(pop_li_3),
              .groups = "drop") |> 
    mutate(li_0_pc = li_0 / populacao,
           li_1_pc = li_1 / populacao,
           li_3_pc = li_3 / populacao)
  
  return(acessibilidade_df)
}
