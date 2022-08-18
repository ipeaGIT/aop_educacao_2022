

get_landuse_data <- function() {
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
  
  
  # retorna o data.frame processado, em formato longo
  return(data_processed)
}

# data <- tar_read(data_by_hex)
aggregate_landuse_by_city <- function(data) {
  
  data_aggregated <- data |> 
    group_by(abbrev_muni, name_muni, code_muni, idade, nivel_ensino) |> 
    summarise(populacao = sum(populacao, na.rm = T),
              matriculas = sum(matriculas, na.rm = T),
              .groups = "drop") |> 
    group_by(abbrev_muni) |> 
    mutate(pop_total = sum(populacao), mat_total = sum(matriculas))
  
  
  return(data_aggregated)
}


