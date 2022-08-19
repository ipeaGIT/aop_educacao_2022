criar_tabela_renda_media <- function() {
  # baixar dados do aopdata
  data_df <- aopdata::read_landuse(city="all", geometry = FALSE)
  
  tabela_df <- data_df |> 
    filter(R003 %in% c(1, 5, 10), P001 > 0) |> 
    group_by(abbrev_muni, name_muni, R003) |> 
    summarise(renda_media = weighted.mean(R001, P001)) |> 
    pivot_wider(names_from = R003, values_from = renda_media, names_prefix = "decil_") 
  
  tabela_df <- tabela_df |> 
    mutate(name_muni = recode(name_muni, 
                              "Goiania" = "RM Goiânia",
                              "Belem" = "Belém",
                              "Brasilia" = "Brasília",
                              "Maceio" = "Maceió",
                              "Sao Goncalo" = "São Gonçalo",
                              "Sao Luis" = "São Luís",
                              "Sao Paulo" = "São Paulo"))
  
  tabela_md <- kable(tabela_df, format = "markdown", digits = 2)

  # save table
  txt_file <- here::here("output", "tab_a1_renda_media.txt")
  
  save_kable(tabela_md, file = txt_file)
  
  return(txt_file)

}

criar_tabela_escolas <- function() {
  # baixar dados do aopdata
  data_df <- aopdata::read_landuse(city="all", geometry = FALSE)
  
  tabela_df <- data_df |> 
    group_by(abbrev_muni, name_muni) |> 
    summarise(infantil = sum(E002, na.rm = TRUE),
              fundamental = sum(E003, na.rm = TRUE),
              medio = sum(E004, na.rm = TRUE),
              total = sum(E001, na.rm = TRUE),
              .groups = "drop")
  
  tabela_df <- tabela_df |> 
    mutate(name_muni = recode(name_muni, 
                              "Goiania" = "RM Goiânia",
                              "Belem" = "Belém",
                              "Brasilia" = "Brasília",
                              "Maceio" = "Maceió",
                              "Sao Goncalo" = "São Gonçalo",
                              "Sao Luis" = "São Luís",
                              "Sao Paulo" = "São Paulo"))
  
  tabela_md <- kable(tabela_df, format = "markdown", digits = 0)
  
  # save table
  txt_file <- here::here("output", "tab_b1_qtd_escolas.txt")
  
  save_kable(tabela_md, file = txt_file)
  
  return(txt_file)
}


criar_tabela_matriculas <- function() {
  # baixar dados do aopdata
  data_df <- aopdata::read_landuse(city="all", geometry = FALSE)
  
  tabela_df <- data_df |> 
    group_by(abbrev_muni, name_muni) |> 
    summarise(infantil = sum(M002, na.rm = TRUE),
              fundamental = sum(M003, na.rm = TRUE),
              medio = sum(M004, na.rm = TRUE),
              total = sum(M001, na.rm = TRUE),
              .groups = "drop")
  
  tabela_df <- tabela_df |> 
    mutate(name_muni = recode(name_muni, 
                              "Goiania" = "RM Goiânia",
                              "Belem" = "Belém",
                              "Brasilia" = "Brasília",
                              "Maceio" = "Maceió",
                              "Sao Goncalo" = "São Gonçalo",
                              "Sao Luis" = "São Luís",
                              "Sao Paulo" = "São Paulo"))
  
  tabela_md <- kable(tabela_df, format = "markdown", digits = 0)
  
  # save table
  txt_file <- here::here("output", "tab_b2_qtd_matriculas.txt")
  
  save_kable(tabela_md, file = txt_file)
  
  return(txt_file)
}
