# insuf_infantil <- tar_read(insuficiencia_ens_infantil_por_cidade)
# insuf_fundamental <- tar_read(insuficiencia_ens_fundamental_por_cidade)
# insuf_medio <- tar_read(insuficiencia_ens_medio_por_cidade)
criar_tabela_geral_insuficiencia <- function(insuf_infantil, insuf_fundamental, insuf_medio) {
  infantil_df <- insuf_infantil |> 
    select(name_muni, nivel_ensino, populacao, li_15_pc, li_30_pc)

  fundamental_df <- insuf_fundamental |> 
    select(name_muni, nivel_ensino, populacao, li_15_pc, li_30_pc)

  medio_df <- insuf_medio |> 
    select(name_muni, nivel_ensino, populacao, li_0_pc, li_1_pc, li_3_pc)
  
  geral_df <- 
    left_join(infantil_df, fundamental_df, 
              by = c("name_muni"),
              suffix = c(".inf", ".fund")) |> 
    left_join(medio_df,
              by = c("name_muni"))
  
  geral_df <- geral_df |> 
    mutate(across(contains("_pc"), scales::percent, accuracy = 0.1))
  
  tabela_md <- kable(geral_df, format = "markdown", digits = 1)
  
  # save table
  txt_file <- here::here("output", "tab_3_insuficiecia_geral.md")
  
  save_kable(tabela_md, file = txt_file)
  
  return(txt_file)
  
}

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
  txt_file <- here::here("output", "tab_a1_renda_media.md")
  
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
  txt_file <- here::here("output", "tab_b1_qtd_escolas.md")
  
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
  txt_file <- here::here("output", "tab_b2_qtd_matriculas.md")
  
  save_kable(tabela_md, file = txt_file)
  
  return(txt_file)
}
