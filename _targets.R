# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

suppressPackageStartupMessages({
  library(geobr)
  library(aopdata)
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(ggplot2)
  library(forcats)
  library(sf)
  library(ggspatial)
  library(ggalt)
  library(scales)
  library(patchwork)
  library(h3jsr)
  library(knitr)
  library(kableExtra)
})

# Set target options:
tar_option_set(
  packages = c("tibble"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  
  # preparação dos dados
  tar_target(hexgrid, download_hexgrid()),
  tar_target(limites_municipais, download_limites_municipais()),
  tar_target(pop_mat_por_hex, download_uso_do_solo()),
  tar_target(pop_mat_por_cidade, agregar_uso_do_solo_por_cidade(pop_mat_por_hex)),
  tar_target(pop_por_decil, agregar_populacao_por_decil(pop_mat_por_hex)),
  tar_target(cobertura_de_vagas, calcular_cobertura_de_vagas(pop_mat_por_cidade)),
  
  tar_target(acessibilidade_por_hex, download_acessibilidade()),
  tar_target(insuficiencia_ens_infantil_por_hex, calcular_insuficiencia_ens_infantil_por_hex(pop_mat_por_hex, acessibilidade_por_hex)),
  tar_target(insuficiencia_ens_infantil_por_cidade, calcular_insuficiencia_ens_infantil_por_cidade(insuficiencia_ens_infantil_por_hex)),
  tar_target(insuficiencia_ens_fundamental_por_hex, calcular_insuficiencia_ens_fundamental_por_hex(pop_mat_por_hex, acessibilidade_por_hex)),
  tar_target(insuficiencia_ens_fundamental_por_cidade, calcular_insuficiencia_ens_fundamental_por_cidade(insuficiencia_ens_fundamental_por_hex)),
  tar_target(insuficiencia_ens_medio_por_hex, calcular_insuficiencia_ens_medio_por_hex(pop_mat_por_hex, acessibilidade_por_hex)),
  tar_target(insuficiencia_ens_medio_por_cidade, calcular_insuficiencia_ens_medio_por_cidade(insuficiencia_ens_medio_por_hex)),
  
  # tabelas, figuras e mapas para o relatório
  
  tar_target(tabela_insuficiencia, criar_tabela_geral_insuficiencia(insuficiencia_ens_infantil_por_cidade, insuficiencia_ens_fundamental_por_cidade, insuficiencia_ens_medio_por_cidade), format = "file"),
  tar_target(tabela_renda_media, criar_tabela_renda_media(), format = "file"),
  tar_target(tabela_escolas, criar_tabela_escolas(), format = "file"),
  tar_target(tabela_matriculas, criar_tabela_matriculas(), format = "file"),
  
  tar_target(figura_cobertura_de_vagas, plotar_cobertura_de_vagas(cobertura_de_vagas, pop_por_decil), format = "file"),
  tar_target(figura_insuficiencia_ens_infantil, plotar_insuficiencia_ens_infantil(insuficiencia_ens_infantil_por_cidade), format = "file"),
  tar_target(mapa_insuficiencia_ens_infantil, mapear_insuficiencia_ens_infantil(hexgrid, limites_municipais, insuficiencia_ens_infantil_por_hex), format = "file"),

  tar_target(figura_insuficiencia_ens_medio, plotar_insuficiencia_ens_medio(insuficiencia_ens_medio_por_cidade), format = "file"),
  tar_target(mapa_insuficiencia_ens_medio, mapear_insuficiencia_ens_medio(hexgrid, limites_municipais, insuficiencia_ens_medio_por_hex), format = "file"),
  
  tar_target(munis, lista_munis(pop_mat_por_hex)), 
  tar_target(num_figs, 1:20), 
  tar_target(mapa_insuficiencia_ens_infantil_medio, 
             mapear_insuficiencia_cidade(muni = munis, num_fig = num_figs,  hexgrid, limites_municipais, 
                                         insuficiencia_ens_infantil_por_hex, insuficiencia_ens_medio_por_hex), 
             pattern = map(munis, num_figs), format = "file")
  
  
)



