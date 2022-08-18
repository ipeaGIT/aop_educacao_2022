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
  library(stringr)
  library(ggplot2)
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
  tar_target(pop_mat_por_hex, download_uso_do_solo()),
  tar_target(pop_mat_por_cidade, agregar_uso_do_solo_por_cidade(pop_mat_por_hex)),
  tar_target(pop_por_decil, agregar_populacao_por_decil(pop_mat_por_hex)),
  tar_target(cobertura_de_vagas, calcular_cobertura_de_vagas(pop_mat_por_cidade)),
  
  
  # tabelas, figuras e mapas para o relatório
  
  tar_target(figura_cobertura_de_vagas, plotar_cobertura_de_vagas(cobertura_de_vagas, pop_por_decil), format = "file")
  
)



