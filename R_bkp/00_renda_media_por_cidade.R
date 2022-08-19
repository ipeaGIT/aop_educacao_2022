library(aopdata)
library(dplyr)
library(tidyr)

data_df <- aopdata::read_landuse(city="all", geometry = FALSE)

data_df |> 
  filter(R003 %in% c(1, 5, 10), P001 > 0) |> 
  group_by(abbrev_muni, name_muni, R003) |> 
  summarise(renda_media = weighted.mean(R001, P001)) |> 
  pivot_wider(names_from = R003, values_from = renda_media, names_prefix = "decil_") 




