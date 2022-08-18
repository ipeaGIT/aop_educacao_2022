library(sf)
library(tidyverse)
library(aopdata)

poa_sf <- aopdata::read_landuse(city="poa", geometry = TRUE)

poa_sf <- poa_sf %>% filter(P001 > 0)

mapview::mapview(poa_sf, zcol="P001")

st_write(poa_sf, "poa.gpkg")

geral_df <- full_df %>%
  filter(P001 > 0, R002 > 0, modo == "caminhada") %>%
  select(id_hex, sigla_muni, nome_muni, P001, R002, R003, 
         E001, M001, M002, M003, M004, CMAMT30, CMAET30) %>%
  drop_na()


ratio_df <- geral_df %>%
  group_by(sigla_muni, nome_muni, R003) %>%
  mutate(ratio = CMAMT30 / P001) %>%
  summarise(ratio_pop_mat = mean(ratio, na.rm = T))

ratio_df %>%
  mutate(income = factor(R003)) %>%
  ggplot(aes(x=income, y = ratio_pop_mat)) +
  geom_path(aes(group=nome_muni)) +
  geom_point(aes(color = income)) +
  facet_wrap(~nome_muni) +
  labs(title = "Escolas acessíveis em 30min de caminhada",
       subtitle = "média ponderada por por quintil de renda")

  

data_df <- aopdata::read_landuse(city="all", geometry = FALSE)

data_df |> 
  filter(R003 %in% c(1, 5, 10), P001 > 0) |> 
  group_by(abbrev_muni, name_muni, R003) |> 
  summarise(renda_media = weighted.mean(R001, P001)) |> 
  pivot_wider(names_from = R003, values_from = renda_media, names_prefix = "decil_") |> 
  write_tsv(file = "renda_media.txt")




