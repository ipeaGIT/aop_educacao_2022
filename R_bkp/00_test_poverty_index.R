# load support functions -------------------------------------------------

source("R/fun/setup.R")

library(scales)

# load data ---------------------------------------------------------------

access_active_df <- aopdata::read_access(city="all", year = 2019)


tar_load(acessibilidade_por_hex)
tar_load(pop_mat_por_hex)

access_df <- left_join(pop_mat_por_hex, acessibilidade_por_hex,
                       by = c("id_hex", "abbrev_muni", "name_muni", "code_muni")) |> 
  select(-matriculas, - CMAEM30) |> 
  filter(renda_decil <= 5, renda_decil > 0)




# cálculo do indíce P2 de pobreza de acessibilidade -----------------------------------------------

## preparação dos dados

geral_df <- access_active_df %>%
  filter(P010 > 0, R003 > 0, R003 <= 5,mode == "walk") %>%
  select(id_hex, abbrev_muni, name_muni, 
         populacao = P001, 
         pop_criancas = P010, 
         renda_decil = R003,
         TMIEI) %>%
  mutate(tempo_viagem = case_when(TMIEI <= 15 ~ "15 min",
                                  TMIEI <= 30 ~ "30 min",
                                  TRUE ~ "-")) %>%
  mutate(renda_decil = factor(renda_decil),
         tempo_viagem = factor(tempo_viagem, levels = c("-", "30 min", "15 min"))) %>%
  drop_na()

poa_df <- geral_df %>%
  filter(abbrev_muni == "poa")
  
group_by(abbrev_muni, name_muni) %>%
  nest() %>%
  mutate(p0 = map(data$pop_criancas, p0))


# x <- poa_df$TMIEI
# wt <- poa_df$pop_criancas
# k <- 15

P0 <- function(x, k, wt) {
  i_poor <- which(x > k)
  n_poor <- sum(wt[i_poor])
  n <- sum(wt)
  
  return(n_poor / n)
}

P1 <- function(x, k, wt) {
  # infinite TMI means no opportunity was reached within the chosen max trip 
  # duration. thus, just change infinite values for the max TMI.
  x[is.infinite(x)] <- max(x[!is.infinite(x)], na.rm = T)
  x[is.na(x)] <- max(x[!is.infinite(x) & !is.na(x)], na.rm = T)
  
  # 1. calculate gap between TMI and threshold k
  gap <- x - k
  
  # 1.1 negative gaps mean those people are above poverty line k, so just change
  # them to zero
  gap[gap < 0] <- 0
  
  # 2. calculate proportional gap, which is gap / max(gap)
  proportional_gap <- gap / max(gap)
  
  # 3. weight gaps by population (wt)
  weighted_gap <-  proportional_gap * wt
  
  # 4. calculate poverty index P1, which is the averaged weighted gap
  p1_index <- sum(weighted_gap) / sum(wt)

  return(p1_index)
}

P2 <- function(x, k, wt) {
  # infinite or NA TMI means no opportunity was reached within the chosen max trip 
  # duration. thus, just change infinite values for the max TMI.
  x[is.infinite(x)] <- max(x[!is.infinite(x)], na.rm = T)
  x[is.na(x)] <- max(x[!is.infinite(x) & !is.na(x)], na.rm = T)
  
  
  # 1. calculate gap between TMI and threshold k
  gap <- x - k
  
  # 1.1 negative gaps mean those people are above poverty line k, so just change
  # them to zero
  gap[gap < 0] <- 0
  
  # 1.2 square the gaps
  gap <- gap ^ 2
  
  # 2. calculate proportional gap, which is gap / max(gap)
  proportional_gap <- gap / max(gap)
  
  # 3. weight gaps by population (wt)
  weighted_gap <-  proportional_gap * wt
  
  # 4. calculate poverty index P1, which is the averaged weighted gap
  p1_index <- sum(weighted_gap) / sum(wt)
  
  return(p1_index)
}


setDT(access_df)


# 15 minutes --------------------------------------------------------------



summary_15_df <- access_df[, .(p0 = P0(TMIEI, 15, populacao),
                            p1 = P1(TMIEI, 15, populacao),
                            p2 = P2(TMIEI, 15, populacao)), by = .(abbrev_muni, name_muni, idade, nivel_ensino)]

summary_15_df %>%
  filter(idade == "0a5") |> 
  mutate(name_muni = fct_reorder(name_muni, p0)) |> 
  pivot_longer(cols = starts_with("p"), names_to = "index") %>%
  ggplot() +
  geom_col(aes(x=name_muni, y=value, fill = index), position = "dodge") +
  coord_flip() +
  # scale_y_continuous(limits = c(0, 0.3)) +
  facet_wrap(~index, scales = "free_x") +
  labs(title = "TMI - 15 minutos") +
  theme(legend.position = "none")



# 30 minutes --------------------------------------------------------------


summary_30_df <- access_df[, .(p0 = P0(TMIEI, 30, populacao),
                            p1 = P1(TMIEI, 30, populacao),
                            p2 = P2(TMIEI, 30, populacao)), by = .(abbrev_muni, name_muni, idade, nivel_ensino)]

summary_30_df %>%
  filter(idade == "0a5") |> 
  mutate(name_muni = fct_reorder(name_muni, p0)) |> 
  pivot_longer(cols = starts_with("p"), names_to = "index") %>%
  ggplot() +
  geom_col(aes(x=name_muni, y=value, fill = index), position = "dodge") +
  coord_flip() +
  # scale_y_continuous(limits = c(0, 0.3)) +
  facet_wrap(~index, scales = "free_x") +
  labs(title = "TMI - 30 minutos") +
  theme(legend.position = "none")

