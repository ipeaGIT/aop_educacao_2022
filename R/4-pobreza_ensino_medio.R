library(dplyr)
library(data.table)
library(sf)
library(ggplot2)

# download data
aop_data <- aopdata::read_access(city = c("for", "spo", "rio", "bho", "cur", "poa", "goi", "rec", "cam"), 
                                 mode = "public_transport", year = 2019, geometry = TRUE)
aop_data <- aop_data %>% filter(P001 > 0 & peak == 1)
aop_data1 <- aop_data %>% select(id_hex, abbrev_muni, code_muni, P001, P012, P013, R003, CMAEM60)

table(aop_data$abbrev_muni)

# get distribution for access to schools
ggplot()+
  geom_boxplot(data = aop_data, aes(x = abbrev_muni, y = CMAEM60))


summary(aop_data$CMAET60)


# assuming thresholds
poverty1 <- 5
poverty2 <- 10


aop_data1 <- aop_data1 %>%
  mutate(poverty1 = ifelse(CMAEM60 < 5, TRUE, FALSE)) %>%
  mutate(poverty2 = ifelse(CMAEM60 < 10, TRUE, FALSE))


aop_data1_poverty1 <- aop_data1 %>% filter(poverty1)
aop_data1_poverty2 <- aop_data1 %>% filter(poverty2)

# copunt the proportion of the population at each trheshould
aop_summary_poverty1 <- aop_data1_poverty1 %>%
  st_set_geometry(NULL) %>%
  # onlyt the poorest
  filter(R003 %in% c(1, 2, 3, 4)) %>%
  group_by(abbrev_muni) %>%
  summarise(pop_poverty1 = sum(P012, P013))
aop_summary_poverty2 <- aop_data1_poverty2 %>%
  st_set_geometry(NULL) %>%
  # only the poorest
  filter(R003 %in% c(1, 2, 3, 4)) %>%
  group_by(abbrev_muni) %>%
  summarise(pop_poverty2 = sum(P012, P013))
  
aop_summary_poverty<- aop_data1 %>%
  st_set_geometry(NULL) %>%
  filter(R003 %in% c(1, 2, 3, 4)) %>%
  group_by(abbrev_muni) %>%
  summarise(pop_total = sum(P012, P013)) %>%
  # join poverty
  left_join(aop_summary_poverty1) %>%
  left_join(aop_summary_poverty2) %>%
  # calculate prop
  mutate(poverty1_prop = pop_poverty1 / pop_total,
         poverty2_prop = pop_poverty2 / pop_total,)


plot_each_city1 <- function(variables) {
  # variables <- "for"
  code <- aop_data1_poverty1 %>% filter(abbrev_muni == variables) %>% pull(code_muni) %>% unique(.)
  limits <- geobr::read_municipality(code)
  data <- aop_data1_poverty1 %>% filter(abbrev_muni == variables) %>%
    group_by(id_hex) %>%
    filter(R003 %in% c(1, 2, 3, 4)) %>%
    mutate(pop_jovem = sum(P012, P013))
  summary <- aop_summary_poverty %>% filter(abbrev_muni == variables)
  
  ggplot()+
    geom_sf(data = limits, fill = "grey85", alpha = 0.2) +
    geom_sf(data = data, aes(fill = pop_jovem), color = NA)+
    # scale_fill_viridis_c(direction = 1, guide = NULL)+
    scale_fill_distiller(palette = "Oranges", direction = 1, guide = NULL)+
    theme_void()+
    theme(legend.position = "bottom",
          title = element_text(size = 14))+
    labs(title = sprintf("%s: %s people (%s percent)", 
                         variables, summary$pop_poverty1, scales::percent(summary$poverty1_prop, accuracy = 0.01)))

  }

plot_each_city2 <- function(variables) {
  # variables <- "for"
  code <- aop_data1_poverty1 %>% filter(abbrev_muni == variables) %>% pull(code_muni) %>% unique(.)
  limits <- geobr::read_municipality(code)
  
  data <- aop_data1_poverty2 %>% filter(abbrev_muni == variables) %>%
    group_by(id_hex) %>%
    filter(R003 %in% c(1, 2, 3, 4)) %>%
    mutate(pop_jovem = sum(P012, P013))
  summary <- aop_summary_poverty %>% filter(abbrev_muni == variables)
  
  ggplot()+
    geom_sf(data = limits, fill = "grey85", alpha = 0.2) +
    geom_sf(data = data, aes(fill = pop_jovem), color = NA)+
    scale_fill_distiller(palette = "Oranges", direction = 1, guide = NULL)+
    theme_void()+
    theme(legend.position = "bottom",
          title = element_text(size = 14))+
    labs(title = sprintf("%s: %s people (%s percent)", 
                         variables, summary$pop_poverty2, scales::percent(summary$poverty2_prop, accuracy = 0.01)))

  }

maps_poverty1 <- lapply(c("for", "bho", "poa"), plot_each_city1)
maps_poverty2 <- lapply(c("for", "bho", "poa"), plot_each_city2)

library(patchwork)
maps_poverty1_patch <- purrr::reduce(maps_poverty1, `/`)
maps_poverty2_patch <- purrr::reduce(maps_poverty2, `/`)

# togehter
library(cowplot)
maps <- cowplot::plot_grid(maps_poverty1_patch, maps_poverty2_patch, ncol = 2,
                           labels = c("Poverty = 5", "Poverty = 10"),
                           label_size = 18)

ggsave(plot = maps, 
       filename = "figures/poverty_ensino_medio/poverty_ensino_medio.png",
      height = 16, width = 16
       )
