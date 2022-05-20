library(dplyr)
library(data.table)
library(sf)
library(ggplot2)
library(aopdata)

# download data
aop_data <- aopdata::read_access(city = c("for", "spo", "rio", "bho", "cur", "poa", "goi", "rec", "cam"), 
                                 mode = "public_transport", year = 2019, geometry = TRUE)
aop_data <- aop_data %>% filter(P001 > 0)
aop_data1 <- aop_data %>% select(id_hex, abbrev_muni, name_muni, code_muni, P001, P012, P013, R003, E004, CMAEM30, CMAEM60, CMAMT30)

table(aop_data$abbrev_muni)

# get distribution for access to schools
boxplot_acess_cities <- ggplot()+
  geom_boxplot(data = aop_data, aes(x = abbrev_muni, y = CMAEM30, weight = P001))
# save
ggsave(plot = boxplot_acess_cities,
       filename = "figures/poverty_ensino_medio/boxplot_acess_cities.png",
       width = 16, height = 10, units = "cm")

# grafico com percentis p/ todas as cidades
quintiles <- Hmisc::wtd.quantile(aop_data1$CMAEM30, weights=aop_data1$P001, 
                                 probs=c( seq(0 , 1 , 0.05) ), 
                                 type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'), 
                                 normwt=FALSE, na.rm=T) %>% as.data.frame()
quintiles <- Hmisc::wtd.quantile(aop_data1$CMAEM60, weights=aop_data1$P001, 
                                 probs=c( seq(0 , 1 , 0.05) ), 
                                 type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'), 
                                 normwt=FALSE, na.rm=T) %>% as.data.frame()
quintiles <- Hmisc::wtd.quantile(aop_data1$CMAEM60, weights=aop_data1$P001, 
                                 probs=c( seq(0 , 1 , 0.05) ), 
                                 type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'), 
                                 normwt=FALSE, na.rm=T) %>% as.data.frame()

quintiles$percentil <- rownames(quintiles)

ggplot()+
  geom_col(data = quintiles, aes(x = percentil, y = .))

summary(aop_data$CMAET60)


# assuming thresholds
# para CMAEM30: P25 = 2; P50 = 5
poverty1_line <- 0
poverty2_line <- 2
poverty3_line <- 5


aop_data1 <- aop_data1 %>%
  mutate(poverty1 = ifelse(CMAEM60 <= poverty1_line, TRUE, FALSE)) %>%
  mutate(poverty2 = ifelse(CMAEM60 <= poverty2_line, TRUE, FALSE)) %>%
  mutate(poverty3 = ifelse(CMAEM60 <= poverty3_line, TRUE, FALSE))

table(aop_data1$poverty1)
table(aop_data1$poverty2)
table(aop_data1$poverty3)

aop_data1_poverty1 <- aop_data1 %>% filter(poverty1)
aop_data1_poverty2 <- aop_data1 %>% filter(poverty2)
aop_data1_poverty3 <- aop_data1 %>% filter(poverty3)

# copunt the proportion of the population at each trheshould
aop_summary_poverty1 <- aop_data1_poverty1 %>%
  st_set_geometry(NULL) %>%
  # onlyt the poorest
  filter(R003 %in% c(1, 2, 3, 4)) %>%
  group_by(abbrev_muni,name_muni) %>%
  summarise(pop_poverty1 = sum(P012))
aop_summary_poverty2 <- aop_data1_poverty2 %>%
  st_set_geometry(NULL) %>%
  # only the poorest
  filter(R003 %in% c(1, 2, 3, 4)) %>%
  group_by(abbrev_muni, name_muni) %>%
  summarise(pop_poverty2 = sum(P012))
aop_summary_poverty3 <- aop_data1_poverty3 %>%
  st_set_geometry(NULL) %>%
  # only the poorest
  filter(R003 %in% c(1, 2, 3, 4)) %>%
  group_by(abbrev_muni,name_muni) %>%
  summarise(pop_poverty3 = sum(P012))

aop_summary_poverty1_income <- aop_data1_poverty1 %>%
  st_set_geometry(NULL) %>%
  # onlyt the poorest
  filter(R003 %in% c(1, 2, 3, 4)) %>%
  group_by(abbrev_muni,name_muni,  R003) %>%
  summarise(pop_poverty1 = sum(P012))
aop_summary_poverty2_income <- aop_data1_poverty2 %>%
  st_set_geometry(NULL) %>%
  # only the poorest
  filter(R003 %in% c(1, 2, 3, 4)) %>%
  group_by(abbrev_muni, name_muni, R003) %>%
  summarise(pop_poverty2 = sum(P012))
aop_summary_poverty3_income <- aop_data1_poverty3 %>%
  st_set_geometry(NULL) %>%
  # only the poorest
  filter(R003 %in% c(1, 2, 3, 4)) %>%
  group_by(abbrev_muni,name_muni, R003) %>%
  summarise(pop_poverty3 = sum(P012))

# dadao total
aop_summary_poverty<- aop_data1 %>%
  st_set_geometry(NULL) %>%
  filter(R003 %in% c(1, 2, 3, 4)) %>%
  group_by(abbrev_muni,name_muni) %>%
  summarise(pop_total = sum(P012)) %>%
  # join poverty
  left_join(aop_summary_poverty1) %>%
  left_join(aop_summary_poverty2) %>%
  left_join(aop_summary_poverty3) %>%
  # calculate prop
  mutate(poverty1_prop = 1 - (pop_poverty1 / pop_total),
         poverty2_prop = 1 - (pop_poverty2 / pop_total),
         poverty3_prop = 1 - (pop_poverty3 / pop_total))

aop_summary_poverty_income <- aop_data1 %>%
  st_set_geometry(NULL) %>%
  filter(R003 %in% c(1, 2, 3, 4)) %>%
  group_by(abbrev_muni, name_muni, R003) %>%
  summarise(pop_total = sum(P012)) %>%
  ungroup() %>%
  # join poverty
  left_join(aop_summary_poverty1_income) %>%
  left_join(aop_summary_poverty2_income) %>%
  left_join(aop_summary_poverty3_income) %>%
  # calculate prop
  mutate(poverty1_prop = 1 - (pop_poverty1 / pop_total),
         poverty2_prop = 1 - (pop_poverty2 / pop_total),
         poverty3_prop = 1 - (pop_poverty3 / pop_total))

# graph
# data to long format
aop_summary_poverty_long <- aop_summary_poverty_income %>%
  select(name_muni, R003, ends_with("prop")) %>%
  tidyr::pivot_longer(cols = poverty1_prop:poverty3_prop, names_to = "poverty_type", values_to = "value")

pobreza_grafico <- ggplot(data = aop_summary_poverty_long)+
  geom_line(aes(x = R003, y = value, color = poverty_type))+
  geom_point(aes(x = R003, y = value, color = poverty_type))+
  facet_wrap(~name_muni)+
  scale_color_brewer(palette = "Set1")+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)+
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA, color = "grey40"),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom")+
  labs(color = "Nível de pobreza")

# salvar
ggsave(plot = pobreza_grafico, 
       filename = "figures/poverty_ensino_medio/poverty_graph_ensino_medio.png",
       height = 13, width = 16, units = "cm"
)



# map for selected cities - good and bad cases ----------------------------


plot_each_city <- function(variables, poverty = "poverty1") {
  # variables <- "for"
  # variables <- c("rio")
  # variables <- "poa"
  # poverty <- "poverty1"
  
  # select dataset
  data <- get(sprintf("aop_data1_%s", poverty))
  
  code <- data %>% filter(abbrev_muni %in% variables) %>% pull(code_muni) %>% unique(.)
  limits <- geobr::read_municipality(code)
  data <- data %>% filter(abbrev_muni %in% variables) %>%
    filter(R003 %in% c(1, 2, 3, 4)) %>%
    group_by(name_muni, id_hex) %>%
    mutate(pop_jovem = sum(P012))
  summary <- aop_summary_poverty %>% filter(abbrev_muni == variables) 
  
  # get max value for limits
  # max_value <- max(data$P012)
  
  # escolas
  escolas <- aop_data1 %>%
    filter(abbrev_muni == variables) %>%
    filter(E004 > 0)
  
  # mapview::mapview(data, col.regions = "red") + escolas
  
  # extract vars
  a <- sprintf("pop_%s", poverty)
  b <- sprintf("%s_prop", poverty)
  summary_pop <- summary[[a]]
  summary_pop_prob <- summary[[b]]
  
  ggplot()+
    geom_sf(data = limits, fill = "grey85", alpha = 0.2) +
    geom_sf(data = data, aes(fill = pop_jovem), color = NA)+
    geom_sf(data = escolas, color = "black")+
    # scale_fill_viridis_c(direction = 1, guide = NULL)+
    scale_fill_distiller(palette = "Oranges", direction = 1, limits = c(0, 350))+
    # scale_fill_viridis_c(option = "inferno", direction = 1, limits = c(0, 350))+
    theme_void()+
    theme(legend.position = "bottom",
          title = element_text(size = 14),
          legend.text = element_text(size = 15))+
    labs(title = sprintf("%s: %s people (%s)", 
                         variables, summary_pop, scales::percent(summary_pop_prob, accuracy = 0.01)),
         fill = "Pop jovem")
  
}


maps_poverty1 <- lapply(c("spo", "rio"), plot_each_city, "poverty1")
maps_poverty2 <- lapply(c("spo", "rio"), plot_each_city, "poverty2")
maps_poverty3 <- lapply(c("spo", "rio"), plot_each_city, "poverty3")

library(patchwork)
maps_poverty1_patch <- maps_poverty1[[1]] +  maps_poverty1[[2]] + plot_annotation(subtitle = "P1 (0 escolas)") + plot_layout(guides = 'collect') & theme(legend.position = "bottom") 
maps_poverty2_patch <- maps_poverty2[[1]] +  maps_poverty2[[2]]  + plot_annotation(subtitle = "P1 (2 escolas)") + plot_layout(guides = 'collect') & theme(legend.position = "bottom")
maps_poverty3_patch <- maps_poverty3[[1]] +  maps_poverty3[[2]]  + plot_annotation(subtitle = "P1 (5 escolas)") + plot_layout(guides = 'collect') & theme(legend.position = "bottom")
# maps_poverty1_patch <- purrr::reduce(maps_poverty1, `+`)
# maps_poverty2_patch <- purrr::reduce(maps_poverty2, `+`)
# maps_poverty3_patch <- purrr::reduce(maps_poverty3, `+`)

# togehter
library(cowplot)
maps <- cowplot::plot_grid(maps_poverty1_patch, maps_poverty2_patch, maps_poverty3_patch, nrow = 3
                           # labels = c("Sâo Paulo", "Rio"),
                           # label_size = 20
                           )
# maps <- cowplot::plot_grid(maps_poverty1_patch, maps_poverty2_patch, ncol = 2,
#                            labels = c("P=2", "P=5"),
#                            label_size = 20)

ggsave(plot = maps, 
       filename = "figures/poverty_ensino_medio/poverty_ensino_medio_spo-rio.png",
       height = 22, width = 18
)
