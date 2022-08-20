
# Cobertura de vagas ------------------------------------------------------


# cobertura_de_vagas <- tar_read(cobertura_de_vagas)
# pop_por_decil <- tar_read(pop_por_decil)
plotar_cobertura_de_vagas <- function(cobertura_de_vagas, pop_por_decil) {

  # inverter a ordem dos decis para o plot
  pop_por_decil$renda_decil <- fct_rev(pop_por_decil$renda_decil)
  pop_por_decil$name_muni <- factor(pop_por_decil$name_muni, 
                                    levels = levels(cobertura_de_vagas$name_muni))
  
  pop_por_decil$idade <- factor(pop_por_decil$idade,
                                levels = c("0a5", "6a14", "15a18"),
                                labels = c("0 a 5 anos de idade",
                                           "6 a 14 anos de idade",
                                           "15 a 18 anos de idade"))
  
  cobertura_de_vagas$idade <- factor(cobertura_de_vagas$idade,
                                     levels = c("0a5", "6a14", "15a18"),
                                     labels = c("0 a 5 anos de idade",
                                                "6 a 14 anos de idade",
                                                "15 a 18 anos de idade"))
  
  p <- cobertura_de_vagas |> 
    ggplot(aes(x=name_muni)) +
    geom_col(data=pop_por_decil, aes(fill=renda_decil, y=proporcao)) +
    geom_errorbar(aes(ymin = 0, ymax = cobertura), width = 0.5, color = "gray20") +
    coord_flip() +
    scale_y_continuous(breaks = seq(0, 1, 0.2), labels = seq(0, 100, 20)) +
    scale_fill_viridis_d(option="E", direction = 1) +
    guides(fill = guide_legend(nrow = 1, reverse = TRUE, label.position = "bottom")) +
    labs(#subtitle = "% de matrículas em escolas em relação à população em idade escolar",
         x = NULL, 
         y = "% de crianças em cada decil de renda",
         fill = "Decil de renda") +
    theme_minimal() +
    theme(panel.border = element_rect(fill = NA, color = "grey40"),
          strip.text = element_text(face = "bold", margin = margin(0,0,0.1,0, "cm")),
          legend.position = "bottom",
          legend.key.size = unit(0.4, "cm"),
          panel.spacing = unit(0.6, "lines")) +
    facet_wrap(~nivel_ensino + idade)

  # save plot
  figura <- here::here("output", "fig_01_cobertura_de_vagas.png")
  
  ggsave(plot = p, filename = figura, width = 16, height = 13, units = "cm", dpi = 300)
  
  return(figura)
}


# Ensino Infantil ---------------------------------------------------------


# insuficiencia_ens_infantil_por_cidade <- tar_read(insuficiencia_ens_infantil_por_cidade)
plotar_insuficiencia_ens_infantil <- function(insuficiencia_ens_infantil_por_cidade) {
  
  muni_order <- insuficiencia_ens_infantil_por_cidade %>%
    arrange(li_15_pc) |> 
    pull(name_muni)
  
  summary_df <- insuficiencia_ens_infantil_por_cidade |> 
    mutate(name_muni = factor(name_muni, levels = muni_order))

  p <- summary_df |> 
    ggplot(aes(y=name_muni)) +
    geom_dumbbell(aes(x = li_15_pc, xend = li_30_pc), 
                         size=3, color="gray80", alpha=.8, colour_x = "steelblue4", colour_xend = "springgreen4")+
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels=paste0(seq(0, 100, 10), "%")) +
    geom_text(data = filter(summary_df, abbrev_muni == "sgo"),
              aes(x = li_30_pc, y = name_muni),
              label = "30 minutos", fontface = "bold",
              color = "springgreen4",
              vjust = 1.5) +
    geom_text(data = filter(summary_df, abbrev_muni == "sgo"),
              aes(x = li_15_pc, y = name_muni),
              label = "15 minutos", fontface = "bold",
              color = "steelblue4",
              vjust = 1.5) +
    labs(#title = "% de crianças de 0 a 5 anos de idade a mais de 15 e 30 min de\ncaminhada de uma escola de educação infantil",
         # caption = "(50% das famílias mais pobres em cada cidade)",
         y=NULL,
         x="Proporção de crianças de baixa renda com acessibilidade insuficiente", # "% de crianças atendidas",
         color = NULL) +
    theme_minimal() +
    theme(panel.border = element_rect(fill = NA, color = "grey40"),
          strip.text = element_text(face = "bold"),
          legend.position = "bottom")
  
  # save plot
  figura <- here::here("output", "fig_02_insuficiencia_ens_infantil.png")
  
  ggsave(plot = p, filename = figura, 
         width = 16, height = 13, units = "cm", dpi = 300, scale=1.1)

  return(figura)
  
}



# Ensino Médio ------------------------------------------------------------

# insuficiencia_ens_medio_por_cidade <- tar_read(insuficiencia_ens_medio_por_cidade)
plotar_insuficiencia_ens_medio <- function(insuficiencia_ens_medio_por_cidade) {
  
  muni_order <- insuficiencia_ens_medio_por_cidade %>%
    arrange(li_3_pc) |> 
    pull(name_muni)
  
  summary_df <- insuficiencia_ens_medio_por_cidade |> 
    mutate(name_muni = factor(name_muni, levels = muni_order))
  
  p <- summary_df |> 
    ggplot(aes(y=name_muni)) +
    geom_dumbbell(aes(x = li_0_pc, xend = li_3_pc), 
                         size=3, color="gray80", alpha=.8, colour_x = "steelblue4", colour_xend = "springgreen4") +
    geom_point(aes(x=li_1_pc), color = "black", size = 3) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels=paste0(seq(0, 100, 10), "%")) +
    geom_text(data = filter(summary_df, abbrev_muni == "goi"),
              aes(x = li_3_pc, y = name_muni),
              label = "3 escolas", fontface = "bold",
              color = "springgreen4",
              vjust = 1.5) +
    geom_text(data = filter(summary_df, abbrev_muni == "goi"),
              aes(x = li_1_pc, y = name_muni),
              label = "1 escola", fontface = "bold",
              color = "black",
              vjust = 1.5) +
    geom_text(data = filter(summary_df, abbrev_muni == "goi"),
              aes(x = li_0_pc, y = name_muni),
              label = "0 escolas", fontface = "bold",
              color = "steelblue4",
              vjust = 1.5) +
    labs(y=NULL,
         x="Proporção de jovens de baixa renda com acessibilidade insuficiente", # "% de crianças atendidas",
         color = NULL) +
    theme_minimal() +
    theme(panel.border = element_rect(fill = NA, color = "grey40"),
          strip.text = element_text(face = "bold"),
          legend.position = "bottom")
  
  # save plot
  figura <- here::here("output", "fig_04_insuficiencia_ens_medio.png")
  
  ggsave(plot = p, filename = figura, 
         width = 16, height = 7, units = "cm", dpi = 300, scale=1.1)
  
  return(figura)
  
}
