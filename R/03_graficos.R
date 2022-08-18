# cobertura_de_vagas <- tar_read(cobertura_de_vagas)
# pop_por_decil <- tar_read(pop_por_decil)
plotar_cobertura_de_vagas <- function(cobertura_de_vagas, pop_por_decil) {
  
  p <- cobertura_de_vagas |> 
    ggplot(aes(x=name_muni)) +
    geom_errorbar(aes(ymin = 0, ymax = cobertura), width = 0.5) +
    coord_flip() +
    scale_y_continuous(breaks = seq(0, 1, 0.2), labels = seq(0, 100, 20)) +
    labs(x = NULL, y = "% de estudantes em cada decil de renda",
         fill = "Decil de renda",
         subtitle = "% de vagas em escolas em relação à população em idade escolar") +
    facet_wrap(~nivel_ensino)
    
    
  # save plot ---------------------------------------------------------------
  figura <- here::here("figuras", "fig_01_cobertura_de_vagas.png")
  
  ggsave(plot = p, filename = figura, width = 16, height = 12, units = "cm", dpi = 300)
  
  return(figura)
  
  
    # 
    # ggplot(aes(x=name_muni, y=proporcao_decil)) +
    # geom_col(aes(fill=renda_decil)) +
    # geom_errorbar(data = vagas_tidy, aes(ymin=0, ymax=proporcao_atendidos, y=NULL), width = 0.5) +
    # coord_flip() +
    # scale_fill_viridis_d(option="E", direction = 1) +
    # scale_y_continuous(breaks = seq(0, 1, 0.2), labels = seq(0, 100, 20)) +
    # labs(x = NULL, y = "% de estudantes em cada decil de renda",
    #      fill = "Decil de renda",
    #      subtitle = "% de vagas em escolas em relação à população em idade escolar") +
    # theme(legend.position = "bottom",
    #       legend.key.size = unit(0.4, "cm")) +
    # guides(fill = guide_legend(nrow = 1, reverse = TRUE)) +
    # facet_wrap(~nivel_ensino, nrow = 1)
    # 
  
  
  # 
  # estudantes_tidy %>% 
  #   mutate(name_muni = factor(name_muni, levels = cidades_factor$name_muni)) %>%
  #   ggplot(aes(x=name_muni, y=proporcao_decil)) +
  #   geom_col(aes(fill=renda_decil)) +
  #   geom_errorbar(data = vagas_tidy, aes(ymin=0, ymax=proporcao_atendidos, y=NULL), width = 0.5) +
  #   coord_flip() +
  #   scale_fill_viridis_d(option="E", direction = 1) +
  #   scale_y_continuous(breaks = seq(0, 1, 0.2), labels = seq(0, 100, 20)) +
  #   labs(x = NULL, y = "% de estudantes em cada decil de renda",
  #        fill = "Decil de renda",
  #        subtitle = "% de vagas em escolas em relação à população em idade escolar") +
  #   theme(legend.position = "bottom",
  #         legend.key.size = unit(0.4, "cm")) +
  #   guides(fill = guide_legend(nrow = 1, reverse = TRUE)) +
  #   facet_wrap(~nivel_ensino, nrow = 1)
  
}