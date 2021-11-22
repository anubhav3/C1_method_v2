plot_str_prop <- function(prop, real, dirnam){
  
  s0 <- plot_prop(prop$connectance, "Connectance", real$connectance, x_min = 0, x_max = 1)
  s1 <- plot_prop(prop$prop_basal, "Proportion basal", real$prop_basal, x_min = 0, x_max = 1)
  s2 <- plot_prop(prop$prop_inter, "Proportion intermediate", real$prop_inter, x_min = 0, x_max = 1)
  s3 <- plot_prop(prop$prop_top, "Proportion top", real$prop_top, x_min = 0, x_max = 1)
  s4 <- plot_prop(prop$prop_herb, "Proportion herbivores", real$prop_herb, x_min = 0, x_max = 1)
  # s5 <- plot_prop(prop$mean_trop_lvl, "Mean trophic level", real$mean_trop_lvl)
  # s6 <- plot_prop(prop$max_trop_lvl, "Max trophic level", real$max_trop_lvl)
  s7 <- plot_prop(prop$mean_omn, "Mean omnivory", real$mean_omn, x_min = 0, x_max = max(prop$mean_omn))
  s8 <- plot_prop(prop$sd_gen, "Standard deviation of generalism", real$sd_gen, x_min = 0, x_max = max(prop$sd_gen))
  s9 <- plot_prop(prop$sd_vulner, "Standard deviation of vulnerability", real$sd_vulner, x_min = 0, x_max = max(prop$sd_vulner))
  s10 <- plot_prop(prop$diet_sim, "Diet similarity", real$diet_sim, x_min = 0, x_max = 1)
  mpl <- prop$mean_path_lt[is.finite(prop$mean_path_lt)]
  s11 <- plot_prop(mpl, "Mean path length", real$mean_path_lt, x_min = 0, x_max = max(mpl))
  s12 <- plot_prop(prop$nest_prop, "Nestedness", real$nest_prop, x_min = 0, x_max = 100)
  
  figure <- ggarrange(s0,s1,s2,s3,s4,s7,s8,s9,s10,s11,s12, nrow = 6, ncol=2,
                      common.legend = TRUE, legend="bottom")
  name <-  paste(c(dirnam,"/",'properties.png'), collapse = '')
  # annotate_figure(figure, top = paste("foodweb properties with ss = ", desc))
  annotate_figure(figure, left = text_grob("density", size = 20, family = "Times New Roman", rot = 90))
  ggsave(filename = name, width=9, height=12)
  
}