plot_dist_prop <- function(connectance, prop.1.corr, prop.0.corr,
                           dist, ACC, TSS, dirnam, real_connectance){
  
  p0 <- plot_prop(connectance, "Connectance", real_connectance, x_min = 0, x_max = 1)
  p1 <- plot_prop(prop.1.corr, "proportion of 1s correct", real = 1, x_min = 0, x_max = 1)
  p2 <- plot_prop(prop.0.corr, "proportion of 0s correct", real = 1, x_min = 0, x_max = 1)
  p3 <- plot_prop(dist, "distance", real = 0, x_min = 0, x_max = 1)
  p4 <- plot_prop(ACC, "accuracy ACC",real = 1, x_min = 0, x_max = 1)
  p5 <- plot_prop(TSS, "true skill statistic TSS",real = 1, x_min = -1, x_max = 1)
  figure1 <- ggarrange(p0,p1,p2,p3,p4,p5, nrow = 6, ncol=1,
                       common.legend = TRUE, legend="bottom")
  name <-  paste(c(dirnam,"/",'distribution of connectance 1 0.png'), collapse = '')
  annotate_figure(figure1, top = paste("distribution of connectance 1 0 with ss = ", desc))
  ggsave(filename = name, width=6, height=11)
  
}