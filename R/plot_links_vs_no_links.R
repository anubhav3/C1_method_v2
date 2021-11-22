plot_links_vs_no_links <- function(prop.1.corr, prop.0.corr, connectance, desc, dirnam){
  
  asd <- tibble(prop1 = prop.1.corr,
                prop0 = prop.0.corr,
                connectance = connectance)
  
  ggplot(asd, aes(x = prop0, y = prop1, color = connectance)) +
    geom_point(size = 4) +
    scale_color_gradient(low = "blue", high = "red") +
    xlab("proportion of 0s correct")+
    ylab("proportion of 1s correct")+
    ggtitle(desc)+
    theme(axis.title=element_text(size=18, face = "bold")) +
    xlim(c(0,1)) +
    ylim(c(0,1))
  ggsave(filename = paste(c(dirnam,'/','1_vs_0.png'),collapse = ''),width=8,height=6)
}