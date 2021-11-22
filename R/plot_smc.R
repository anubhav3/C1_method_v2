# plots the posterior distribution of parameters in SMC method

plot_smc <- function(pred_data, dirnam){
  
  par_dict <- c("a", "ai", "aj", "r.b")
  par_index <- 1
  for(par in par_dict){
    print(par)
    if(par == "a"){
      xlow <- model_prior_par$l_log_a
      xhigh <- model_prior_par$r_log_a
    }
    else if(par == "ai"){
      xlow <- model_prior_par$l_ai
      xhigh <- model_prior_par$r_ai
    }
    else if(par == "aj"){
      xlow <- model_prior_par$l_aj
      xhigh <- model_prior_par$r_aj
    }
    else if(par == "r.b"){
      xlow <- model_prior_par$l_log_r.b
      xhigh <- model_prior_par$r_log_r.b
    }
    pbly <- 1/(xhigh-xlow)
    n_run <- dim(pred_data$w)[2]
    
    tidy_posts <- tibble(par_val = pred_data$post_dists[,par_index,1],
                         weight = pred_data$w[,1]/sum(pred_data$w[,1]),
                         run = 1,
                         par_name = par)
    for(i in 2:n_run){
      tidy_posts <- bind_rows(tidy_posts,
                              tibble(par_val = pred_data$post_dists[,par_index,i],
                                     weight = pred_data$w[,i]/sum(pred_data$w[,i]),
                                     run = i,
                                     par_name = par))
    }
    
    fname <- paste(c(dirnam, "/", par, ".png"), collapse = "")
    col_pal <- c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6")
    plt <- ggplot(tidy_posts) +
      geom_density(aes(x = par_val, y = ..density.., weight = weight,
                         fill = factor(run)),
                   alpha = 0.25) +
      # geom_histogram(aes(x = par_val, y = ..density.., weight = weight,
      #                  fill = factor(run)), bins = 50, position = "identity",
      #              alpha = 0.25) +
      # facet_wrap(~run, ncol = 1) +
      theme_bw()+
      theme(axis.title = element_text(size=18, face = "bold"),
            axis.text = element_text(size=18, face = "bold"),
            legend.title = element_text(size=18, face = "bold"),
            legend.text = element_text(size=18, face = "bold")) +
      xlab(par) +
      ylab("density") +
      # scale_fill_discrete(name = "distance",
      #                     labels = epsilon_t()) +
      geom_segment(aes(x = xlow, y = 0, xend = xlow, yend = pbly), color = "red", size = 2) +
      geom_segment(aes(x = xhigh, y = 0, xend = xhigh, yend = pbly), color = "red", size = 2) +
      geom_segment(aes(x = xlow, y = pbly, xend = xhigh, yend = pbly), color = "red", size = 2) +
      scale_fill_manual(values = col_pal, aesthetics = "fill",
                        name = "distance",
                        labels = epsilon_t())
      
    
    ggsave(filename = fname, plot = plt, dpi = 200, width = 60, height = 60, units = c("cm"))
    
    par_index <- par_index + 1 
  }
  
}




