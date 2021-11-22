plot_distribution <- function(xval, labell, prior_dist, n_bins, true_val_plot, real_par, parA, parB){
  if(true_val_plot == F){
    if(labell == "a") {
      xpr <- prior_dist$a; xi <- min(xpr); xf <- max(xpr); labell = "a"
      df <- data.frame(xval = xpr, Distribution = "Prior")
      df <- rbind(df, 
                  data.frame(xval = xval, Distribution = "Posterior"))
      plot_distribution <- ggplot(df) +
        # geom_histogram(bins = n_bins) +
        geom_density(mapping = aes(x = xval, y = ..density.. , color = Distribution, fill = Distribution), alpha = 0.5) +
        # geom_density(color="red", fill = "red", alpha = 0.5) +
        xlab(TeX("log_{10}(a)")) +
        ylab("Density") +
        xlim(c(xi,xf))+
        # scale_x_log10() +
        theme(axis.title=element_text(size=18, face = "bold")) +
        theme_classic() +
        labs(tag = "(c)") +
        theme(plot.tag.position = c(0.05, 1))
      # geom_vline(xintercept = parA, color = "brown") +
      # geom_vline(xintercept = parB, color = "purple")
    }
    else if (labell == "a_i") {
      xpr <- prior_dist$ai; xval = xval; xi <- min(xpr); xf <- max(xpr); labell = "a_i"
      df <- data.frame(xval = xpr, Distribution = "Prior")       
      df <- rbind(df,                    
                  data.frame(xval = xval, Distribution = "Posterior"))
      plot_distribution <- ggplot(df) +
        # geom_histogram(bins = n_bins) +
        geom_density(mapping = aes(x = xval, y = ..density.. , color = Distribution, fill = Distribution), alpha = 0.5) +
        # geom_density(color="red", fill = "red", alpha = 0.5) +
        xlab(TeX(labell)) +
        ylab("Density") +
        xlim(c(xi,xf))+
        theme(axis.title=element_text(size=18, face = "bold"))+
        theme_classic() +
        labs(tag = "(a)") +
        theme(plot.tag.position = c(0.05, 1))
      # geom_vline(xintercept = parA, color = "brown") +
      # geom_vline(xintercept = parB, color = "purple")
    }
    else if (labell == "a_j") {
      xpr <- prior_dist$aj; xval = xval; xi <- min(xpr); xf <- max(xpr); labell = "a_j"
      df <- data.frame(xval = xpr, Distribution = "Prior")       
      df <- rbind(df,                    
                  data.frame(xval = xval, Distribution = "Posterior"))
      plot_distribution <- ggplot(df) +
        # geom_histogram(bins = n_bins) +
        geom_density(mapping = aes(x = xval, y = ..density.. , color = Distribution, fill = Distribution), alpha = 0.5) +
        # geom_density(color="red", fill = "red", alpha = 0.5) +
        xlab(TeX(labell)) +
        ylab("Density") +
        xlim(c(xi,xf))+
        theme(axis.title=element_text(size=18, face = "bold")) +
        theme_classic() +
        labs(tag = "(b)") +
        theme(plot.tag.position = c(0.05, 1))
      # geom_vline(xintercept = parA, color = "brown") +
      # geom_vline(xintercept = parB, color = "purple")
    }
    else if (labell == "r.b") {
      xpr <- prior_dist$r.b; xi <- min(xpr); xf <- max(xpr); labell = "r.b"
      df <- data.frame(xval = xpr, Distribution = "Prior")       
      df <- rbind(df,                    
                  data.frame(xval = xval, Distribution = "Posterior"))
      plot_distribution <- ggplot(df) +
        # geom_histogram(bins = n_bins) +
        geom_density(mapping = aes(x = xval, y = ..density.. , color = Distribution, fill = Distribution), alpha = 0.5) +
        # geom_density(color="red", fill = "red", alpha = 0.5) +
        xlab(TeX("log_{10}(b)")) +
        ylab("Density") +
        xlim(c(xi,xf))+
        # scale_x_log10() +
        theme(axis.title=element_text(size=18, face = "bold"))+
        theme_classic() +
        labs(tag = "(d)") +
        theme(plot.tag.position = c(0.05, 1)) +
        geom_vline(xintercept = log10(2))
      # geom_vline(xintercept = parA, color = "brown") +
      # geom_vline(xintercept = parB, color = "purple")
    }
  }
  
  else{
    if(labell == "a") {
      xpr <- prior_dist$a; xi <- min(xpr); xf <- max(xpr); labell = "a"
      df <- data.frame(xval = xpr, Distribution = "Prior")       
      df <- rbind(df,                    
                  data.frame(xval = xval, Distribution = "Posterior"))
      plot_distribution <- ggplot(df) +
        # geom_histogram(bins = n_bins) +
        geom_density(mapping = aes(x = xval, y = ..density.. , color = Distribution, fill = Distribution), alpha = 0.5) +
        # geom_density(color="red", fill = "red", alpha = 0.5) +
        xlab(TeX("log(a)")) +
        ylab("Density") +
        xlim(c(xi,xf))+
        # scale_x_log10() +
        theme(axis.title=element_text(size=18, face = "bold"))+
        theme_classic() +
        geom_vline(xintercept = real_par)
      # geom_vline(xintercept = parA, color = "brown") +
      # geom_vline(xintercept = parB, color = "purple")
    }
    else if (labell == "a_i") {
      xpr <- prior_dist$ai; xval = xval; xi <- min(xpr); xf <- max(xpr); labell = "a_i"
      df <- data.frame(xval = xpr, Distribution = "Prior")       
      df <- rbind(df,                    
                  data.frame(xval = xval, Distribution = "Posterior"))
      plot_distribution <- ggplot(df) +
        # geom_histogram(bins = n_bins) +
        geom_density(mapping = aes(x = xval, y = ..density.. , color = Distribution, fill = Distribution), alpha = 0.5) +
        # geom_density(color="red", fill = "red", alpha = 0.5) +
        xlab(TeX(labell)) +
        ylab("Density") +
        xlim(c(xi,xf))+
        theme(axis.title=element_text(size=18, face = "bold"))+
        theme_classic()+
        geom_vline(xintercept = real_par)
      # geom_vline(xintercept = parA, color = "brown") +
      # geom_vline(xintercept = parB, color = "purple")
    }
    else if (labell == "a_j") {
      xpr <- prior_dist$aj; xval = xval; xi <- min(xpr); xf <- max(xpr); labell = "a_j"
      df <- data.frame(xval = xpr, Distribution = "Prior")       
      df <- rbind(df,                    
                  data.frame(xval = xval, Distribution = "Posterior"))
      plot_distribution <- ggplot(df) +
        # geom_histogram(bins = n_bins) +
        geom_density(mapping = aes(x = xval, y = ..density.. , color = Distribution, fill = Distribution), alpha = 0.5) +
        # geom_density(color="red", fill = "red", alpha = 0.5) +
        xlab(TeX(labell)) +
        ylab("Density") +
        xlim(c(xi,xf))+
        theme(axis.title=element_text(size=18, face = "bold")) +
        theme_classic()+
        geom_vline(xintercept = real_par)
      # geom_vline(xintercept = parA, color = "brown") +
      # geom_vline(xintercept = parB, color = "purple")
    }
    else if (labell == "r.b") {
      xpr <- prior_dist$r.b; xi <- min(xpr); xf <- max(xpr); labell = "r.b"
      df <- data.frame(xval = xpr, Distribution = "Prior")       
      df <- rbind(df,                    
                  data.frame(xval = xval, Distribution = "Posterior"))
      plot_distribution <- ggplot(df) +
        # geom_histogram(bins = n_bins) +
        geom_density(mapping = aes(x = xval, y = ..density.. , color = Distribution, fill = Distribution), alpha = 0.5) +
        # geom_density(color="red", fill = "red", alpha = 0.5) +
        xlab(TeX("log(b)")) +
        ylab("Density") +
        xlim(c(xi,xf))+
        # scale_x_log10() +
        theme(axis.title=element_text(size=18, face = "bold"))+
        theme_classic()+
        geom_vline(xintercept = real_par)
      # geom_vline(xintercept = parA, color = "brown") +
      # geom_vline(xintercept = parB, color = "purple")
    }
  }
  
  
  
}

plot_marg_dist <- function(predicted_foodweb, prior_dist, dirnam, desc,
                           model_prior_par, real_foodweb, true_val_plot = F,
                           parA, parB){
  
  N_prior <- dim(predicted_foodweb$post_dists[1])[1]
  prior_dist <- prior_dist(par = model_prior_par, N_prior*10000)
  n_bins = as.integer(N_prior/20)
  
  pars <- predicted_foodweb$post_dists
  p1 <- plot_distribution(pars$ai,"a_i", prior_dist, n_bins, true_val_plot, real_foodweb$ai, parA = parA$ai, parB = parB$ai)
  p2 <- plot_distribution(pars$aj, "a_j", prior_dist, n_bins, true_val_plot, real_foodweb$aj, parA = parA$aj, parB = parB$aj)
  p3 <- plot_distribution(pars$a, "a", prior_dist, n_bins, true_val_plot, real_foodweb$a, parA = parA$a, parB = parB$a)
  p4 <- plot_distribution(pars$r.b, "r.b", prior_dist, n_bins, true_val_plot, real_foodweb$r.b, parA = parA$r.b, parB = parB$r.b)
  
  name = paste(c(dirnam,"/",'distribution.png'),collapse = '')
  figure <- ggarrange(p1,p2,p3,p4, 
                      common.legend = TRUE, legend = "bottom")
  # annotate_figure(figure, top = text_grob(paste(c("marginal distribution with ss = ", desc), collapse = ''), face = "bold", size = 18))
  ggsave(filename = name,width=8,height=8)
}
