#Here, we compare the approximate posterior distributions from the three ABC Methods

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
        xlab(TeX("log(a)")) +
        ylab("Density") +
        xlim(c(xi,xf))+
        # scale_x_log10() +
        theme(axis.title=element_text(size=18, face = "bold"))+
        theme_classic()
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
        theme_classic()
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
        theme_classic()
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
        theme_classic()
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
        xlab(TeX("log_{10}(b)")) +
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



rej_data <- readRDS("results/rejection/Benguela Pelagic/rN=1000_tol=0.6_TSS_lower_a/Benguela Pelagic.RDS")
mcmc_data <- readRDS("results/mcmc/Benguela Pelagic/mN=2e+05_tol=0.6_TSS_lower_a/Benguela Pelagic_thin.RDS")
smc_data <- readRDS("results/smc/Benguela Pelagic/sN=2500_tol=0.6_TSS_lower_a/5/Benguela Pelagic.RDS")

rej_post <- rej_data$post_dists
mcmc_post <- mcmc_data$post_dists
smc_post <- smc_data$post_dists

par  <- 'ai'

post_dist <- data.frame(par_data = rej_post[[par]], par_name = par, Method = "Rejection")
post_dist <- rbind(post_dist,
                   data.frame(par_data = mcmc_post[[par]], par_name = par, Method = "MCMC"))
post_dist <- rbind(post_dist,
                   data.frame(par_data = smc_post[[par]], par_name = par, Method = "SMC"))



N_prior <- dim(rej_post)[1]
prior_dist <- prior_dist_x(par = model_prior_par, N_prior*10000)
n_bins <- as.integer(N_prior/20)

dist_val <- rbind(post_dist,
                  data.frame(par_data = prior_dist[[par]], par_name = par, Method = "Prior"))

xi <- min(prior_dist[[par]])
xf <- max(prior_dist[[par]])

p1 <- ggplot(dist_val) +
  geom_density(mapping = aes(x = par_data, y = ..density.. , color = Method, fill = Method), alpha = 0.5) +
  theme_classic() +
  xlab(TeX("a_i")) +
  ylab("Density") +
  xlim(c(xi,xf))


par  <- 'aj'

post_dist <- data.frame(par_data = rej_post[[par]], par_name = par, Method = "Rejection")
post_dist <- rbind(post_dist,
                   data.frame(par_data = mcmc_post[[par]], par_name = par, Method = "MCMC"))
post_dist <- rbind(post_dist,
                   data.frame(par_data = smc_post[[par]], par_name = par, Method = "SMC"))



N_prior <- dim(rej_post)[1]
# prior_dist <- prior_dist(par = model_prior_par, N_prior*1000)
n_bins <- as.integer(N_prior/20)

dist_val <- rbind(post_dist,
                  data.frame(par_data = prior_dist[[par]], par_name = par, Method = "Prior"))

xi <- min(prior_dist[[par]])
xf <- max(prior_dist[[par]])

p2 <- ggplot(dist_val) +
  geom_density(mapping = aes(x = par_data, y = ..density.. , color = Method, fill = Method), alpha = 0.5) +
  theme_classic() +
  xlab(TeX("a_j")) +
  ylab("Density") +
  xlim(c(xi,xf))


par  <- 'a'

post_dist <- data.frame(par_data = rej_post[[par]], par_name = par, Method = "Rejection")
post_dist <- rbind(post_dist,
                   data.frame(par_data = mcmc_post[[par]], par_name = par, Method = "MCMC"))
post_dist <- rbind(post_dist,
                   data.frame(par_data = smc_post[[par]], par_name = par, Method = "SMC"))



N_prior <- dim(rej_post)[1]
# prior_dist <- prior_dist(par = model_prior_par, N_prior*1000)
n_bins <- as.integer(N_prior/20)

dist_val <- rbind(post_dist,
                  data.frame(par_data = prior_dist[[par]], par_name = par, Method = "Prior"))

xi <- min(prior_dist[[par]])
xf <- max(prior_dist[[par]])

p3 <- ggplot(dist_val) +
  geom_density(mapping = aes(x = par_data, y = ..density.. , color = Method, fill = Method), alpha = 0.5) +
  theme_classic() +
  xlab(TeX("log_{10}(a)")) +
  ylab("Density") +
  xlim(c(xi,xf))


par  <- 'r.b'

post_dist <- data.frame(par_data = rej_post[[par]], par_name = par, Method = "Rejection")
post_dist <- rbind(post_dist,
                   data.frame(par_data = mcmc_post[[par]], par_name = par, Method = "MCMC"))
post_dist <- rbind(post_dist,
                   data.frame(par_data = smc_post[[par]], par_name = par, Method = "SMC"))



N_prior <- dim(rej_post)[1]
# prior_dist <- prior_dist(par = model_prior_par, N_prior*1000)
n_bins <- as.integer(N_prior/20)

dist_val <- rbind(post_dist,
                  data.frame(par_data = prior_dist[[par]], par_name = par, Method = "Prior"))

xi <- min(prior_dist[[par]])
xf <- max(prior_dist[[par]])

p4 <- ggplot(dist_val) +
  geom_density(mapping = aes(x = par_data, y = ..density.. , color = Method, fill = Method), alpha = 0.5) +
  theme_classic() +
  xlab(TeX("log_{10}(b)")) +
  ylab("Density") +
  xlim(c(xi,xf)) 

fname <- paste("results/misc/","Benguela Pelagic_comp_Method.png", sep = "")
  
figure <- ggarrange(p1,p2,p3,p4, 
                    common.legend = TRUE, legend = "bottom")
# ggsave(filename = fname, width = 8, height = 8)


