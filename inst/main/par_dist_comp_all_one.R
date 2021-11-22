# 21.11.2021
# Used to compare predicted parameter values of posterior distribution from different ABC methods

library(HDInterval)
library(ggplot2)
library(readxl)
library(psych); library(DirectedClustering); library(colorspace)
library(stringr)

## Relative path from the project directory
sourceDirectory("R", modifiedOnly=FALSE)

par_data <- read_excel("data/parameter_values.xlsx")
ind <- 1
plot_par <- list()
desc <- "TSS_lower_a"
desc_mcmc <-  "TSS_lower_a"


fw_title <- c("Benguela Pelagic", "Broadstone Stream \n (taxonomic aggregation)",
              "Broom", "Capinteria", "Caricaie Lakes",
              "Grasslands", "Mill Stream",
              "Skipwith Pond", "Small Reef", "Tuesday Lake",
              "Ythan", "Broadstone Stream \n (size aggregation)")

# for figure numbering
fig_ind <- c("(a)", "(b)", "(c)", "(d)", 
             "(e)", "(f)", "(g)", "(h)", 
             "(i)", "(j)", "(k)", "(l)")

for(foodweb in par_data$foodweb){
  
  if(is.na(par_data$N_s[ind]) == FALSE){
    foodweb_data <- readRDS(paste("data/", foodweb, ".web.RDS", sep=""))
    
    dir_rej <- paste("rN=1000_tol=", par_data$dist_rej[ind] ,"_", desc, sep = "")
    dir_mcmc <- paste("mN=",par_data$N_m[ind],"_tol=", par_data$dist_rej[ind] ,"_", desc_mcmc, sep = "")
    dir_smc <- paste("sN=",par_data$N_s[ind],"_tol=", par_data$dist_smc[ind] ,"_", desc,"/5", sep = "")
    rej_data <- readRDS(paste("results/rejection/", foodweb, "/", dir_rej, "/", foodweb, ".RDS", sep = ""))
    mcmc_data <- readRDS(paste("results/mcmc/", foodweb, "/", dir_mcmc, "/", foodweb, "_thin.RDS", sep = ""))
    smc_data <- readRDS(paste("results/smc/", foodweb, "/", dir_smc, "/", foodweb, ".RDS", sep = ""))
    pnas_data <- data.frame(a = log10(par_data$a[ind]), ai = par_data$ai[ind], 
                            aj = par_data$aj[ind], r.b = par_data$r.b[ind])
    
    rej_post <- rej_data$post_dists
    mcmc_post <- mcmc_data$post_dists
    smc_post <- smc_data$post_dists
    
    rej_par <- hdi(rej_post)
    mcmc_par <- hdi(mcmc_post)
    smc_par <- hdi(smc_post)
    pp <- ADBM_prior_par(foodweb_data)
    prior_par <- array(dim = c(2,4))
    prior_par[1,1] <- pp$l_log_a
    prior_par[2,1] <- pp$r_log_a
    prior_par[1,2] <- pp$l_ai
    prior_par[2,2] <- pp$r_ai
    prior_par[1,3] <- pp$l_aj
    prior_par[2,3] <- pp$r_aj
    prior_par[1,4] <- pp$l_log_r.b
    prior_par[2,4] <- pp$r_log_r.b
    
    
    i <- 2
    j <- 5
    
    #Prior parameters
    df <- data.frame(x0 = as.double(prior_par[1,]), x1 = as.double(prior_par[2,]), y0 = c(i-1,i+j-1,i+2*j-1, i+3*j-1), y1 = c(i-1,i+j-1,i+2*j-1, i+3*j-1), Estimates = "Prior", par = names(rej_par[1,]))
    
    # rejection
    df <- rbind(df, data.frame(x0 = as.double(rej_par[1,]), x1 = as.double(rej_par[2,]), y0 = c(i,i+j,i+2*j, i+3*j), y1 = c(i,i+j,i+2*j, i+3*j), Estimates = "Rejection", par = names(rej_par[1,])))
    #MCMC
    df <- rbind(df, data.frame(x0 = as.double(mcmc_par[1,]), x1 = as.double(mcmc_par[2,]), y0 = c(i+1,i+j+1,i+2*j+1, i+3*j+1), y1 = c(i+1,i+j+1,i+2*j+1, i+3*j+1), Estimates = "MCMC", par = names(rej_par[1,])))
    #SMC
    df <- rbind(df, data.frame(x0 = as.double(smc_par[1,]), x1 = as.double(smc_par[2,]), y0 = c(i+2,i+j+2,i+2*j+2, i+3*j+2), y1 = c(i+2,i+j+2,i+2*j+2, i+3*j+2), Estimates = "SMC", par = names(rej_par[1,])))
    
    #PNAS
    df <- rbind(df, data.frame(x0 = as.double(pnas_data), x1 = as.double(pnas_data), y0 = c(i+3,i+j+3,i+2*j+3, i+3*j+3), y1 = c(i+3,i+j+3,i+2*j+3, i+3*j+3), Estimates = "PNAS 2008", par = names(rej_par[1,])))
    
    df1 <- data.frame(x0 = as.double(pnas_data), x1 = as.double(pnas_data), y0 = c(i+0.5, i+j+0.5, i+2*j+0.5, i+3*j+0.5), y1 = c(i+0.5, i+j+0.5, i+2*j+0.5, i+3*j+0.5), Estimates = "PNAS 2008", par = names(rej_par[1,]))
    
    fname <- paste("results/comparison/", foodweb, "_par.png", sep = "")
    # png(filename = fname, height = 1000, width = 1200, pointsize = 30)
    cols = c("Prior" = "black", "Rejection" = "red", "MCMC" = "blue", "SMC" = "brown","PNAS 2008" = "green")
    labes = c("Prior" = "Prior distribution", "Rejection" = "Rejection ABC", "MCMC" = "MCMC ABC", "SMC" = "SMC ABC", "PNAS 2008" = "Point estimates from Petchey et al. (2008)")
    
    main_title <- foodweb
    
    fw_title_temp <- fw_title[ind]
    plot_par[[ind]] <- ggplot() +
      geom_segment(data = df, aes(x = x0, xend = x1, y = y0, yend = y1, color = Estimates), size = 1.5) +
      geom_point(data = df1, aes(x = x0, y = y0, color = Estimates), size = 2.5) +
      theme_classic() + 
      # annotate("text", x = -1, y = 1.5, label = 'log[10](a)', parse = TRUE, size = 10,family="Comic Sans MS") +
      # annotate("text", x = 0.75, y = 4.5, label = 'a[i]', parse = TRUE, size = 10,family="Comic Sans MS") +
      # annotate("text", x = -1, y = 7.5, label = 'a[j]', parse = TRUE, size = 10,family="Comic Sans MS") +
      # annotate("text", x = 0, y = 10.5, label = 'log[10](b)', parse = TRUE, size = 10,family="Comic Sans MS") +
      xlab("Values") +
      ylab("Parameters") +
      theme(text = element_text(size=20, family="Times New Roman"), axis.line = element_line(colour = 'black', size = 2),
            plot.margin = margin(2, 2, 2, 2, "cm")) +
      scale_color_manual(values = cols, labels = str_wrap(labes, 20))+
      # labs(title = str_wrap(main_title, 35))+
      labs(title = fw_title_temp, tag = fig_ind[ind]) +
      theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), plot.tag = element_text(face = "bold")) +
      # theme(axis.text.y = element_blank()) +
      scale_y_continuous(breaks = c(i+0.5, i+j+0.5, i+2*j+0.5, i+3*j+0.5),
                         labels = c(TeX('$log_{10}a$'), TeX('$a_i$'), TeX('$a_j$'), TeX('$log_{10}b$'))) +
      # theme(legend.position = "none") +
      theme(plot.margin = unit(c(1,1,1,1), "cm"))
    
    # ggsave(filename = fname, plot = plot_par, height = 10, width = 10)
  }
  
  else{
    foodweb_data <- readRDS(paste("data/", foodweb, ".web.RDS", sep=""))
    
    dir_rej <- paste("rN=1000_tol=", par_data$dist_rej[ind] ,"_", desc, sep = "")
    dir_mcmc <- paste("mN=",par_data$N_m[ind],"_tol=", par_data$dist_rej[ind] ,"_", desc_mcmc, sep = "")
    # dir_smc <- paste("sN=",par_data$N_s[ind],"_tol=", par_data$dist_s[ind] ,"_", desc,"/5", sep = "")
    rej_data <- readRDS(paste("results/rejection/", foodweb, "/", dir_rej, "/", foodweb, ".RDS", sep = ""))
    mcmc_data <- readRDS(paste("results/mcmc/", foodweb, "/", dir_mcmc, "/", foodweb, "_thin.RDS", sep = ""))
    # smc_data <- readRDS(paste("results/smc/", foodweb, "/", dir_smc, "/", foodweb, ".RDS", sep = ""))
    pnas_data <- data.frame(a = log10(par_data$a[ind]), ai = par_data$ai[ind], 
                            aj = par_data$aj[ind], r.b = par_data$r.b[ind])
    
    rej_post <- rej_data$post_dists
    mcmc_post <- mcmc_data$post_dists
    # smc_post <- smc_data$post_dists
    
    rej_par <- hdi(rej_post)
    mcmc_par <- hdi(mcmc_post)
    # smc_par <- hdi(smc_post)
    pp <- ADBM_prior_par(foodweb_data)
    prior_par <- array(dim = c(2,4))
    prior_par[1,1] <- pp$l_log_a
    prior_par[2,1] <- pp$r_log_a
    prior_par[1,2] <- pp$l_ai
    prior_par[2,2] <- pp$r_ai
    prior_par[1,3] <- pp$l_aj
    prior_par[2,3] <- pp$r_aj
    prior_par[1,4] <- pp$l_log_r.b
    prior_par[2,4] <- pp$r_log_r.b
    
    
    i <- 2
    j <- 5
    
    #Prior parameters
    df <- data.frame(x0 = as.double(prior_par[1,]), x1 = as.double(prior_par[2,]), y0 = c(i-1,i+j-1,i+2*j-1, i+3*j-1), y1 = c(i-1,i+j-1,i+2*j-1, i+3*j-1), Estimates = "Prior", par = names(rej_par[1,]))
    
    # rejection
    df <- rbind(df, data.frame(x0 = as.double(rej_par[1,]), x1 = as.double(rej_par[2,]), y0 = c(i,i+j,i+2*j, i+3*j), y1 = c(i,i+j,i+2*j, i+3*j), Estimates = "Rejection", par = names(rej_par[1,])))
    #MCMC
    df <- rbind(df, data.frame(x0 = as.double(mcmc_par[1,]), x1 = as.double(mcmc_par[2,]), y0 = c(i+1,i+j+1,i+2*j+1, i+3*j+1), y1 = c(i+1,i+j+1,i+2*j+1, i+3*j+1), Estimates = "MCMC", par = names(rej_par[1,])))
    # #SMC
    # df <- rbind(df, data.frame(x0 = as.double(smc_par[1,]), x1 = as.double(smc_par[2,]), y0 = c(i+2,i+j+2,i+2*j+2, i+3*j+2), y1 = c(i+2,i+j+2,i+2*j+2, i+3*j+2), Estimates = "SMC", par = names(rej_par[1,])))
    
    #PNAS
    df <- rbind(df, data.frame(x0 = as.double(pnas_data), x1 = as.double(pnas_data), y0 = c(i+3,i+j+3,i+2*j+3, i+3*j+3), y1 = c(i+3,i+j+3,i+2*j+3, i+3*j+3), Estimates = "PNAS 2008", par = names(rej_par[1,])))
    
    df1 <- data.frame(x0 = as.double(pnas_data), x1 = as.double(pnas_data), y0 = c(i+0.5, i+j+0.5, i+2*j+0.5, i+3*j+0.5), y1 = c(i+0.5, i+j+0.5, i+2*j+0.5, i+3*j+0.5), Estimates = "PNAS 2008", par = names(rej_par[1,]))
    
    fname <- paste("results/comparison/", foodweb, "_par.png", sep = "")
    # png(filename = fname, height = 1000, width = 1200, pointsize = 30)
    cols = c("Prior" = "black", "Rejection" = "red", "MCMC" = "blue","PNAS 2008" = "green")
    labes = c("Prior" = "Prior distribution", "Rejection" = "Rejection ABC", "MCMC" = "MCMC ABC", "PNAS 2008" = "Point estimates from Petchey et al (2008)")
    
    main_title <- foodweb
    fw_title_temp <- fw_title[ind]
    plot_par[[ind]] <- ggplot() +
      geom_segment(data = df, aes(x = x0, xend = x1, y = y0, yend = y1, color = Estimates), size = 1.5) +
      geom_point(data = df1, aes(x = x0, y = y0, color = Estimates), size = 2.5) +
      theme_classic() + 
      # annotate("text", x = -1, y = 1.5, label = 'log[10](a)', parse = TRUE, size = 10,family="Comic Sans MS") +
      # annotate("text", x = 0.75, y = 4.5, label = 'a[i]', parse = TRUE, size = 10,family="Comic Sans MS") +
      # annotate("text", x = -1, y = 7.5, label = 'a[j]', parse = TRUE, size = 10,family="Comic Sans MS") +
      # annotate("text", x = 0, y = 10.5, label = 'log[10](b)', parse = TRUE, size = 10,family="Comic Sans MS") +
      xlab("Values") +
      ylab("Parameters") +
      theme(text = element_text(size=20, family="Times New Roman"), axis.line = element_line(colour = 'black', size = 2),
            plot.margin = margin(2, 2, 2, 2, "cm")) +
      scale_color_manual(values = cols, labels = str_wrap(labes, 20))+
      # labs(title = str_wrap(main_title, 35)) +
      labs(title = fw_title_temp, tag = fig_ind[ind]) +
      theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), plot.tag = element_text(face = "bold")) +
      # theme(axis.text.y = element_blank()) +
      scale_y_continuous(breaks = c(i+0.5, i+j+0.5, i+2*j+0.5, i+3*j+0.5),
                         labels = c(TeX('$log_{10}a$'), TeX('$a_i$'), TeX('$a_j$'), TeX('$log_{10}b$'))) +
      # theme(legend.position = "none") +
      theme(plot.margin = unit(c(1,1,1,1), "cm"))
    
    # ggsave(filename = fname, plot = plot_par, height = 10, width = 10)
  }
  
  ind <- ind + 1
}


fname <- paste("results/misc/", "par.png", sep = "")

plot_all <- ggarrange(plot_par[[1]], plot_par[[2]], plot_par[[3]], plot_par[[4]], plot_par[[5]],
                      plot_par[[6]], plot_par[[7]], plot_par[[8]], plot_par[[9]], plot_par[[10]],
                      plot_par[[11]], plot_par[[12]],
                      nrow = 3, ncol = 4, common.legend = TRUE, legend="bottom")
# ggsave(filename = fname, plot = plot_all, height = 20, width = 20)
