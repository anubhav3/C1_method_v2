# 21.11.2021
# Investigating how mean standardised error in str prop (exc connectance) varies with mean standardised error in connectance

library(R.utils)
library(tictoc)

## Loading required libraries
library(ggpubr); library(plotrix); library(ggplot2)
library(cowplot); library(latex2exp); library(HDInterval)
library(doParallel); library(foreach); library(raster)
library(cheddar); library(dplyr); library(reshape2)
library(psych); library(DirectedClustering); library(bipartite)
library(readxl); library(ggjoy); library(HDInterval)

## Relative path from the project directory
sourceDirectory("R", modifiedOnly=FALSE)

dist_par_data <- read_excel("data/parameter_values.xlsx")

foodweb_all <- dist_par_data$foodweb
tot_fw <- length(foodweb_all)
nmat <- 1000

mean_err_TSS_ABC <- data.frame(mean_err = double(), TSS = double(), mean_TSS = double(), foodweb = double())
desc <- "TSS_lower_a"

prop_name <- c("prop_basal", "prop_inter", "prop_top", "prop_herb",
               "clus_coeff", "sd_gen", "sd_vulner", "nest_prop", "diet_sim")

fw_title <- c("Benguela Pelagic", "Broadstone Stream \n (taxonomic aggregation)",
              "Broom", "Capinteria", "Caricaie Lakes",
              "Grasslands", "Mill Stream",
              "Skipwith Pond", "Small Reef", "Tuesday Lake",
              "Ythan", "Broadstone Stream \n (size aggregation)")

fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)")

# for figure numbering
fig_ind <- c("(a)", "(b)", "(c)", "(d)", 
             "(e)", "(f)", "(g)", "(h)", 
             "(i)", "(j)", "(k)", "(l)")


n_prop <- length(prop_name)

plot_list <- list()

SE_vs_TSS <- data.frame(l_SE = double(), u_SE = double(), mean_SE = double(),
                        l_TSS = double(), u_TSS = double(), mean_TSS = double(),
                        foodweb = character, mean_SE_conn = double())

#### Computing the max SE for connectance over all the food webs for ABC ####
max_error_conn <- numeric(1)
min_error_conn <- numeric(1)

for(fw_index in 1:tot_fw){
  
  fw_name <- foodweb_all[fw_index]
  
  foodweb_data <- readRDS(paste("data/", fw_name, ".web.RDS", sep=""))
  real_prop_org <- real_prop_v2(foodweb_data)
  
  fname <- paste("results/rejection/", fw_name, "/rN=1000_tol=", dist_par_data$dist_rej[fw_index],"_",desc,"/",
                 fw_name,"_prop.RDS", sep = "")
  
  prop_data_org <- readRDS(fname)
  prop_all <- prop_data_org$prop["connectance"]
  real_prop <- real_prop_org["connectance"]
  
  err_mat <- as.numeric(prop_all$connectance)
  
  for(i in 1:nmat){
    err_mat[i] <- abs(err_mat[i] - real_prop)
  }
  
  max_error_conn <- max(as.numeric(na.omit(err_mat)))
  
  min_error_conn <- min(as.numeric(na.omit(err_mat)))
  
  print(fw_index)
}


#### Computing the max SE over all the food webs for ABC ####
max_error <- numeric(n_prop)
min_error <- numeric(n_prop)

for(fw_index in 1:tot_fw){
  
  fw_name <- foodweb_all[fw_index]
  
  foodweb_data <- readRDS(paste("data/", fw_name, ".web.RDS", sep=""))
  real_prop_org <- real_prop_v2(foodweb_data)
  
  fname <- paste("results/rejection/", fw_name, "/rN=1000_tol=", dist_par_data$dist_rej[fw_index],"_",desc,"/",
                 fw_name,"_prop.RDS", sep = "")
  
  prop_data_org <- readRDS(fname)
  prop_all <- prop_data_org$prop
  real_prop <- real_prop_org[prop_name]
  
  err_mat <- prop_all[prop_name]
  
  for(i in 1:nmat){
    err_mat[i,] <- abs(err_mat[i,] - real_prop)
  }
  
  for(i in 1:n_prop){
    max_error_temp <- max(as.numeric(na.omit(err_mat[,i])))
    print(max_error_temp)
    max_error[i] <- max(max_error_temp, max_error[i])
    
    min_error_temp <- min(as.numeric(na.omit(err_mat[,i])))
    min_error[i] <- max(min_error_temp, min_error[i])
  }
  print(fw_index)
}

#### Computing the standardised error for ABC method ####
for(fw_index in 1:tot_fw){
  
  fw_name <- foodweb_all[fw_index]
  
  if(fw_name == "Broadstone Stream size_agg"){
    prop_name <- c("prop_basal", "prop_inter", "prop_top", "prop_herb",
                   "clus_coeff", "sd_gen", "sd_vulner", "nest_prop")
    n_prop <- length(prop_name)
  }
  
  if(fw_name != "Broadstone Stream size_agg"){
    prop_name <- c("prop_basal", "prop_inter", "prop_top", "prop_herb",
                   "clus_coeff", "sd_gen", "sd_vulner", "nest_prop", "diet_sim")
    n_prop <- length(prop_name)
  }
  
  foodweb_data <- readRDS(paste("data/", fw_name, ".web.RDS", sep=""))
  real_prop_org <- real_prop_v2(foodweb_data)
  
  fname <- paste("results/rejection/", fw_name, "/rN=1000_tol=", dist_par_data$dist_rej[fw_index],"_",desc,"/",
                 fw_name,"_prop.RDS", sep = "")
  
  prop_data_org <- readRDS(fname)
  prop_all <- prop_data_org$prop
  real_prop <- real_prop_org[prop_name]
  real_prop_conn <- as.numeric(real_prop_org["connectance"])
  
  err_mat <- prop_all[prop_name]
  err_mat_conn <- as.numeric(prop_all$connectance)
  
  for(i in 1:nmat){
    err_mat[i,] <- abs(err_mat[i,] - real_prop)
    err_mat_conn[i] <- abs(err_mat_conn[i] - real_prop_conn)
  }
  
  for(i in 1:n_prop){
    err_mat[,i] <- err_mat[,i]/max_error[i]
    err_mat_conn[i] <- err_mat_conn[i]/max_error_conn
  }
  
  
  err_mat_TSS <- cbind(err_mat, TSS = prop_all$TSS)
  
  mean_err <- rowMeans(err_mat)
  mean_err_TSS <- data.frame(mean_err = mean_err, TSS = prop_all$TSS, mean_TSS = mean(prop_all$TSS), foodweb = fw_name)
  mean_err_conn <- data.frame(mean_err = mean_err, SE_conn = err_mat_conn, foodweb = fw_name)
  mean_err_TSS_ABC <-rbind(mean_err_TSS_ABC, mean_err_TSS)
  
  CI_err <- hdi(mean_err, credMass = 0.95)
  CI_TSS <- hdi(prop_all$TSS, credMass = 0.95)
  
  SE_vs_TSS_temp <- data.frame(l_SE = as.numeric(CI_err[1]), u_SE = as.numeric(CI_err[2]), mean_SE = mean(mean_err),
                               l_TSS = as.numeric(CI_TSS[1]), u_TSS = as.numeric(CI_TSS[2]), mean_TSS = mean(prop_all$TSS),
                               foodweb = fw_name, mean_SE_conn = mean(err_mat_conn))
  
  SE_vs_TSS <- rbind(SE_vs_TSS, SE_vs_TSS_temp)
  fw_title_temp <- fw_title[fw_index]
  
  plot_list[[fw_index]] <- ggplot(mean_err_conn) +
    geom_point(aes(x = SE_conn, y = mean_err), alpha = 0.1) +
    # geom_smooth(aes(x = SE_conn, y = mean_err), method = "lm", color = "black", se = FALSE) +
    xlab("Mean standardised error in connectance") +
    ylab("Mean standardised error") +
    theme_classic() +
    # ylim(c(0,1)) +
    # xlim(c(0,1)) +
    labs(title = fw_title_temp, tag = fig_ind[fw_index]) +
    theme(plot.title = element_text(hjust = 0.5), plot.tag = element_text(face = "bold"))
}

plot_all <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], plot_list[[5]],
                      plot_list[[6]], plot_list[[7]], plot_list[[8]], plot_list[[9]], plot_list[[10]],
                      plot_list[[11]], plot_list[[12]],
                      
                      nrow = 3, ncol = 4, common.legend = TRUE, legend ="bottom",
)


# ggsave(plot = plot_all, filename = "results/misc/mse_str_prop_vs_mse_conn")

shape <- rep(15:18, 4)

plot_mse_str_prop_vs_mse_conn <- ggplot(SE_vs_TSS) +
  geom_point(aes(x = mean_SE_conn, y = mean_SE, color = foodweb, shape = foodweb), size = 4) +
  geom_smooth(aes(x = mean_SE_conn, y = mean_SE), method = "lm") +
  theme_classic() +
  xlab("Mean standardised error in connectance") +
  ylab("Mean standardised error in structural properties") +
  scale_shape_manual(name = "Food web", labels = fw_labs, values = shape) +
  scale_color_discrete(name = "Food web", labels = fw_labs) 

# ggsave(plot = plot_mse_str_prop_vs_mse_conn, filename = "results/misc/plot_mse_str_prop_vs_mse_conn.png")


lin_mod <- lm(mean_SE ~ mean_SE_conn, data = SE_vs_TSS)
summary(lin_mod)
anova(lin_mod)
