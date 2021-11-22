## 19.11.2021

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


########### Generating Figure 7a #######

dist_par_data <- read_excel("data/parameter_values.xlsx")

foodweb_all <- dist_par_data$foodweb
tot_fw <- length(foodweb_all)
nmat <- 1000

mean_err_TSS_ABC <- data.frame(mean_err = double(), TSS = double(), mean_TSS = double(), foodweb = double())

desc <- "TSS_lower_a"

prop_name <- c("connectance", "prop_basal", "prop_inter", "prop_top", "prop_herb",
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

lm_list <- list()
n_prop <- length(prop_name)

plot_list <- list()

SE_vs_TSS <- data.frame(l_SE = double(), u_SE = double(), mean_SE = double(),
                        l_TSS = double(), u_TSS = double(), mean_TSS = double(),
                        foodweb = character())

# Computing the max SE over all the food webs for ABC
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

## Computing the standardised error for ABC method
for(fw_index in 1:tot_fw){
  
  fw_name <- foodweb_all[fw_index]
  
  if(fw_name == "Broadstone Stream size_agg"){
    prop_name <- c("connectance", "prop_basal", "prop_inter", "prop_top", "prop_herb",
                   "clus_coeff", "sd_gen", "sd_vulner", "nest_prop")
    n_prop <- length(prop_name)
  }
  if(fw_name != "Broadstone Stream size_agg"){
    prop_name <- c("connectance", "prop_basal", "prop_inter", "prop_top", "prop_herb",
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
  
  err_mat <- prop_all[prop_name]
  
  for(i in 1:nmat){
    err_mat[i,] <- abs(err_mat[i,] - real_prop)
  }
  
  for(i in 1:n_prop){
    err_mat[,i] <- err_mat[,i]/max_error[i]
  }
  
  
  err_mat_TSS <- cbind(err_mat, TSS = prop_all$TSS)
  
  mean_err <- rowMeans(err_mat)
  mean_err_TSS <- data.frame(mean_err = mean_err, TSS = prop_all$TSS, mean_TSS = mean(prop_all$TSS), foodweb = fw_name)
  
  mean_err_TSS_ABC <-rbind(mean_err_TSS_ABC, mean_err_TSS)
  
  CI_err <- hdi(mean_err, credMass = 0.95)
  CI_TSS <- hdi(prop_all$TSS, credMass = 0.95)
  
  SE_vs_TSS_temp <- data.frame(l_SE = as.numeric(CI_err[1]), u_SE = as.numeric(CI_err[2]), mean_SE = mean(mean_err),
                               l_TSS = as.numeric(CI_TSS[1]), u_TSS = as.numeric(CI_TSS[2]), mean_TSS = mean(prop_all$TSS),
                               foodweb = fw_name)
  
  SE_vs_TSS <- rbind(SE_vs_TSS, SE_vs_TSS_temp)
  
  lm_local <- lm(mean_err ~ TSS, data = mean_err_TSS)
  sry_local <- summary(lm_local)
  sry_list_temp <- list(summary = sry_local, foodweb = fw_name)
  lm_list[[fw_index]] <- sry_list_temp
}

shape <- rep(15:18, 4)

plot_SE_vs_TSS_7a <- ggplot(SE_vs_TSS) +
  geom_errorbar(aes(x = mean_TSS, y = mean_SE, ymin = l_SE, ymax = u_SE), alpha = 0.5, width = 0) +
  geom_linerange(aes(x = mean_TSS, y = mean_SE, xmin = l_TSS, xmax = u_TSS), alpha = 0.5) +
  geom_point(aes(x = mean_TSS, y = mean_SE, color = foodweb, shape = foodweb), size = 4) +
  geom_smooth(aes(x = mean_TSS, y = mean_SE), method = "lm", se = FALSE) +
  xlab("True skill statistic") +
  ylab("Mean standardised error") +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12), plot.tag = element_text(face = "bold")) +
  xlim(c(0,1)) +
  scale_y_continuous(breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5), limits = c(-0.02, 0.55)) +
  scale_color_discrete(name = "Food web", labels = fw_labs) +
  scale_shape_manual(name = "Food web", labels = fw_labs, values = shape) +
  labs(tag = "(a)", face=" bold")


lm_fig7a <- lm(mean_SE ~ mean_TSS, data = SE_vs_TSS)
summary(lm_fig7a)
anova(lm_fig7a)

########### Generating Figure 7b #####

dist_par_data <- read_excel("data/parameter_values.xlsx")
# dist_par_data <- dist_par_data[-dim(dist_par_data)[1],]

foodweb_all <- dist_par_data$foodweb
tot_fw <- length(foodweb_all)
nmat <- 1000

mean_err_TSS_ABC <- data.frame(mean_err = double(), TSS = double(), mean_TSS = double(), foodweb = double())

desc <- "TSS_lower_a"

prop_name <- c("connectance", "prop_basal", "prop_inter", "prop_top", "prop_herb",
               "clus_coeff", "sd_gen", "sd_vulner", "diet_sim", "nest_prop")

fw_title <- c("Benguela Pelagic", "Broadstone Stream \n (taxonomic aggregation)",
              "Broom", "Capinteria", "Caricaie Lakes",
              "Grasslands", "Mill Stream",
              "Skipwith Pond", "Small Reef", "Tuesday Lake",
              "Ythan")

fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan")

lm_list <- list()
n_prop <- length(prop_name)

SE_vs_TSS <- data.frame(l_SE = double(), u_SE = double(), mean_SE = double(),
                        l_TSS = double(), u_TSS = double(), mean_TSS = double(),
                        foodweb = character())


# Computing the max SE over all the food webs for ABC
max_error <- numeric(n_prop)
min_error <- numeric(n_prop)

for(fw_index in 1:tot_fw){
  
  fw_name <- foodweb_all[fw_index]
  
  if(fw_name == "Broadstone Stream size_agg"){
    prop_name <- c("connectance", "prop_basal", "prop_inter", "prop_top", "prop_herb",
                   "clus_coeff", "sd_gen", "sd_vulner", "nest_prop")
    n_prop <- length(prop_name)
  }
  if(fw_name != "Broadstone Stream size_agg"){
    prop_name <- c("connectance", "prop_basal", "prop_inter", "prop_top", "prop_herb",
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

# Computing the max SE over all the food webs for Petchey et al. (2008)

SE_Petchey <- data.frame(SE = double(), foodweb = character())
max_error_Petchey <- numeric(n_prop)

for(fw_index in 1:tot_fw){
  
  fw_name <- foodweb_all[fw_index]
  
  foodweb_data <- readRDS(paste("data/", fw_name, ".web.RDS", sep=""))
  real_prop_org <- real_prop_v2(foodweb_data)
  real_prop <- real_prop_org[prop_name]
  
  fname <- paste("data/Petchey_openaccess_foodwebs/Petchey_", fw_name, ".web.RDS", sep = "")
  
  prop_data_org <- readRDS(fname)
  prop_all <- real_prop_v2(prop_data_org)
  
  err_mat_Petchey <- prop_all[prop_name]
  
  err_mat_Petchey <- abs(err_mat_Petchey - real_prop)
  
  for(i in 1:n_prop){
    max_error_temp <- max(as.numeric(na.omit(err_mat_Petchey[,i])))
    max_error_Petchey[i] <- max(max_error_temp, max_error_Petchey[i])
    print(max_error_temp)
    max_error[i] <- max(max_error_temp, max_error[i])
  }
  print(fw_index)
}


## Computing the standardised error for ABC method
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
    err_mat[,i] <- err_mat[,i]/max_error[i]
  }
  
  
  err_mat_TSS <- cbind(err_mat, TSS = prop_all$TSS)
  
  mean_err <- rowMeans(err_mat)
  mean_err_TSS <- data.frame(mean_err = mean_err, TSS = prop_all$TSS, mean_TSS = mean(prop_all$TSS), foodweb = fw_name)
  
  mean_err_TSS_ABC <-rbind(mean_err_TSS_ABC, mean_err_TSS)
  
  CI_err <- hdi(mean_err, credMass = 0.95)
  CI_TSS <- hdi(prop_all$TSS, credMass = 0.95)
  
  SE_vs_TSS_temp <- data.frame(l_SE = as.numeric(CI_err[1]), u_SE = as.numeric(CI_err[2]), mean_SE = mean(mean_err),
                               l_TSS = as.numeric(CI_TSS[1]), u_TSS = as.numeric(CI_TSS[2]), mean_TSS = mean(prop_all$TSS),
                               foodweb = fw_name)
  
  SE_vs_TSS <- rbind(SE_vs_TSS, SE_vs_TSS_temp)
  
  lm_local <- lm(mean_err ~ TSS, data = mean_err_TSS)
  sry_local <- summary(lm_local)
  sry_list_temp <- list(summary = sry_local, foodweb = fw_name)
  lm_list[[fw_index]] <- sry_list_temp
}



## Computing the standardised error for Petchey et al. (2008)
for(fw_index in 1:tot_fw){
  
  fw_name <- foodweb_all[fw_index]
  
  foodweb_data <- readRDS(paste("data/", fw_name, ".web.RDS", sep=""))
  real_prop_org <- real_prop_v2(foodweb_data)
  real_prop <- real_prop_org[prop_name]
  
  fname <- paste("data/Petchey_openaccess_foodwebs/Petchey_", fw_name, ".web.RDS", sep = "")
  
  prop_data_org <- readRDS(fname)
  prop_all <- real_prop_v2(prop_data_org)
  
  err_mat_Petchey <- prop_all[prop_name]
  
  err_mat_Petchey <- abs(err_mat_Petchey - real_prop)
  
  for(i in 1:n_prop){
    err_mat_Petchey[,i] <- err_mat_Petchey[,i]/max_error[i]
  }
  
  
  mean_err_Petchey <- rowMeans(err_mat_Petchey)
  
  SE_Petchey_temp <- data.frame(SE = mean_err_Petchey,
                                foodweb = fw_name)
  
  SE_Petchey <- rbind(SE_Petchey, SE_Petchey_temp)

}


SE_vs_TSS_ABC_Petchey <- SE_vs_TSS
names(SE_vs_TSS_ABC_Petchey)[3] <- "mean_SE_ABC" 

SE_vs_TSS_ABC_Petchey <- cbind(SE_vs_TSS_ABC_Petchey, SE_Petchey["SE"])
names(SE_vs_TSS_ABC_Petchey)[8] <- "SE_Petchey"

plot_SE_vs_TSS_ABC_Petchey_7b <- ggplot(SE_vs_TSS_ABC_Petchey) +
  geom_point(aes(x = SE_Petchey, y = mean_SE_ABC, color = foodweb, shape = foodweb), size = 4) +
  theme_classic() +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  scale_color_discrete(name = "Food web", labels = fw_labs) +
  scale_shape_manual(name = "Food web", labels = fw_labs, values = shape) +
  # theme(legend.position = "bottom") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12), plot.tag = element_text(face = "bold")) +
  xlab("Mean standardised error (Petchey et al. 2008)") +
  ylab("Mean standardised error (ABC)") +
  labs(tag = "(b)", face=" bold")


plot_fig7 <- ggarrange(plot_SE_vs_TSS_7a, plot_SE_vs_TSS_ABC_Petchey_7b, ncol = 2, common.legend = TRUE,
                       legend = "bottom")

fname_fig7 <- "results/misc/fig7.png"
# ggsave(filename = fname_fig7, plot = plot_fig7, height = 15, width = 25, units = "cm")
