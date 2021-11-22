# 20.11.2021
# We investigate the linear relationship between standardised error in food web properties wrt TSS

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
tot_fw <- length(dist_par_data$foodweb)
foodweb_all <- dist_par_data$foodweb[1:tot_fw]
nmat <- 1000

mean_err_TSS_all <- data.frame(mean_err = double(), TSS = double(), mean_TSS = double(), foodweb = double())

desc <- "TSS_lower_a"

prop_name <- c("connectance", "prop_basal", "prop_inter", "prop_top", "prop_herb",
               "clus_coeff", "sd_gen", "sd_vulner", "diet_sim", "nest_prop")

fw_title <- c("Benguela Pelagic", "Broadstone Stream \n (taxonomic aggregation)",
              "Broom", "Capinteria", "Caricaie Lakes",
              "Grasslands", "Mill Stream",
              "Skipwith Pond", "Small Reef", "Tuesday Lake",
              "Ythan", "Broadstone Stream \n (size aggregation)")

lm_list <- list()

SE_vs_TSS <- data.frame(l_SE = double(), u_SE = double(), mean_SE = double(),
                        l_TSS = double(), u_TSS = double(), mean_TSS = double(),
                        foodweb = character())

max_error <- numeric(n_prop)

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
  }
  print(fw_index)
}


for(fw_index in 1:tot_fw){
  
  fw_name <- foodweb_all[fw_index]
  
  if(fw_name == "Broadstone Stream size_agg"){
    prop_name <- c("connectance", "prop_basal", "prop_inter", "prop_top", "prop_herb",
                                 "clus_coeff", "sd_gen", "sd_vulner", "nest_prop")
    n_prop <- length(prop_name)
  }
  if(fw_name != "Broadstone Stream size_agg"){
    prop_name <- c("connectance", "prop_basal", "prop_inter", "prop_top", "prop_herb",
                   "clus_coeff", "sd_gen", "sd_vulner", "diet_sim", "nest_prop")
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
  
  mean_err_TSS_all <-rbind(mean_err_TSS_all, mean_err_TSS)
  
  CI_err <- hdi(mean_err, credMass = 0.95)
  CI_TSS <- hdi(prop_all$TSS, credMass = 0.95)
  
  SE_vs_TSS_temp <- data.frame(l_SE = as.numeric(CI_err[1]), u_SE = as.numeric(CI_err[2]), mean_SE = mean(mean_err),
                               l_TSS = as.numeric(CI_TSS[1]), u_TSS = as.numeric(CI_TSS[2]), mean_TSS = mean(prop_all$TSS),
                               foodweb = fw_name)
  
  SE_vs_TSS <- rbind(SE_vs_TSS, SE_vs_TSS_temp)
  fw_title_temp <- fw_title[fw_index]

  
  lm_local <- lm(mean_err ~ TSS, data = mean_err_TSS)
  sry_local <- summary(lm_local)
  sry_list_temp <- list(summary = sry_local, foodweb = fw_name)
  lm_list[[fw_index]] <- sry_list_temp
}


# saveRDS(object = lm_list, file = "results/misc/summary_SE_vs_TSS_lm.RDS")

