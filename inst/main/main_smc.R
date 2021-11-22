library(R.utils); library(tictoc); library(stringr)
library(ggpubr); library(plotrix); library(ggplot2)
library(cowplot); library(latex2exp); library(HDInterval)
library(doParallel); library(foreach); library(raster)
library(cheddar); library(dplyr); library(reshape2)
library(psych); library(DirectedClustering); library(colorspace)
library(ggimage)

## Start of computation time
tic("Time elapsed:")

## Relative path from the project directory
sourceDirectory("R", modifiedOnly=FALSE)

#Remember to check these:
fw_name <- "Ythan"
prior_dist_pbly <- prior_prob_joint_smc
prior_dist_x <- prior_unif_x
proposal_dist_pbly <- proposal_prob_joint_smc
proposal_dist_x <- proposal_dist_joint_x_smc
dist_ss <- dist_TSS
desc <- "TSS_lower_a"
##########################

fname_data <- paste("data/", fw_name, ".web.RDS", sep="")
fw_data <- readRDS(fname_data)

npar <- 4
model <- ratio.power_exp
model_core_par <- ADBM_core_par(fw_data)
model_prior_par <- ADBM_prior_par(fw_data)
epsilon <- epsilon_t()
input_par <- input_parameters()

## Creating a directory where all the results will be stored
dir_N <- input_par$N 
dir_tol <- input_par$tol
dirnam <- paste(c("results/smc/",fw_name,"/",'sN=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
dir.create(paste0("results/smc/",fw_name))
dir.create(dirnam)

output <- smc(all.web.info = fw_data, model = model, model_core_par,
                      model_prior_par, input_par, dist_ss,
                      prior_dist_x, prior_dist_pbly, proposal_dist_x,
                      proposal_dist_pbly, epsilon, npar)

saveRDS(output, file = paste(c(dirnam,"/",fw_name,".RDS"), collapse = ''))

time_elap <- toc()

## Displaying some important information in a pdf file
display_info_smc(time = time_elap, dirnam = dirnam)

fname_output <- paste(dirnam, "/", fw_name, ".RDS", sep = "")
output <- readRDS(file = fname_output)

## Creating parameters'distribution graph
plot_smc(output, dirnam)

for(n_th in 1:5){
  post_dists <- as.data.frame(output$post_dists[,,n_th])
  names(post_dists) <- c("a","ai","aj","r.b")
  ind <- which(output$w[,n_th] != 0)
  predicted_foodweb <- list(post_dists = post_dists[ind,],
                            w = output$w[,n_th][ind],
                            tau_sq = output$tau_sq[n_th,,],
                            dist = output$dist[n_th,][ind])
  dirnam_sub <- paste(c(dirnam,"/", n_th), collapse = '')
  dir.create(dirnam_sub)
  fname <- paste(dirnam_sub, "/", fw_name, ".RDS", sep = "")
  saveRDS(predicted_foodweb, file = fname)

  ## Comment this section for producing only graphs:
  # tic()
  # properties <- fw_prop(predicted_foodweb, model_core_par, all.web.info = fw_data)
  # fname_new <- paste(dirnam_sub, "/", fw_name,"_prop.RDS", sep = "")
  # saveRDS(properties, file = fname_new)
  # time_elap_fw <- toc()
  # 
  # ## Displaying some important information in a pdf file
  # display_info_prop(time = time_elap_fw, dirnam = dirnam_sub)
  # #######################################################
  # 
  # fname_pred_fw <- paste(dirnam_sub, "/", fw_name, ".RDS", sep = "")
  # predicted_foodweb <- readRDS(file = fname_pred_fw)
  # fname_prop <- paste(dirnam_sub, "/", fw_name,"_prop.RDS", sep = "")
  # prop_web <- readRDS(file = fname_prop)
  # 
  # plot_foodweb_prop(real_foodweb = fw_data, predicted_foodweb = predicted_foodweb,
  #                   dirnam = dirnam_sub, model_core_par = model_core_par,
  #                   model_prior_par = model_prior_par, model = model,
  #                   desc = desc, prior_dist_x = prior_dist_x,
  #                   web.to.analyse = foodweb, prop_web = prop_web,
  #                   true_val_plot = F)

}
