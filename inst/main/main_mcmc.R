library(R.utils); library(tictoc); library(stringr)
library(ggpubr); library(plotrix); library(ggplot2)
library(cowplot); library(latex2exp); library(HDInterval)
library(doParallel); library(foreach); library(raster)
library(cheddar); library(dplyr); library(reshape2)
library(psych); library(DirectedClustering); library(colorspace)
library(coda); library(ggimage); library(bipartite)

## Start of computation time
tic("Time elapsed:")

## Relative path from the project directory
sourceDirectory("R", modifiedOnly=FALSE)

#Remember to check these:
fw_name <- "Broadstone Stream size_agg"
prior_dist_pbly <- prior_pbly_unif
prior_dist_x <- prior_unif_x
proposal_dist_pbly <- proposal_dist_pbly_unif_mcmc
proposal_dist_x <- proposal_dist_x_unif_mcmc
ADBM_par_sd_f <- ADBM_par_sd()
dist_ss <- dist_TSS
desc <- "TSS_lower_a"
##########################

fname_data <- paste("data/", fw_name, ".web.RDS", sep="")
fw_data <- readRDS(fname_data)

model <- ratio.power_exp
model_core_par <- ADBM_core_par(fw_data)
model_prior_par <- ADBM_prior_par(fw_data)
input_par <- input_parameters()

## Creating a directory where all the results will be stored
dir_N <- input_par$N 
dir_tol <- input_par$tol
dirnam <- paste(c("results/mcmc/",fw_name,"/",'mN=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
dir.create(paste0("results/mcmc/",fw_name))
dir.create(dirnam)

output <- mcmc(all.web.info = fw_data, model = model, model_core_par = model_core_par,
               model_prior_par = model_prior_par, input_parameters = input_par, 
               dist_ss = dist_ss, prior_dist_pbly= prior_dist_pbly, 
               prior_dist_x= prior_dist_x, proposal_dist_pbly = proposal_dist_pbly, 
               proposal_dist_x =  proposal_dist_x, ADBM_par_sd = ADBM_par_sd_f)

saveRDS(output, file = paste(c(dirnam,"/",fw_name,".RDS"), collapse = ''))

time_elap <- toc()
print(paste("No.of jumps =", output[[1]]$n_jump))

## Displaying some important information in a pdf file
display_info_mcmc(prop_acc_sim = output[[1]]$n_jump, time = time_elap, dirnam = dirnam)

#burnin and thinning and convergence diagnostic
fname_raw_output <- paste(dirnam, "/", fw_name, ".RDS", sep = "")
output <- readRDS(file = fname_raw_output)
thin_output <- mcmc_burn_thin(output[[1]])
conv_result <- mcmc_chk_conv(output)
fname_conv <- paste(dirnam, "/", fw_name, "_conv.RDS", sep = "")
saveRDS(conv_result, file = fname_conv)
fname_thin <- paste(dirnam, "/", fw_name, "_thin.RDS", sep = "")
saveRDS(thin_output, file = fname_thin)

# tic()
# ## Computing food web properties
# fname_output <- paste(dirnam, "/", fw_name, "_thin.RDS", sep = "")
# output <- readRDS(file = fname_output)
# properties <- fw_prop(foodweb = output, other = model_core_par, all.web.info = fw_data)
# fname_prop <- paste(dirnam, "/", fw_name,"_prop.RDS", sep = "")
# saveRDS(properties, file = fname_prop)
# 
# time_elap_fw <- toc()
# 
# ## Displaying some important information in a pdf file
# display_info_prop(time = time_elap_fw, dirnam = dirnam)
# 
# fname_pred_fw <- paste(dirnam, "/", fw_name, "_thin.RDS", sep = "")
# predicted_foodweb <- readRDS(file = fname_pred_fw)
# 
# fname_prop <- paste(dirnam, "/", fw_name,"_prop.RDS", sep = "")
# prop_web <- readRDS(file = fname_prop)
# 
# ## Plotting the real and predicted food web matrix alongwith some food web properties
# plot_foodweb_prop(real_foodweb = fw_data, predicted_foodweb = predicted_foodweb,
#                   dirnam = dirnam, model_core_par = model_core_par, 
#                   model_prior_par = model_prior_par,
#                   model = model, desc = desc, prior_dist_x = prior_dist_x, 
#                   web.to.analyse = fw_name, prop_web = prop_web,
#                   true_val_plot = F)
