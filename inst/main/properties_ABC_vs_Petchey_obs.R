# 19.11.2021
# We plot the predicted food web properties parameterised using ABC against that from Petchey et al. (2008) and observed

library(R.utils)
library(tictoc)

## Loading required libraries
library(ggpubr); library(plotrix); library(ggplot2)
library(cowplot); library(latex2exp); library(HDInterval)
library(doParallel); library(foreach); library(raster)
library(cheddar); library(dplyr); library(reshape2)
library(psych); library(DirectedClustering)
library(readxl); library(ggjoy); library(bipartite)

## Relative path from the project directory
sourceDirectory("R", modifiedOnly=FALSE)

dist_par_data <- read_excel("data/parameter_values.xlsx")
tot_fw <- length(dist_par_data$foodweb)
foodweb_all <- dist_par_data$foodweb[1:tot_fw]


fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)")

# for figure numbering
fig_ind <- c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)", "(i)", "(j)")

shapes <- c(rep(c(1:4),4))
## Just looking at structural properties of a food web

plot_par <- list()
tot_prop <- 15
real_list <- list()
petchey_list <- list()

for(foodweb in dist_par_data$foodweb){
  foodweb_data <- readRDS(paste("data/", foodweb, ".web.RDS", sep=""))
  real_list[[foodweb]] <- real_prop_v2(foodweb_data)
  
  pred_fw <- readRDS(paste0("data/Petchey_openaccess_foodwebs/Petchey_", foodweb, ".web.RDS"))
  petchey_list[[foodweb]] <- real_prop_v2(all.web.info = pred_fw)
}


prop_name <-  c("prop_basal", "prop_inter", "prop_top", 
                "prop_herb", "mean_omn","clus_coeff", "sd_gen", 
                "sd_vulner", "diet_sim", "nest_prop")
prop_name_full <- c("Proportion of basal species", "Proportion of intermediate species", "Proportion of top species", 
                    "Proportion of herbivores", "Mean omnivory","Clustering coefficient", "SD of generality", 
                    "SD of vulnerability", "Diet similarity", "Nestedness")
desc <- "TSS_lower_a"
ind <- 1
for(pname in prop_name){
  
  df_prop <- data.frame(mean_ABC_prop = double(),
                        lower_ABC_prop = double(),
                        upper_ABC_prop = double(),
                        point_prop = double(),
                        fw_name = character(),
                        shape = double())
  
  fw_index <- 1
  for(fw_name in dist_par_data$foodweb){
    
    fname <- paste("results/rejection/", fw_name, "/rN=1000_tol=",dist_par_data$dist_rej[fw_index],"_TSS_lower_a/",
                   fw_name,"_prop.RDS", sep = "")
    
    prop_data <- readRDS(fname)
    
    prop_data <- prop_data$prop[,pname]
    prop_CI <- hdi(prop_data)
    mean_prop <- mean(na.omit(prop_data))
    
    df_prop <- rbind(df_prop,
                     data.frame(mean_ABC_prop = mean_prop,
                                lower_ABC_prop = as.numeric(prop_CI[1]),
                                upper_ABC_prop = as.numeric(prop_CI[2]),
                                real_prop = real_list[[fw_index]][,pname],
                                petchey_prop = petchey_list[[fw_index]][,pname],
                                fw_name = fw_name,
                                shape = shapes[fw_index])
    )
    fw_index <- fw_index + 1
    
  }
  
  if(ind %in% c(7,8,10)){
    plot_par[[ind]] <- ggplot(data = df_prop) +
      geom_linerange(aes(x = petchey_prop, ymin = lower_ABC_prop, ymax = upper_ABC_prop, 
                         color = fw_name, linetype = fw_name),
                     size = 1.0) +
      geom_point(aes(x = petchey_prop, y = mean_ABC_prop), position = "jitter") +
      geom_point(aes(x = petchey_prop, y = real_prop), position = "jitter", color = "green") +
      geom_abline(slope = 1, intercept = 0, linetype = 3) +
      xlim(c(0, max(df_prop$upper_ABC_prop))) +
      ylim(c(0, max(df_prop$upper_ABC_prop))) +
      theme_classic() +
      xlab(paste(prop_name_full[ind], " \n (Petchey et al. 2008)", sep = " ")) +
      ylab(paste(prop_name_full[ind], " \n (ABC [black circle] or Observed [green circle])", sep = " ")) +
      # ylab(paste(prop_name_full[ind], sep = " ")) +
      scale_color_discrete(labels = fw_labs, name = "Food web") +
      scale_linetype_manual(labels = fw_labs, name = "Food web", values = shapes) +
      labs(tag = fig_ind[ind], face="bold") +
      theme(plot.tag.position = c(0.01, 1.1), plot.tag = element_text(face = "bold"),
            legend.text = element_text(size = 12))
  }
  
  else{
    plot_par[[ind]] <- ggplot(data = df_prop) +
      geom_linerange(aes(x = petchey_prop, ymin = lower_ABC_prop, ymax = upper_ABC_prop, 
                         color = fw_name, linetype = fw_name),
                     size = 1.0) +
      geom_point(aes(x = petchey_prop, y = real_prop), position = "jitter", color = "green") +
      geom_point(aes(x = petchey_prop, y = mean_ABC_prop), position = "jitter") +
      geom_abline(slope = 1, intercept = 0, linetype = 3) +
      xlim(c(0,1)) +
      ylim(c(0,1)) +
      theme_classic() +
      # theme(legend.position = "none")+
      xlab(paste(prop_name_full[ind], " \n (Petchey et al. 2008)", sep = " ")) +
      ylab(paste(prop_name_full[ind], " \n (ABC [black circle] or Observed [green circle])", sep = " ")) +
      # ylab(paste(prop_name_full[ind], sep = " ")) +
      scale_color_discrete(labels = fw_labs, name = "Food web") +
      scale_linetype_manual(labels = fw_labs, name = "Food web", values = shapes) +
      labs(tag = fig_ind[ind]) +
      theme(plot.tag.position = c(0.01, 1.1), plot.tag = element_text(face = "bold"),
            legend.text = element_text(size = 12))
  }
  
  ind <- ind + 1
  
}

fname <- paste("results/misc/", "r_properties_ABC_vs_petchey_", desc, ".png", sep = "")

plot_all <- ggarrange(plot_par[[1]], plot_par[[2]], plot_par[[3]], plot_par[[4]], plot_par[[5]],
                      plot_par[[6]], plot_par[[7]], plot_par[[8]], plot_par[[9]], plot_par[[10]],
                      common.legend = TRUE, legend = "bottom",
                      nrow = 3, ncol = 4) +
  theme(plot.margin = margin(1,0.1,0.1,1, "cm"))

# ggsave(filename = fname, plot = plot_all, height = 30, width = 40, units = "cm")

