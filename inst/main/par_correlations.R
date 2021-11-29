# 06.09.2021
# We compute parameter correlations here

library(readxl)
library(tidyverse)
library(latex2exp)
dist_par_data <- read_excel("data/parameter_values.xlsx")

foodweb_all <- dist_par_data$foodweb
tot_fw <- length(foodweb_all)
desc <- "TSS_lower_a"

cor_par_df <- data.frame(log_a_ai = double(), log_a_aj = double(), log_a_log_b = double(), ai_aj = double(), ai_log_b = double(), aj_log_b = double(),
                         foodweb = character())
range_M <- data.frame(lower_M = double(), upper_M = double(), mean_M = double())
par_df <- data.frame(mean_a = double(), mean_ai = double(), mean_aj = double(), mean_b = double())

for(fw_index in 1:tot_fw){
  
  fw_name <- foodweb_all[fw_index]
  foodweb_data <- readRDS(paste("data/", fw_name, ".web.RDS", sep=""))
  
  fname <- paste("results/rejection/", fw_name, "/rN=1000_tol=", dist_par_data$dist_rej[fw_index],"_",desc,"/",
                 fw_name,".RDS", sep = "")
  
  fw_par <- readRDS(fname)$post_dists
  cor_par <- cor(fw_par)
  
  cor_par_df <- rbind(data.frame(log_a_ai = cor_par["a", "ai"], log_a_aj = cor_par["a", "aj"], log_a_log_b = cor_par["a", "r.b"], 
                                 ai_aj = cor_par["ai", "aj"], ai_log_b = cor_par["ai", "r.b"], aj_log_b = cor_par["aj", "r.b"],
                                 foodweb = fw_name)
                      , cor_par_df)
  M_min <- range(foodweb_data$species.sizes)[1]
  M_max <- range(foodweb_data$species.sizes)[2]
  M_mean <- mean(foodweb_data$species.sizes)
  
  range_M <- rbind(data.frame(lower_M = M_min, upper_M = M_max, mean_M = M_mean),
                   range_M)
  par_df <- rbind(data.frame(mean_a = mean(fw_par$a), mean_ai = mean(fw_par$ai), mean_aj = mean(fw_par$aj), mean_b = mean(fw_par$r.b)),
                  par_df)
}

df <- cor_par_df

df_v2 <- df %>%
  pivot_longer(!foodweb, names_to = "Par_pair", values_to = "Correlation")


fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)")

shape <- rep(15:18, 4)


plot_par_cor <- ggplot(df_v2) +
  geom_point(aes(x = Par_pair, y = Correlation, color = foodweb, shape = foodweb), size = 2) +
  scale_color_discrete(name = "Food web", labels = fw_labs) +
  scale_shape_manual(name = "Food web", labels = fw_labs, values = shape) +
  ylim(c(-1,1)) +
  theme_classic() +
  xlab("Parameter pairs") +
  scale_x_discrete(labels = c("log_a_ai" = parse(text = TeX('$(log_{10}a$, $a_i)$')), 
                              "log_a_aj" = parse(text = TeX('$(log_{10}a$, $a_j)$')), 
                              "log_a_log_b" = parse(text = TeX('$(log_{10}a$, $log_{10}b)$')), 
                              "ai_aj" = parse(text = TeX('$(a_i$, $a_j)$')), 
                              "ai_log_b" = parse(text = TeX('$(a_i$, $log_{10}b)$')), 
                              "aj_log_b" = parse(text = TeX('$(a_j$, $log_{10}b)$'))))

# ggsave(filename = "results/misc/par_correlation.png", plot = plot_par_cor, width = 10, height = 6)


######## Sorting parameter correlation values #######

cor_val <- c(as.matrix(cor_par_df[1:5]))
ncor <- length(cor_val)
ncor_less_than_50_per <- length(which(cor_val < 0.5))/ncor
ncor_less_than_25_per <- length(which(cor_val < 0.25))/ncor
