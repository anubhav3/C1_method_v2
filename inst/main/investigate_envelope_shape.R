# 22.11.2021
# We investigate the evelope shape of the points in Fig. S37-38 of SI Ver. 1

library(dplyr)
library(ggplot2)
library(tidyverse)

####### Mill Stream
fw_prop <- readRDS("results/rejection/Mill Stream/rN=1000_tol=0.75_TSS_lower_a/Mill Stream_prop.RDS")$prop

fw_prop <- readRDS("results/rejection/Grasslands/rN=1000_tol=0.95_TSS_lower_a/Grasslands_prop.RDS")$prop

fw_prop_longer <- fw_prop %>%
  pivot_longer(!TSS, names_to = "Properties", values_to = "values")

ggplot(fw_prop_longer) +
  geom_point(aes(x = TSS, y = values)) +
  facet_wrap(~Properties, scales = "free")


fw_par <- readRDS("results/rejection/Mill Stream/rN=1e+05_tol=2_TSS_lower_a/Mill Stream.RDS")
dd <- data.frame(connectance = fw_par$acc_ss, TSS = 1 - fw_par$dist)

ggplot(dd) +
  geom_point(aes(x = TSS, y = connectance), size = 0.1)


fw_par <- readRDS("results/rejection/Broadstone Stream size_agg/rN=1e+05_tol=2_TSS_lower_a/Broadstone Stream size_agg.RDS")
dd <- data.frame(connectance = fw_par$acc_ss, TSS = 1 - fw_par$dist)

ggplot(dd) +
  geom_point(aes(x = TSS, y = connectance), size = 0.1)


sim_fw_mat <- function(par, M){
  
  M <- sort(M)
  local_par <- data.frame(a = 10^par$a, ai = par$ai, aj = par$aj, r.b = 10^par$r.b)
  sim_model_core_par <- list(e = 1, n = 1, ni = -3/4, r.a = 1, M  = M)
  pred_mat <- ratio.power(opt=local_par, x = sim_model_core_par)
  
  return(pred_mat)
}

##### Plot the food webs with different connectance for a single TSS value. #######
TSS_val <- 0.5
par_sel <- readRDS("results/rejection/Mill Stream/rN=1000_tol=0.75_TSS_lower_a/Mill Stream.RDS")
fw_ind <- which((1 - par_sel$dist) > 0.495 & (1 - par_sel$dist) < 0.505)
connectance <- sort(par_sel$acc_ss[fw_ind])

order_sort_conn <- order(par_sel$acc_ss[fw_ind])
fw_ind <- fw_ind[order_sort_conn]

fw_real <- readRDS("data/Mill Stream.web.RDS")
M <- fw_real$species.sizes
par_list <- par_sel$post_dists
pred_mat_list <- list()
plot_list <- list()
i <- 1

for(k in fw_ind){
  pred_mat_list[[i]] <- sim_fw_mat(par = par_list[k,], M = M)
  plot_list[[i]] <- Plot.matrix_ggplot(pred_mat = pred_mat_list[[i]], title = paste0("Connectance = ", round(connectance[i],2)))
  i <- i + 1
  }

plot_all <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], plot_list[[5]],
          plot_list[[6]], plot_list[[7]], plot_list[[8]], plot_list[[9]], plot_list[[10]],
          nrow = 2, ncol = 5)



############ Grasslands #######

fw_par <- readRDS("results/rejection/Grasslands/rN=1e+05_tol=2_TSS_lower_a/Grasslands.RDS")
dd <- data.frame(connectance = fw_par$acc_ss, TSS = 1 - fw_par$dist)

ggplot(dd) +
  geom_point(aes(x = TSS, y = connectance))





#########  Selecting indices along the boundary #######

par_all <- readRDS("results/rejection/Mill Stream/rN=1000_tol=0.75_TSS_lower_a/Mill Stream.RDS")
TSS <- 1 - par_all$dist
conn <- par_all$acc_ss

dd <- data.frame(TSS = TSS, conn = conn)

ggplot(dd) +
  geom_point(aes(x = TSS, y = conn)) +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  geom_abline(intercept = 1.01, slope = -1)

# dd <- dd %>%
#   filter(conn > -TSS + 1.01)

sel_ind <- which(dd$conn > -dd$TSS + 1.01)

dd <- dd[sel_ind[order(dd[sel_ind,]$TSS)],]

fw_ind <- as.numeric(rownames(dd))
n_ind <- as.integer(seq(from = 1, to = 589, length = 20))
fw_ind <- fw_ind[n_ind]

fw_real <- readRDS("data/Mill Stream.web.RDS")
M <- fw_real$species.sizes
par_list <- par_all$post_dists
pred_mat_list <- list()
plot_list <- list()
connectance <- dd$conn[n_ind]
i <- 1

for(k in fw_ind){
  pred_mat_list[[i]] <- sim_fw_mat(par = par_list[k,], M = M)
  plot_list[[i]] <- Plot.matrix_ggplot(pred_mat = pred_mat_list[[i]], title = paste0("Connectance = ", round(connectance[i],2)))
  i <- i + 1
}

plot_all <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], plot_list[[5]],
                      plot_list[[6]], plot_list[[7]], plot_list[[8]], plot_list[[9]], plot_list[[10]],
                      nrow = 2, ncol = 5)

plot_all <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], plot_list[[5]],
                      plot_list[[6]], plot_list[[7]], plot_list[[8]], plot_list[[9]], plot_list[[10]],
                      plot_list[[11]], plot_list[[12]], plot_list[[13]], plot_list[[14]], plot_list[[15]],
                      plot_list[[16]], plot_list[[17]], plot_list[[18]], plot_list[[19]], plot_list[[20]],
                      nrow = 4, ncol = 5)


