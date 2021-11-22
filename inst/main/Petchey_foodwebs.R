# 17.11.2021
# We generate the food webs from Petchey et al. (2008)

fw_name <- "Ythan"
fw_par_all <- readRDS(paste0("data/fitpar/", fw_name, "NMRHweb_openaccess.RDS"))
fw_par <- fw_par_all$pars
fw_data <- readRDS(paste0("data/", fw_name, ".web.RDS"))

n_species <- length(fw_data$species.sizes)

# set.seed(1)
M <- fw_data$species.sizes
M <- sort(M)

sim_a <- fw_par["a"]
sim_ai <- fw_par["ai.V1"]
sim_aj <- fw_par["aj.V2"]
sim_r.b <- fw_par["r.b.V3"]

local_par <- data.frame(a = sim_a, ai = sim_ai, aj = sim_aj, r.b = sim_r.b)
sim_model_core_par <- list(e = 1, n = 1, ni = -3/4, r.a = 1, M  = M)
pred_mat <- ratio.power(opt=local_par, x = sim_model_core_par)
rownames(pred_mat) <- as.character(seq(1, n_species))
colnames(pred_mat) <- as.character(seq(1, n_species))


all.web.info <- list(web.name = fw_name, species.sizes = M, 
                     species.names = as.character(seq(1, n_species)),
                     predation.matrix = pred_mat, a = sim_a, ai = sim_ai, 
                     aj = sim_aj, r.b = sim_r.b)

# saveRDS(object = all.web.info, 
#         file = paste0("data/Petchey_openaccess_foodwebs/Petchey_", fw_name, ".web.RDS"))



######## For Broadstone Stream size_agg ########
fw_name <- "Broadstone Stream size_agg"
fw_data <- readRDS(paste0("data/", fw_name, ".web.RDS"))

n_species <- length(fw_data$species.sizes)

# set.seed(1)
M <- fw_data$species.sizes
M <- sort(M)

sim_a <- 4.39E-13
sim_ai <- -3.45E+00
sim_aj <- 4.78E-01
sim_r.b <- 2.19E+00

local_par <- data.frame(a = sim_a, ai = sim_ai, aj = sim_aj, r.b = sim_r.b)
sim_model_core_par <- list(e = 1, n = 1, ni = -3/4, r.a = 1, M  = M)
pred_mat <- ratio.power(opt=local_par, x = sim_model_core_par)
rownames(pred_mat) <- as.character(seq(1, n_species))
colnames(pred_mat) <- as.character(seq(1, n_species))


all.web.info <- list(web.name = fw_name, species.sizes = M, 
                     species.names = as.character(seq(1, n_species)),
                     predation.matrix = pred_mat, a = sim_a, ai = sim_ai, 
                     aj = sim_aj, r.b = sim_r.b)

# saveRDS(object = all.web.info, 
#         file = paste0("data/Petchey_openaccess_foodwebs/Petchey_", fw_name, ".web.RDS"))

