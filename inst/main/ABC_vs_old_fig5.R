#We generate a graph plotting TSS, connectance and ADBM's parameters estimated from the current study (using ABC method) and Petchey et al (2008)
library(readxl)
library(HDInterval)
library(ggplot2)
library(latex2exp)
library(ggpubr)
library(R.utils)

## Relative path from the project directory
sourceDirectory("R", modifiedOnly=FALSE)

## We plot TSS 

dd_TSS <- read_excel("data/parameter_values.xlsx")
# dd_TSS <- dd_TSS[-dim(dd_TSS)[1],]
df_TSS <- data.frame(mean_ABC_TSS = double(),
                     lower_ABC_TSS = double(),
                     upper_ABC_TSS = double(),
                     point_TSS = double(),
                     fw_name = character())

fw_index <- 1
TSS_comp <- data.frame(TSS = double(), TSS_rig = double(), fname = character())
for(fw_name in dd_TSS$foodweb){
  fname <- paste("results/rejection/", fw_name, "/rN=1000_tol=",dd_TSS$dist_rej[fw_index],"_TSS_lower_a/",
                 fw_name,"_prop.RDS", sep = "")
  
  prop_data <- readRDS(fname)
  
  TSS_data <- prop_data$prop$TSS
  TSS_CI <- hdi(TSS_data, credMass = 0.95)
  mean_TSS <- mean(TSS_data)
  
  real_foodweb <- readRDS(paste("data/", fw_name, ".web.RDS", sep=""))
  Petchey_foodweb <- readRDS(paste("data/Petchey_openaccess_foodwebs/Petchey_", fw_name, ".web.RDS", sep = ""))
  TSS_rig <- 1 - dist_TSS(ss_sim = Petchey_foodweb$predation.matrix, 
                          ss_real = real_foodweb$predation.matrix)

  df_TSS <- rbind(df_TSS,
                  data.frame(mean_ABC_TSS = mean_TSS,
                             lower_ABC_TSS = as.numeric(TSS_CI[1]),
                             upper_ABC_TSS = as.numeric(TSS_CI[2]),
                             point_TSS = TSS_rig,
                             fw_name = fw_name)
  )
  fw_index <- fw_index + 1
  
}

plot_TSS <- ggplot(data = df_TSS) +
  geom_point(aes(x = point_TSS, y = mean_ABC_TSS), position = "jitter") +
  geom_errorbar(aes(x = point_TSS, ymin = lower_ABC_TSS, ymax = upper_ABC_TSS), color = "red",
                position = "dodge") +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  theme_classic() +
  xlab("TSS (Petchey et al. 2008)") +
  ylab("TSS (using ABC)") +
  labs(tag = "(a)") +
  theme(plot.tag.position = c(0.03, 1), plot.tag = element_text(face = "bold"))

##We plot connectance

dd_conn <- read_excel("data/parameter_values.xlsx")
# dd_conn <- dd_TSS[-dim(dd_conn)[1],]
df_conn <- data.frame(mean_ABC_conn = double(),
                      lower_ABC_conn = double(),
                      upper_ABC_conn = double(),
                      point_conn = double(),
                      fw_name = character())

fw_index <- 1
for(fw_name in dd_conn$foodweb){
  fname <- paste("results/rejection/", fw_name, "/rN=1000_tol=",dd_conn$dist_rej[fw_index],"_TSS_lower_a/",
                 fw_name,"_prop.RDS", sep = "")
  
  prop_data <- readRDS(fname)
  
  conn_data <- prop_data$prop$connectance
  conn_CI <- hdi(conn_data, credMass = 0.95)
  mean_conn <- mean(conn_data)
  
  Petchey_foodweb <- readRDS(paste("data/Petchey_openaccess_foodwebs/Petchey_", fw_name, ".web.RDS", sep = ""))
  Petchey_pred_mat <- Petchey_foodweb$predation.matrix
  point_conn <- sum(Petchey_pred_mat)/((dim(Petchey_pred_mat)[1])^2)
  
  df_conn <- rbind(df_conn,
                   data.frame(mean_ABC_conn = mean_conn,
                              lower_ABC_conn = as.numeric(conn_CI[1]),
                              upper_ABC_conn = as.numeric(conn_CI[2]),
                              point_conn = point_conn,
                              fw_name = fw_name)
  )
  fw_index <- fw_index + 1
  
}

plot_connectance <- ggplot(data = df_conn) +
  geom_point(aes(x = point_conn, y = mean_ABC_conn), position = "jitter") +
  geom_errorbar(aes(x = point_conn, ymin = lower_ABC_conn, ymax = upper_ABC_conn), color = "red",
                position = "dodge") +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  theme_classic() +
  xlab("Connectance (Petchey et al. 2008) \n Observed connectance") +
  ylab("Connectance (using ABC)") +
  labs(tag = "(b)") +
  theme(plot.tag.position = c(0.03, 1), plot.tag = element_text(face = "bold"))

## We plot a

dd <- read_excel("data/parameter_values.xlsx")
# dd <- dd[-dim(dd)[1],]
df_a <- data.frame(mean_ABC_a = double(),
                   lower_ABC_a = double(),
                   upper_ABC_a = double(),
                   lower_prior_a = double(),
                   upper_prior_a = double(),
                   point_a = double(),
                   fw_name = character())

fw_index <- 1
for(fw_name in dd$foodweb){
  fname <- paste("results/rejection/", fw_name, "/rN=1000_tol=",dd$dist_rej[fw_index],"_TSS_lower_a/",
                 fw_name,".RDS", sep = "")
  
  par_data <- readRDS(fname)
  
  a_data <- par_data$post_dists$a
  a_CI <- hdi(a_data)
  mean_a <- mean(a_data)
  
  df_a <- rbind(df_a,
                data.frame(mean_ABC_a = mean_a,
                           lower_ABC_a = as.numeric(a_CI[1]),
                           upper_ABC_a = as.numeric(a_CI[2]),
                           point_a = log10(dd$a[fw_index]),
                           lower_prior_a = dd$lower_prior_a[fw_index],
                           upper_prior_a = dd$upper_prior_a[fw_index],
                           fw_name = fw_name)
  )
  fw_index <- fw_index + 1
  
}

plot_a <- ggplot(data = df_a) +
  geom_point(aes(x = point_a, y = mean_ABC_a)) +
  geom_segment(aes(x = point_a, xend = point_a, y = lower_prior_a, yend = upper_prior_a), color = "grey") +
  geom_errorbar(aes(x = point_a, ymin = lower_ABC_a, ymax = upper_ABC_a), color = "red", lty = "twodash", width = 0) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  xlim(c(-25, 11)) +
  ylim(c(-25, 10)) +
  theme_classic() +
  xlab(TeX("$log_{10}(a)$ (Petchey et al. 2008)")) +
  ylab(TeX("$log_{10}(a)$ (using ABC)")) +
  labs(tag = "(c)") +
  theme(plot.tag.position = c(0.03, 1), plot.tag = element_text(face = "bold"))

## We plot parameter ai

df_ai <- data.frame(mean_ABC_ai = double(),
                    lower_ABC_ai = double(),
                    upper_ABC_ai = double(),
                    point_ai = double(),
                    fw_name = character())

fw_index <- 1
for(fw_name in dd$foodweb){
  fname <- paste("results/rejection/", fw_name, "/rN=1000_tol=",dd$dist_rej[fw_index],"_TSS_lower_a/",
                 fw_name,".RDS", sep = "")
  
  par_data <- readRDS(fname)
  
  ai_data <- par_data$post_dists$ai
  ai_CI <- hdi(ai_data)
  mean_ai <- mean(ai_data)
  
  df_ai <- rbind(df_ai,
                 data.frame(mean_ABC_ai = mean_ai,
                            lower_ABC_ai = as.numeric(ai_CI[1]),
                            upper_ABC_ai = as.numeric(ai_CI[2]),
                            point_ai = dd$ai[fw_index],
                            fw_name = fw_name)
  )
  fw_index <- fw_index + 1
  
}

plot_ai <- ggplot(data = df_ai) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -1.5, ymax = 1.5), color = "grey", alpha = 0.05) +
  geom_point(aes(x = point_ai, y = mean_ABC_ai), position = "jitter") +
  geom_errorbar(aes(x = point_ai, ymin = lower_ABC_ai, ymax = upper_ABC_ai), color = "red",
                position = "dodge") +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  # xlim(c(-3, 3)) +
  # ylim(c(-3, 3)) +
  theme_classic() +
  xlab(TeX("$a_i$ (Petchey et al. 2008)")) +
  ylab(TeX("$a_i$ (using ABC)")) +
  labs(tag = "(d)") +
  theme(plot.tag.position = c(0.03, 1), plot.tag = element_text(face = "bold"))+
  scale_y_continuous(breaks = c(-2.5, -1.5, 0, 1.5, 2.5), limits = c(-2.5,2.5))+
  scale_x_continuous(breaks = c(-3.5, -2.5, -1.5, 0, 1.5, 2.5, 3.5), limits = c(-3.5, 3.5))


## We plot parameter aj

df_aj <- data.frame(mean_ABC_aj = double(),
                    lower_ABC_aj = double(),
                    upper_ABC_aj = double(),
                    point_aj = double(),
                    fw_name = character())

fw_index <- 1
for(fw_name in dd$foodweb){
  fname <- paste("results/rejection/", fw_name, "/rN=1000_tol=",dd$dist_rej[fw_index],"_TSS_lower_a/",
                 fw_name,".RDS", sep = "")
  
  par_data <- readRDS(fname)
  
  aj_data <- par_data$post_dists$aj
  aj_CI <- hdi(aj_data)
  mean_aj <- mean(aj_data)
  
  df_aj <- rbind(df_aj,
                 data.frame(mean_ABC_aj = mean_aj,
                            lower_ABC_aj = as.numeric(aj_CI[1]),
                            upper_ABC_aj = as.numeric(aj_CI[2]),
                            point_aj = dd$aj[fw_index],
                            fw_name = fw_name)
  )
  fw_index <- fw_index + 1
  
}

plot_aj <- ggplot(data = df_aj) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 3), color = "grey", alpha = 0.05) +
  geom_point(aes(x = point_aj, y = mean_ABC_aj), position = "jitter") +
  geom_errorbar(aes(x = point_aj, ymin = lower_ABC_aj, ymax = upper_ABC_aj), color = "red",
                position = "dodge") +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  # xlim(c(-3, 3)) +
  # ylim(c(-3, 3)) +
  theme_classic() +
  xlab(TeX("$a_j$ (Petchey et al. 2008)")) +
  ylab(TeX("$a_j$ (using ABC)")) +
  labs(tag = "(e)") +
  theme(plot.tag.position = c(0.03, 1), plot.tag = element_text(face = "bold"))+
  scale_y_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3), limits = c(-3,3))+
  scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3), limits = c(-3,3))


## We plot parameter b

df_b <- data.frame(mean_ABC_b = double(),
                   lower_ABC_b = double(),
                   upper_ABC_b = double(),
                   lower_prior_b = double(),
                   upper_prior_b = double(),
                   point_b = double(),
                   fw_name = character(),
                   truncated_lower_prior_b = character(),
                   truncated_upper_prior_b = character())

fw_index <- 1
for(fw_name in dd$foodweb){
  fname <- paste("results/rejection/", fw_name, "/rN=1000_tol=",dd$dist_rej[fw_index],"_TSS_lower_a/",
                 fw_name,".RDS", sep = "")
  
  par_data <- readRDS(fname)
  
  b_data <- par_data$post_dists$r.b
  b_CI <- hdi(b_data)
  mean_b <- mean(b_data)
  
  df_b <- rbind(df_b,
                data.frame(mean_ABC_b = mean_b,
                           lower_ABC_b = as.numeric(b_CI[1]),
                           upper_ABC_b = as.numeric(b_CI[2]),
                           lower_prior_b = dd$lower_prior_r.b[fw_index],
                           upper_prior_b = dd$upper_prior_r.b[fw_index],
                           point_b = log10(dd$r.b[fw_index]),
                           fw_name = fw_name,
                           truncated_lower_prior_b = dd$truncated_lower_prior_r.b[fw_index],
                           truncated_upper_prior_b = dd$truncated_upper_prior_r.b[fw_index])
  )
  fw_index <- fw_index + 1
  
}

plot_b <- ggplot(data = df_b) +
  geom_point(aes(x = point_b, y = mean_ABC_b), position = "jitter") +
  geom_linerange(aes(x = point_b, ymin = lower_prior_b, ymax = upper_prior_b), color = "grey") +
  geom_errorbar(aes(x = point_b, ymin = lower_ABC_b, ymax = upper_ABC_b), color = "red", lty = "twodash") +
  geom_text(aes(x = point_b, y = -4, label = truncated_lower_prior_b), position = position_jitter(height = 3), size = 2) +
  geom_text(aes(x = point_b, y = 4, label = truncated_upper_prior_b), position = position_jitter(height = 3), size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  theme_classic() +
  xlab(TeX("$log_{10}(b)$ (Petchey et al. 2008)")) +
  ylab(TeX("$log_{10}(b)$ (using ABC)")) +
  labs(tag = "(f)") +
  theme(plot.tag.position = c(0.03, 1), plot.tag = element_text(face = "bold")) +
coord_cartesian(xlim= c(-6, 6), ylim = c(-6, 6))




plot_fig5 <- ggarrange(plot_TSS, plot_connectance, plot_a, plot_ai, plot_aj, plot_b, nrow = 2, ncol = 3)
# ggsave(plot = plot_fig5, file = "results/misc/ABC_vs_point_estimates.png", width = 30, height = 20, units = "cm")

