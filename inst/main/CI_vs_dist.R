plot_CI_vs_dist <- function(dist_x, conn_x, dirnam, true_val, fw_name){
  order_index_dist <- order(dist_x)
  
  sort_dist <- dist_x[order_index_dist]
  sort_conn <- conn_x[order_index_dist]
  
  nsim <- length(dist_x)
  nt <- 40
  
  dist <- numeric(nt)
  ci_lower <- numeric(nt)
  ci_upper <- numeric(nt)
  mean_r <- numeric(nt)
  dist_ind <- numeric(nt)
  
  for(i in 1:nt){
    # dist_ind[i] <- which.max(sort_dist >= i/10)
    # conn <- sort_conn[seq(1,dist_ind[i])]
    conn <- sort_conn[which(sort_dist <= i/20)]
    
    ci <- hdi(conn, credMass = 0.99999)
    ci_lower[i] <- ci[1]
    ci_upper[i] <- ci[2]
    mean_r[i] <- mean(conn)
    dist[i] <- i/20
  }
  
  dd <- data.frame(x = dist, y = mean_r, ci_lower = ci_lower, ci_upper = ci_upper)
  
  fname <- paste(dirnam, "/",fw_name, "_connectanceCI_vs_tol.png", sep = "")
  dd_legend <- data.frame(x = c(-10, -10, -10), legend = c("Mean", "Prediction interval", "Observed connectance"))
  cols <- c("Mean" = "black", "Prediction interval" = "red", "Observed connectance" = "green")
  ggplot(dd) +
    geom_point(aes(x  = dist, y = mean_r)) +
    geom_line(aes(x  = dist, y = mean_r)) +
    geom_errorbar(aes(x  = dist, ymin = ci_lower, ymax = ci_upper), col = "red") +
    xlab("Distance threshold") +
    ylab("Connectance") +
    # labs(title = "Connectance vs distance (ss = TSS)") +
    # scale_x_reverse() +
    geom_hline(yintercept = true_val, col = "green") +
    geom_vline(data = dd_legend, aes(xintercept = x, color = legend), size = 1.5) +
    theme_bw() +
    theme(axis.title = element_text(size=30, family="Times New Roman"), 
          axis.text = element_text(size=20, family="Times New Roman"),
          legend.text = element_text(size=20, family="Times New Roman" ),
          legend.title = element_text(size=25, family="Times New Roman" )) +
    xlim(c(0,2.1)) +
    ylim(c(0,1)) +
    scale_color_manual(values = cols, name = "Legend") 
  
  ggsave(filename = fname, width = 25, height = 20, units = "cm")
  
}

fw_name <- "Broadstone Stream size_agg"
dirnam <- paste("results/rejection/",fw_name, "/rN=1e+05_tol=2_TSS_lower_a", sep = "")

fw_data <- readRDS(paste(dirnam, "/", fw_name, ".RDS", sep = ""))
fw_dist <- fw_data$dist
fw_conn <- fw_data$acc_ss


fname_real <- paste("data/", fw_name, ".web.RDS", sep= "")
fw_real <- readRDS(file = fname_real)

real_conn <- real_prop(all.web.info = fw_real)$connectance

plot_CI_vs_dist(fw_dist, fw_conn, dirnam, real_conn, fw_name)





