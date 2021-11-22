
# par_full <- readRDS("results/rejection/Benguela Pelagic/rN=1000_tol=0.6_TSS_unif_rall_WBF_3/Benguela Pelagic.Rdata")
# par_full <- par_full$post_dists
# parA <- par_full[which.min(par_full$r.b),]
# parB <- par_full[which.max(par_full$r.b),]
# 
# webA = give_web(par = parA, other_par = model_core_par) 
# webB = give_web(par = parB, other_par = model_core_par)

plot_pred_mat <- function(real_pred_mat, pred_pred_mat, dirnam, nsim, webA, webB){
  
  # conn.real.web = format(round(sum(real_pred_mat)/dim(real_pred_mat)[1]^2, 2), nsmall = 2)
  # png(paste(c(dirnam,"/",'observed_matrix.png'),collapse = ''), width=1000,height=1000)

  ## Plot the real predation matrix
  # Plot.matrix(web = real_pred_mat, title = paste("Observed predation matrix"), axes.labels = T)
  # dev.off()
  
  
  ## Plot the predicted predation matrix
  # png(paste(c(dirnam,"/",'predicted_matrix.png'),collapse = ''),width=1333,height=1000)
  # Plot.matrix_uncertainty(web = pred_pred_mat, title = paste(c("Predicted predation matrix"), collapse = ''), nsim = nsim)
  # dev.off()
  
  ## Plot the predicted predation matrix with real foodweb imposed
  # nsim <- 1000
  # png(paste(c(dirnam,"/",'predicted_matrix_real.png'),collapse = ''),width=1333,height=1000)
  # Plot.matrix_uncertainty_real(web = pred_pred_mat, real_pred_mat, title = paste(c("Predicted predation matrix for Benguela Pelagic  "), collapse = ''), nsim = nsim)
  # dev.off()
  
  ## Plot predation matrix (A+B)
  # png(paste(c(dirnam,"/",'predicted_matrix_AplusB.png'),collapse = ''),width=1100,height=300)
  # Plot.matrix_AplusB(webA = webA, webB = webB)
  # dev.off()
  
  
  plot_pred_real <- Plot.matrix_uncertainty_real(web = pred_pred_mat, real_pred_mat, title = paste(c("Predicted predation matrix for Benguela Pelagic  "), collapse = ''), nsim = nsim)
  plot_AplusB <- Plot.matrix_AplusB(webA = webA, webB = webB)
  plot_occ <- Plot_dist_occ(web = pred_pred_mat)
  fname <- paste(c(dirnam,"/",'pred_mat.png'),collapse = '')
  plot_tot <- ggarrange(plot_pred_real, plot_occ, plot_AplusB, nrow = 3, ncol = 1, heights = c(3,1,1))
  ggsave(filename = fname, plot = plot_tot, width=18, height = 25)
  
  
}
