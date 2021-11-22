plot_prop <- function(dd, property, real, x_min, x_max){
  dd = data.frame(data = dd)
  inter <- hdi(dd$data)
  mean_data <- mean(dd$data)
  
  df_extra <- data.frame(xintercept = c(real, inter[1], inter[2], mean_data),
                         type = c("real food web", "credible interval", "credible interval", "mean of the distribution"))
  ggplot(dd, aes(x = dd$data, y = ..density..)) +
    geom_histogram(bins = 50) +
    # geom_vline(color = "green", size = 1.5, aes(xintercept = real, color = "green")) +
    # geom_vline(color = "red", aes(xintercept = inter[1], color = "red"))+
    # geom_vline(xintercept = inter[2], color = "red")+
    # geom_vline(color = "blue",size = 1.5, aes(xintercept = mean_data, color = "blue"))+
    geom_vline(data = df_extra, aes(xintercept = xintercept, color = type), size = 1.5) +
    theme_classic()+
    xlab(property) +
    # ylab("Distribution")+
    theme(axis.title=element_text(family="Times New Roman", size = 20), axis.title.y = element_blank(),
          legend.title = element_blank()) +
    xlim(c(x_min, x_max)) 
}

plot_foodweb <- function(real_foodweb = real_foodweb, predicted_foodweb = predicted_foodweb,
                         dirnam = dirnam, model_core_par = model_core_par, 
                         model_prior_par = model_prior_par,
                         model = model, desc, prior_dist_x, 
                         web.to.analyse = web.to.analyse){
  nsim <- dim(predicted_foodweb$post_dists)[1]
  prop_web <- fw_prop(predicted_foodweb, model_core_par, real_foodweb)
  
  saveRDS(prop_web, file = paste(c(dirnam,"/",web.to.analyse,"_prop",".Rdata"), collapse = ''))
  real <- real_prop(real_foodweb)
  prop <- prop_web$prop
  best.web <- prop_web$web
  prop.0.corr <- prop$prop.0.corr
  prop.1.corr <- prop$prop.1.corr
  
  # Plotting predation matrices
  fname <- paste(dirnam, "/", web.to.analyse, ".Rdata", sep = "")
  par_full <- readRDS(fname)
  par_full <- par_full$post_dists
  parA <- par_full[which.min(par_full$r.b),]
  parB <- par_full[which.max(par_full$r.b),]

  webA = give_web(par = parA, other_par = model_core_par)
  webB = give_web(par = parB, other_par = model_core_par)
  plot_pred_mat(real_pred_mat = real_foodweb$predation.matrix, pred_pred_mat = best.web,
                dirnam = dirnam, nsim = nsim, webA = webA, webB = webB)
  

  # ## Plotting the structural properties of food web
  try(plot_str_prop(prop = prop, real = real, dirnam = dirnam))


  ## Plotting proportion of 'links' correct vs proportion of 'no links' correct
  plot_links_vs_no_links(prop.1.corr, prop.0.corr, prop$connectance, desc, dirnam)


  ## Plotting distribution of proportions correct
  plot_dist_prop(prop$connectance, prop.1.corr, prop.0.corr, predicted_foodweb$dist,
                 prop$ACC, prop$TSS, dirnam, real$connectance)


  ## Plotting pairwise plots for parameters
  png(paste(c(dirnam,"/",'pairwise.png'),collapse = ''),width=3000,height=3000, pointsize = 50)
  pairs.panels(predicted_foodweb$post_dists, method = "pearson")
  dev.off()

  ## Plotting marginal distribution of the parameters
  plot_marg_dist(predicted_foodweb = predicted_foodweb, prior_dist = prior_dist,
                 dirnam = dirnam, desc = desc, model_prior_par = model_prior_par,
                 real_foodweb = real_foodweb, true_val_plot = F)

}

plot_foodweb_prop <- function(real_foodweb, predicted_foodweb, dirnam, model_core_par, model_prior_par,
                         model, desc, prior_dist_x, web.to.analyse, prop_web, true_val_plot
                         ){
  
  nsim <- dim(predicted_foodweb$post_dists)[1]
  print(111)
  real <- real_prop_v2(real_foodweb)
  prop <- prop_web$prop
  best.web <- prop_web$web
  prop.0.corr <- prop$prop.0.corr
  prop.1.corr <- prop$prop.1.corr
  print(222)
  
  # Plotting predation matrices
  # fname <- paste(dirnam, "/", web.to.analyse, ".Rdata", sep = "")
  # par_full <- readRDS(fname)
  par_full <- predicted_foodweb$post_dists
  parA <- par_full[which.min(par_full$r.b),]
  parB <- par_full[which.max(par_full$r.b),]
  # print(par_full)
  webA = give_web(par = parA, other_par = model_core_par)
  webB = give_web(par = parB, other_par = model_core_par)
  print(344)
  plot_pred_mat(real_pred_mat = real_foodweb$predation.matrix, pred_pred_mat = best.web,
                dirnam = dirnam, nsim = nsim, webA = webA, webB = webB)
  print(444)
  
  
  # ## Plotting the structural properties of food web
  try(plot_str_prop(prop = prop, real = real, dirnam = dirnam))
  
  
  ## Plotting proportion of 'links' correct vs proportion of 'no links' correct
  plot_links_vs_no_links(prop.1.corr, prop.0.corr, prop$connectance, desc, dirnam)
  print(123)
  
  ## Plotting distribution of proportions correct
  plot_dist_prop(prop$connectance, prop.1.corr, prop.0.corr, predicted_foodweb$dist,
                 prop$ACC, prop$TSS, dirnam, real$connectance)
  # 
  
  ## Plotting pairwise plots for parameters
  png(paste(c(dirnam,"/",'pairwise.png'),collapse = ''),width=3000,height=3000, pointsize = 50)
  pairs.panels(predicted_foodweb$post_dists, method = "pearson")
  dev.off()
  
  ## Plotting marginal distribution of the parameters
  plot_marg_dist(predicted_foodweb = predicted_foodweb, prior_dist = prior_dist_x,
                 dirnam = dirnam, desc = desc, model_prior_par = model_prior_par,
                 real_foodweb = real_foodweb, true_val_plot = true_val_plot,
                 parA = parA, parB = parB)
  print(789)
}


