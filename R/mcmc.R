mcmc <- function(all.web.info, model = ratio.power, model_core_par = ADBM_core_par,
                 model_prior_par= ADBM_prior_par, input_parameters,dist_ss,
                 prior_dist_x, prior_dist_pbly, proposal_dist_pbly,
                 proposal_dist_x, ADBM_par_sd)
{
  M <- all.web.info$species.sizes    #body size
  tol <- input_parameters$tol
  N <- input_parameters$N
  n_cores <- input_parameters$n_cores

  
  registerDoParallel(cores=n_cores)
  
  ss_real <- model_core_par$real.web
  
  post_dists <- data.frame(a=double(), ai=double(), aj=double(), r.b=double())
  
  acc_ss <- numeric(N)
  dist <- numeric(N)
  qnty <- numeric(N)
  
  final_list <- list()
  res_pcores <- foreach(i = 1:n_cores) %dopar%{
    ## Initialise:
    set.seed(i*9)
    Kh <- 0
    K <- weights(0, tol, "epanechnikov")
    while(Kh<=0){
      local_par <- prior_dist_x(par=model_prior_par, no=1)
      ss_sim <- model(opt=local_par, x=model_core_par)
      dist_temp <- dist_ss(ss_sim = ss_sim, ss_real = ss_real)
      Kh <- weights(dist_temp, tol, "epanechnikov")/K
    }
    
    dist[1] <- dist_temp
    
    ## Sampling:
    par_i <- local_par
    Kh_i <- Kh
    post_dists <- local_par
    i <- 1
    n_jump <- 1
    n_count <- numeric()
    n_count[n_jump] <- 1
    
    t_prior_frac <- as.numeric()
    t_prop_frac <- as.numeric()
    t_Kh_frac <- as.numeric()
    
    while(i <= N-1){
      par_temp <- proposal_dist_x(par_i, ADBM_par_sd)
      ss_sim_temp <- try(model(opt=par_temp, x=model_core_par))
      
      if(is.numeric(ss_sim_temp) == TRUE){
        
        distt <- dist_ss(ss_sim = ss_sim_temp, ss_real = ss_real)
        dist[i] <- distt
        
        Kh_temp <- weights(distt, tol = tol, kernel = "epanechnikov")/K
        prior_frac <- (prior_dist_pbly(par_temp, model_prior_par)/prior_dist_pbly(par_i, model_prior_par))
        prop_frac <- (proposal_dist_pbly(par_temp, par_i, ADBM_par_sd)/proposal_dist_pbly(par_i, par_temp, ADBM_par_sd))
        Kh_frac <- (Kh_temp/Kh_i)
        
        
        t_prior_frac[i] <- prior_frac
        t_prop_frac[i] <- prop_frac
        t_Kh_frac[i] <- Kh_frac
        
        qnty[i] <- Kh_frac*prior_frac*prop_frac
        # print(ADBM_par_sd)
        # print(proposal_dist_pbly(par_temp, par_i, ADBM_par_sd))
        x_chk <- runif(1)
        
        if(x_chk <= min(1,qnty[i]))
        {
          par_i <- par_temp
          Kh_i <- Kh_temp
          post_dists <- rbind(post_dists, par_temp)
          n_jump <- n_jump+1
          n_count[n_jump] <- 1
          i <- i+1
          
        }
        else{
          post_dists <- rbind(post_dists, par_i)
          n_count[n_jump] <- n_count[n_jump] + 1
          i <- i+1
        }
      }
      
    }
    
    list(acc_ss = qnty, dist = dist, post_dists = post_dists, total_sim = N, n_jump=n_jump, 
         n_count = n_count, t_prior_frac = t_prior_frac, t_prop_frac = t_prop_frac,
         t_Kh_frac = t_Kh_frac)
  }
  return(res_pcores)
}
