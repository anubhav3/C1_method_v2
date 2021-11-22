# implements rejection algorithm from Handbook of Approximate Bayesian Computation

rejection <- function(all.web.info, model, model_core_par,
                      model_prior_par, input_parameters, dist_ss,
                      prior_dist, weight_type)
{
  M <- all.web.info$species.sizes    #body size
  tol <- input_parameters$tol
  N <- input_parameters$N
  n_cores <- input_parameters$n_cores
  e <- model_core_par$e
  n <- model_core_par$n
  ni <- model_core_par$ni
  r.a = model_core_par$r.a

  registerDoParallel(cores=n_cores)
  
  ss_real <- model_core_par$real.web
  
  count <- 1
  post_dists <- data.frame(a=double(), ai=double(), aj=double(), r.b=double())
  n <- N/n_cores

  res_pcores <- foreach(i = 1:n_cores, .combine = rbind) %dopar% 
    {
      set.seed(i)
      acc_ss <- numeric(n)
      dist <- numeric(n)
      total_sim <- 0
      while(count <= n)
      {
        ## Generating parameters from sampling
        local_par <- prior_dist(par=model_prior_par, no=1)
        
        ## Generating summary stat from likelihood
        ss_sim <- model(opt=local_par, x=model_core_par)
        num_gen <- runif(1, 0, 1)
        K <- weights(0, tol, weight_type)
       
        dist_temp <- dist_ss(ss_sim = ss_sim, ss_real = ss_real)
       
        pbly <- weights(dist_temp, tol, weight_type)/K
        if (num_gen<pbly)
        {
          acc_ss[count] <- connectance_f(ss_sim = ss_sim)
          dist[count] <- dist_temp
          post_dists <- rbind(post_dists, local_par)
          count <- count + 1
        }
        total_sim <- total_sim+1
      }
      list(post_dists_pcore = post_dists, acc_ss_pcore = acc_ss, dist_pcore = dist, total_sim_pcore = total_sim)
    }

  post_dists <- res_pcores[[1]]
  acc_ss <- res_pcores[[n_cores+1]]
  dist <- res_pcores[[2*n_cores+1]]
  total_sim <- res_pcores[[3*n_cores+1]]
  
  if(n_cores > 1){
    for (i in 1:(n_cores-1)){
      post_dists <- rbind(post_dists, res_pcores[[i+1]])
      acc_ss <- c(acc_ss, res_pcores[[n_cores+i+1]])
      dist <- c(dist, res_pcores[[2*n_cores+i+1]])
      total_sim <- total_sim + res_pcores[[3*n_cores+i+1]]
    }
  }
  
  list(acc_ss = acc_ss, dist = dist, post_dists = post_dists, total_sim = total_sim)
  
}


