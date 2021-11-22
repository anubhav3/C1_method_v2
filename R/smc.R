# implements sequential monte carlo algorithm from pg 388 (Beaumont 2010 "Approximate Bayesian Computation in Evolution and Ecology")

smc <- function(all.web.info, model, model_core_par,
                model_prior_par, input_par, dist_ss,
                prior_dist_x, prior_prob, proposal_dist_x, 
                proposal_dist_pbly, eps_t, npar){
  
  M <- all.web.info$species.sizes    #body size
  tol <- input_par$tol
  N <- input_par$N
  n_cores <- input_par$n_cores
  T <- length(eps_t)
  
  registerDoParallel(cores=n_cores)
  
  ss_real <- model_core_par$real.web
  
  post_dists <- data.frame(a=double(), ai=double(), aj=double(), r.b=double())
  
  acc_ss <- numeric(N)
  dist <- numeric(N)
  qnty <- numeric(N)
  
  ## Initialise:
  par <- array(dim=c(N,npar,T))
  dist <- array(100, dim = c(T,N))
  w <- array(0, dim = c(N,T))
  
  # print(0)
  for(i in 1:N){
    while(dist[1,i] >= eps_t[1]){
      local_par <- prior_dist_x(par=model_prior_par, no=1)
      
      ss_sim <- model(opt=local_par, x=model_core_par)
      dist_temp <- dist_ss(ss_sim = ss_sim, ss_real = ss_real)
      

      dist[1,i] <- dist_temp
    }
    par[i,,1] <- c(local_par$a, local_par$ai, local_par$aj, local_par$r.b)
    w[i,1] <- 1/N
    
    # print(1)
  }
  count <- 1
  
  tau_sq <- array(dim=c(T,npar,npar))
  # for(i in 1:npar){
  #   tau_sq[1,i] <- 2*(sd(par[,,1][,i])^2)
  # }
  tau_sq[1,,] <- 2*cov(par[,,1])
  
  # print(2)
  ## Sampling
  for(t in 2:T)
  {
    # for(i in 1:npar){
    #   tau_sq[t,i] <- 2*w.var(par[,i,t-1], w[,i,t-1])
    # }
    # print(0.1)
    tau_sq[t,,] <- 2*cov.wt(x = par[,,t-1], wt = w[,t-1])$cov
    # print(0.2)
    
    for(i in 1:N)
    {
      # print(i)
      dist_temp <- 100
      par_star <- as.numeric(npar)
      while(dist_temp >= eps_t[t])
      {
        
        # for(par_pos in 1:npar){
        #   par_star[par_pos] <- sample(par[,par_pos,t-1], 1, replace = TRUE, prob = w[,par_pos,t-1])
        # }
        par_df <- data.frame(par[,,t-1])
        
        par_star <- sample_n(tbl = par_df, size = 1, replace = TRUE, weight = w[,t-1])
        
        # print(3)
        ## Proposal Distribution
        
        # par[i,1,t] <- rlnorm(1, meanlog = par_star[1], sdlog = sqrt(tau_sq[t,1]))
        # par[i,2,t] <- rnorm(1, mean = par_star[2], sd = sqrt(tau_sq[t,2]))
        # par[i,3,t] <- rnorm(1, mean = par_star[3], sd = sqrt(tau_sq[t,3]))
        # par[i,4,t] <- rlnorm(1, meanlog = par_star[4], sdlog = sqrt(tau_sq[t,4]))
        
        par[i,1:npar,t] <- proposal_dist_x(as.numeric(par_star), tau_sq[t,,])
        # print(4)
        local_par <- data.frame(a=par[i,1,t], ai=par[i,2,t], aj=par[i,3,t], r.b=par[i,4,t])
        
        ss_sim <- try(model(opt=local_par, x=model_core_par))
        if(is.numeric(ss_sim) == TRUE){
          dist_temp <- dist_ss(ss_sim = ss_sim, ss_real = ss_real)
        }
        
        else {
          dist_temp <- 99999
          
        }
        # print(dist_temp)
        # print(c('d',ss_sim))
      
        }
      dist[t,i] <- dist_temp
      
      
    }
    # print(5)
    # for(par_pos in 1:npar){
    #   w[,par_pos,t] <- prior_prob(par[,par_pos,t], par_pos, model_prior_par)/(sum(w[,par_pos,t-1]*proposal_dist_pbly(par[,par_pos,t]
    #                                                                                                             , par[,par_pos,t-1]
    #                                                                                                             , tau_sq[t,par_pos]
    #                                                                                                             , par_pos)))
    #   print(w[,par_pos,t])
    # }
    
    nor_const <- 0
    
    for(i_n in 1:N){
      nor_const <- nor_const + w[i_n,t-1]*proposal_dist_pbly(par[i_n,,t], par[i_n,,t-1], tau_sq[t,,])
      w[i_n,t] <- prior_prob(par[i_n,,t], model_prior_par)
      # print(w[i_n,t])
    }
    
    w[,t] <- w[,t]/nor_const
                                                                                                                   
                                                                                                                   
  }
  
  list(post_dists = par, w=w, tau_sq=tau_sq, dist=dist)
}








