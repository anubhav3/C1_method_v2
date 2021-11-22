## Here, we define the prior distributions for the parameter

prior_unif_x <- function(par_ = par, no = no){
  local_a <- runif(no, par_$l_log_a, par_$r_log_a)
  local_ai <- runif(no, par_$l_ai, par_$r_ai)
  local_aj <- runif(no, par_$l_aj, par_$r_aj)
  local_r.b <- runif(no, par_$l_log_r.b, par_$r_log_r.b)
  
  return(data.frame(a=local_a, ai=local_ai, aj=local_aj, r.b=local_r.b))
}

prior_prob_joint_smc <- function(par, model_par)
{
  prob_a <- dunif(par[1], model_par$l_log_a, model_par$r_log_a)
  prob_ai <- dunif(par[2], model_par$l_ai, model_par$r_ai)
  prob_aj <- dunif(par[3], model_par$l_aj, model_par$r_aj)
  prob_r.b <- dunif(par[4], model_par$l_log_r.b, model_par$r_log_r.b)
  
  pbly <- prob_a*prob_ai*prob_aj*prob_r.b
  
  return(pbly)
}

prior_pbly_unif <- function(par = par, model_par = model_par){
  pbly_a <- dunif(par$a, model_par$l_log_a, model_par$r_log_a)
  pbly_ai <- dunif(par$ai, model_par$l_ai, model_par$r_ai)
  pbly_aj <- dunif(par$aj, model_par$l_aj, model_par$r_aj)
  pbly_r.b <- dunif(par$r.b, model_par$l_log_r.b, model_par$r_log_r.b)
  
  return(pbly_a*pbly_ai*pbly_aj*pbly_r.b)
}