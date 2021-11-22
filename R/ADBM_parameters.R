## input parameters for ADBM

ADBM_core_par = function(foodweb = foodweb){
  real.web = foodweb$predation.matrix
  S = dim(real.web)[1]
  return(list(e = 1,
                        n = 1,
                        ni = -3/4,
                        r.a = 1,
                        M = foodweb$species.sizes,
                        real.web = foodweb$predation.matrix,
                        S = S))
}

#From Bjorn Rall, range of ai from -1.5 to 1.5
#From Bjorn Rall, range of aj from 0 to 3
#range of r.b from -(num+1) to (num+1)

ADBM_prior_par = function(foodweb_ = foodweb){
  num <- range_prior_b(foodweb_$species.sizes)
  return(list(l_a = -3, r_a = 3,
              l_log_a = -10, r_log_a = 10,
                               l_ai = -1.5, r_ai = 1.5,
                               l_aj = 0, r_aj = 3,
                               l_r.b = -10, r_r.b = 10,
              l_log_r.b = -(num+1), r_log_r.b = (num+1),
                               a.shape = 0.4, a.scale = 1,
                               r.b.shape = 0.4, r.b.scale = 1))
}

ADBM_prior_par_sim = function(foodweb_ = foodweb){
  num <- range_prior_b(foodweb_$species.sizes)
  sim_par <- sim_foodweb_par()
  return(list(l_log_a = -10, r_log_a = 10,
              l_ai = sim_par$ai, r_ai = sim_par$ai,
              l_aj = sim_par$aj, r_aj = sim_par$aj,
              l_log_r.b = sim_par$r.b, r_log_r.b = sim_par$r.b))
}


ADBM_par_sd <- function(){
  return(data.frame(a = 3, ai = 0.5, aj = 0.5, r.b  = 1))
}


