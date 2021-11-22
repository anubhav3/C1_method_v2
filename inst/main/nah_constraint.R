
fw_M <- readRDS("data/Benguela Pelagic.web.RDS")$species.sizes
fw_e <- 10
fw_r.a <- 10
fw_n <- 1/10
fw_ni <- -0.75

fw_par <- readRDS("results/rejection/Benguela Pelagic/rN=1000_tol=0.6_TSS_lower_a/Benguela Pelagic.RDS")$post_dists[1,]

fw_a <- 4.15053420
fw_ai <- -0.88414700
fw_aj <- -0.93833617
fw_r.b <- 0.02707676

fw_EHL <- Ratio.allometric.EHL(M = fw_M, e = fw_e, r.a = fw_r.a, r.b = fw_r.b, a = fw_a, ai = fw_ai, aj = fw_aj, n = fw_n, ni = fw_ni)

EHL <- fw_EHL

S <- length(EHL[[1]])

web <- matrix(0, S, S)
overall.energy <- numeric(S)
per.species.energy <- matrix(0, S, S)

P <- EHL[[1]]/EHL[[2]]

j <- 20
# for(j in 1:S){
  
  p <- P[,j]
  
  if(sum(p>0)==1)
    web[which(p>0),j] <- 1
  
  if(sum(p>0)>1){
    
    ## ordering of p required
    
    order.by.p <- order(p, decreasing=T)
    p <- p[order.by.p]
    Lj <- EHL[[3]][,j][order.by.p]
    hj <- EHL[[2]][,j][order.by.p]
    Ej <- EHL[[1]][order.by.p]
    
    cumulative.profit <- cumsum(Ej * Lj) / (1 + cumsum( Lj * hj))
    
    dj <- max(which(cumulative.profit==max(cumulative.profit)))
    
    web[,j] <- c(rep(1, dj), rep(0, S-dj))[order(order.by.p)]
    
    overall.energy[j] <- cumulative.profit[dj]
    
    energies <- c(Ej * Lj)[1:sum(web[,j])] / (1 + cumsum( Lj * hj)[sum(web[,j])]) 
    all.energies <- c(energies, rep(0, S-length(energies)))
    
    per.species.energy[,j] <- all.energies[order(order.by.p)]
    
  }
# }

plot(cumulative.profit)