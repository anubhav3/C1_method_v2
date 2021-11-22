# 20.11.2021
# We investigate the error why trying to obtain the mean trophic level and max trophic level

#### Consider Mill Stream

foodweb <- readRDS("results/rejection/Caricaie Lakes/rN=1000_tol=0.95_TSS_lower_a/Caricaie Lakes.RDS")
fw_data <- readRDS("data/Caricaie Lakes.web.RDS")
all.web.info <- fw_data

other <- ADBM_core_par(fw_data)

parameters <- foodweb$post_dists

M <- all.web.info$species.sizes
title <- all.web.info$web.name

for(k in 1:100){
  print(k)
  best.EHL1 <- Ratio.allometric.EHL(M=M,
                                    e=other$e,
                                    a=10^parameters$a[k], ai=parameters$ai[k], aj=parameters$aj[k],
                                    n=other$n, ni=other$ni,
                                    r.a=other$r.a, r.b=10^parameters$r.b[k])
  best.web1 <- Get.web(best.EHL1)
  community <- mat.to.comm(best.web1, title)
  
  mtl <- mean(ShortWeightedTrophicLevel(community))
}

