#21.10.2020
#The proportion of basal, intermediate and top species have been computed using a different function

fw_prop_v2 <- function(foodweb, other, all.web.info){
  parameters <- foodweb$post_dists
  M <- all.web.info$species.sizes
  title <- all.web.info$web.name
  # fw_prop <- data.frame(prop_basal = double(), prop_inter = double(), prop_top = double(),
  #                       prop_herb = double(), mean_trop_lvl = double(), max_trop_lvl = double(),
  #                       mean_omn = double(), clus_coeff = double(), sd_gen = double(),
  #                       sd_vulner = double(), diet_sim = double(), mean_path_lt = double(),
  #                       prop.1.corr = double(), prop.0.corr = double(), nest_prop = double())
  
  best.EHL1 <- Ratio.allometric.EHL(M=M,
                                    e=other$e,
                                    a=10^parameters$a[1], ai=parameters$ai[1], aj=parameters$aj[1],
                                    n=other$n, ni=other$ni,
                                    r.a=other$r.a, r.b=10^parameters$r.b[1])
  best.web1 <- Get.web(best.EHL1)
  community <- mat.to.comm(best.web1, title)
  
  prop.1.corr <- round(sum(best.web1==1 & all.web.info$predation.matrix==1)/sum(all.web.info$predation.matrix), 2)
  prop.0.corr <- round(sum(best.web1==0 & all.web.info$predation.matrix==0)/
                         (dim(all.web.info$predation.matrix)[1]^2-sum(all.web.info$predation.matrix)), 2)
  
  connectance <- sum(best.web1)/(dim(best.web1)[1]^2)
  prop_species <- prop_basal_inter_top_herb(pred_mat = best.web1)
  prop_basal <- prop_species$prop_basal
  prop_inter <- prop_species$prop_inter
  prop_top <- prop_species$prop_top
  n_singleton <- prop_species$n_singleton
  # without_basal <- RemoveNodes(community, BasalNodes(community))
  prop_herb <- prop_species$prop_herb
  
  # mtl <- try(mean(ShortWeightedTrophicLevel(community)))
  # mean_trop_lvl <- if(is.numeric(mtl)==TRUE) mtl else -999
  # maxtl <- try(max(LongestTrophicLevel(community)))
  # max_trop_lvl <- if(is.numeric(maxtl)==TRUE) maxtl else -999
  
  mean_trop_lvl <- -999
  max_trop_lvl <- -999
  
  mean_omn <- ifelse(is.na(as.numeric(PreyAveragedTrophicLevel(community)))[1] == FALSE, FractionOmnivorous(community), NA)
  clus_coeff <- ClustF(best.web1, type = "directed")$GlobaltotalCC
  sd_gen <- sd(InDegree(community))
  sd_vulner <- sd(OutDegree(community))
  ACC <- 1-dist_ACC(ss_sim = best.web1, ss_real = all.web.info$predation.matrix)
  TSS <- TSS_func(ss_sim = best.web1, ss_real = all.web.info$predation.matrix)
  
  # community_without_diag <- TrophicSimilarity(community)
  # diag(community_without_diag) <- -999
  diet_sim <- MeanMaximumTrophicSimilarity(community)
  
  mean_path_lt <- mean(ShortestPaths(community))
  
  nest_prop <- as.numeric(nested(web = best.web1)) #the default method is binmatnest2
  
  
  fw_prop <- data.frame(connectance= connectance, prop_basal = prop_basal, prop_inter = prop_inter, prop_top = prop_top,
                        prop_herb = prop_herb, mean_trop_lvl = mean_trop_lvl, max_trop_lvl = max_trop_lvl,
                        mean_omn = mean_omn, clus_coeff = clus_coeff, sd_gen = sd_gen,
                        sd_vulner = sd_vulner, diet_sim = diet_sim, mean_path_lt = mean_path_lt,
                        prop.1.corr = prop.1.corr, prop.0.corr = prop.0.corr, ACC=ACC, TSS=TSS,
                        n_singleton = n_singleton, nest_prop = nest_prop)
  
  fw_temp <- fw_prop
  best.web2 <- best.web1
  
  N <- dim(parameters)[1]
  if(N>1){
    for(i in 2:N)
    {
      if(sum(parameters[i,] == parameters[i-1,])==4){
        fw_prop <- rbind(fw_prop, fw_temp)
        best.web1 <- best.web1 + best.web2
      }
      
      else{
        best.EHL1 <- Ratio.allometric.EHL(M=M,
                                          e=other$e,
                                          a=10^parameters$a[i], ai=parameters$ai[i], aj=parameters$aj[i],
                                          n=other$n, ni=other$ni,
                                          r.a=other$r.a, r.b=10^parameters$r.b[i])
        best.web2 <- Get.web(best.EHL1)
        
        prop.1.corr <- round(sum(best.web2==1 & all.web.info$predation.matrix==1)/sum(all.web.info$predation.matrix), 2)
        prop.0.corr <- round(sum(best.web2==0 & all.web.info$predation.matrix==0)/
                               (dim(all.web.info$predation.matrix)[1]^2-sum(all.web.info$predation.matrix)), 2)
        community <- mat.to.comm(best.web2, title)
        
        connectance <- sum(best.web2)/(dim(best.web2)[1]^2)
        prop_species <- prop_basal_inter_top_herb(pred_mat = best.web2)
        prop_basal <- prop_species$prop_basal
        prop_inter <- prop_species$prop_inter
        prop_top <- prop_species$prop_top
        n_singleton <- prop_species$n_singleton
        # without_basal <- RemoveNodes(community, BasalNodes(community))
        prop_herb <- prop_species$prop_herb
        
        # mtl <- try(mean(ShortWeightedTrophicLevel(community)))
        # mean_trop_lvl <- if(is.numeric(mtl)==TRUE) mtl else -999
        # maxtl <- try(max(LongestTrophicLevel(community)))
        # max_trop_lvl <- if(is.numeric(maxtl)==TRUE) maxtl else -999
        
        mean_trop_lvl <- -999
        max_trop_lvl <- -999
        
        mean_omn <- ifelse(is.na(as.numeric(PreyAveragedTrophicLevel(community)))[1] == FALSE, FractionOmnivorous(community), NA)
        clus_coeff <- ClustF(best.web2, type = "directed")$GlobaltotalCC
        sd_gen <- sd(InDegree(community))
        sd_vulner <- sd(OutDegree(community))
        ACC <- 1-dist_ACC(ss_sim = best.web2, ss_real = all.web.info$predation.matrix)
        TSS <- TSS_func(ss_sim = best.web2, ss_real = all.web.info$predation.matrix)
        
        # community_without_diag <- TrophicSimilarity(community)
        # diag(community_without_diag) <- -999
        diet_sim <- MeanMaximumTrophicSimilarity(community)
        
        mean_path_lt <- mean(ShortestPaths(community))
        nest_prop <- as.numeric(nested(web = best.web2))
        
        fw_temp <- data.frame(connectance = connectance, prop_basal = prop_basal, prop_inter = prop_inter, prop_top = prop_top,
                              prop_herb = prop_herb, mean_trop_lvl = mean_trop_lvl, max_trop_lvl = max_trop_lvl,
                              mean_omn = mean_omn, clus_coeff = clus_coeff, sd_gen = sd_gen,
                              sd_vulner = sd_vulner, diet_sim = diet_sim, mean_path_lt = mean_path_lt,
                              prop.1.corr = prop.1.corr, prop.0.corr = prop.0.corr, ACC = ACC, TSS = TSS,
                              n_singleton = n_singleton, nest_prop = nest_prop)
        
        fw_prop <- rbind(fw_prop, fw_temp)
        best.web1 <- best.web1 + best.web2
        
      }
      
      
    }
    fw_prop <- list(prop = fw_prop, web = best.web1)
  }
  return(fw_prop)
}


real_prop_v2 <- function(all.web.info){
  
  title <- all.web.info$web.name
  community <- mat.to.comm(all.web.info$predation.matrix, title)
  connectance <- sum(all.web.info$predation.matrix)/(dim(all.web.info$predation.matrix)[1]^2)
  prop_species <- prop_basal_inter_top_herb(pred_mat = all.web.info$predation.matrix)
  prop_basal <- prop_species$prop_basal
  prop_inter <- prop_species$prop_inter
  prop_top <- prop_species$prop_top
  n_singleton <- prop_species$n_singleton
  # without_basal <- RemoveNodes(community, BasalNodes(community))
  prop_herb <- prop_species$prop_herb
  mean_trop_lvl <- -999
  # mtl <- try(mean(ShortWeightedTrophicLevel(community)))
  # mean_trop_lvl <- if(is.numeric(mtl)==TRUE) mtl else -999
  max_trop_lvl <- -999
  # maxtl <- try(max(LongestTrophicLevel(community)))
  # max_trop_lvl <- if(is.numeric(maxtl)==TRUE) maxtl else -999
  mean_omn <- FractionOmnivorous(community)
  clus_coeff <- ClustF(all.web.info$predation.matrix, type = "directed")$GlobaltotalCC
  # clus_temp <- try(ClustF(all.web.info$predation.matrix, type = "directed"))
  # clus_coeff <- if(is.numeric(clus_temp) == TRUE) clus_temp$GlobaltotalCC else -999
  sd_gen <- sd(InDegree(community))
  sd_vulner <- sd(OutDegree(community))
  
  community_without_diag <- TrophicSimilarity(community)
  diag(community_without_diag) <- -999
  diet_sim <- max(community_without_diag)
  
  mean_path_lt <- mean(ShortestPaths(community))
  nest_prop <- as.numeric(nested(all.web.info$predation.matrix))
  
  real_prop_f <- data.frame(connectance = connectance, prop_basal = prop_basal, prop_inter = prop_inter, prop_top = prop_top,
                            prop_herb = prop_herb, mean_trop_lvl = mean_trop_lvl, max_trop_lvl = max_trop_lvl,
                            mean_omn = mean_omn, clus_coeff = clus_coeff, sd_gen = sd_gen,
                            sd_vulner = sd_vulner, diet_sim = diet_sim, mean_path_lt = mean_path_lt,
                            n_singleton = n_singleton, nest_prop = nest_prop)
  
  return(real_prop_f)
}



fw_prop_test <- function(foodweb, other, all.web.info){
  parameters <- foodweb$post_dists
  M <- all.web.info$species.sizes
  title <- all.web.info$web.name
  # fw_prop <- data.frame(prop_basal = double(), prop_inter = double(), prop_top = double(),
  #                       prop_herb = double(), mean_trop_lvl = double(), max_trop_lvl = double(),
  #                       mean_omn = double(), clus_coeff = double(), sd_gen = double(),
  #                       sd_vulner = double(), diet_sim = double(), mean_path_lt = double(),
  #                       prop.1.corr = double(), prop.0.corr = double(), nest_prop = double())
  
  best.EHL1 <- Ratio.allometric.EHL(M=M,
                                    e=other$e,
                                    a=10^parameters$a[1], ai=parameters$ai[1], aj=parameters$aj[1],
                                    n=other$n, ni=other$ni,
                                    r.a=other$r.a, r.b=10^parameters$r.b[1])
  best.web1 <- Get.web(best.EHL1)
  community <- mat.to.comm(best.web1, title)
  
  prop.1.corr <- round(sum(best.web1==1 & all.web.info$predation.matrix==1)/sum(all.web.info$predation.matrix), 2)
  prop.0.corr <- round(sum(best.web1==0 & all.web.info$predation.matrix==0)/
                         (dim(all.web.info$predation.matrix)[1]^2-sum(all.web.info$predation.matrix)), 2)
  
  connectance <- sum(best.web1)/(dim(best.web1)[1]^2)
  prop_species <- prop_basal_inter_top_herb(pred_mat = best.web1)
  prop_basal <- prop_species$prop_basal
  prop_inter <- prop_species$prop_inter
  prop_top <- prop_species$prop_top
  n_singleton <- prop_species$n_singleton
  # without_basal <- RemoveNodes(community, BasalNodes(community))
  prop_herb <- prop_species$prop_herb
  
  mtl <- try(mean(ShortWeightedTrophicLevel(community)))
  mean_trop_lvl <- if(is.numeric(mtl)==TRUE) mtl else -999
  maxtl <- try(max(LongestTrophicLevel(community)))
  max_trop_lvl <- if(is.numeric(maxtl)==TRUE) maxtl else -999
  
  # mean_trop_lvl <- -999
  # max_trop_lvl <- -999
  
  mean_omn <- ifelse(is.na(as.numeric(PreyAveragedTrophicLevel(community)))[1] == FALSE, FractionOmnivorous(community), NA)
  clus_coeff <- ClustF(best.web1, type = "directed")$GlobaltotalCC
  sd_gen <- sd(InDegree(community))
  sd_vulner <- sd(OutDegree(community))
  ACC <- 1-dist_ACC(ss_sim = best.web1, ss_real = all.web.info$predation.matrix)
  TSS <- TSS_func(ss_sim = best.web1, ss_real = all.web.info$predation.matrix)
  
  # community_without_diag <- TrophicSimilarity(community)
  # diag(community_without_diag) <- -999
  diet_sim <- MeanMaximumTrophicSimilarity(community)
  
  mean_path_lt <- mean(ShortestPaths(community))
  
  nest_prop <- as.numeric(nested(web = best.web1)) #the default method is binmatnest2
  
  
  fw_prop <- data.frame(connectance= connectance, prop_basal = prop_basal, prop_inter = prop_inter, prop_top = prop_top,
                        prop_herb = prop_herb, mean_trop_lvl = mean_trop_lvl, max_trop_lvl = max_trop_lvl,
                        mean_omn = mean_omn, clus_coeff = clus_coeff, sd_gen = sd_gen,
                        sd_vulner = sd_vulner, diet_sim = diet_sim, mean_path_lt = mean_path_lt,
                        prop.1.corr = prop.1.corr, prop.0.corr = prop.0.corr, ACC=ACC, TSS=TSS,
                        n_singleton = n_singleton, nest_prop = nest_prop)
  
  fw_temp <- fw_prop
  best.web2 <- best.web1
  
  N <- dim(parameters)[1]
  if(N>1){
    for(i in 2:N)
    {
      if(sum(parameters[i,] == parameters[i-1,])==4){
        fw_prop <- rbind(fw_prop, fw_temp)
        best.web1 <- best.web1 + best.web2
      }
      
      else{
        best.EHL1 <- Ratio.allometric.EHL(M=M,
                                          e=other$e,
                                          a=10^parameters$a[i], ai=parameters$ai[i], aj=parameters$aj[i],
                                          n=other$n, ni=other$ni,
                                          r.a=other$r.a, r.b=10^parameters$r.b[i])
        best.web2 <- Get.web(best.EHL1)
        
        prop.1.corr <- round(sum(best.web2==1 & all.web.info$predation.matrix==1)/sum(all.web.info$predation.matrix), 2)
        prop.0.corr <- round(sum(best.web2==0 & all.web.info$predation.matrix==0)/
                               (dim(all.web.info$predation.matrix)[1]^2-sum(all.web.info$predation.matrix)), 2)
        community <- mat.to.comm(best.web2, title)
        
        connectance <- sum(best.web2)/(dim(best.web2)[1]^2)
        prop_species <- prop_basal_inter_top_herb(pred_mat = best.web2)
        prop_basal <- prop_species$prop_basal
        prop_inter <- prop_species$prop_inter
        prop_top <- prop_species$prop_top
        n_singleton <- prop_species$n_singleton
        # without_basal <- RemoveNodes(community, BasalNodes(community))
        prop_herb <- prop_species$prop_herb
        
        mtl <- try(mean(ShortWeightedTrophicLevel(community)))
        mean_trop_lvl <- if(is.numeric(mtl)==TRUE) mtl else -999
        maxtl <- try(max(LongestTrophicLevel(community)))
        max_trop_lvl <- if(is.numeric(maxtl)==TRUE) maxtl else -999
        
        # mean_trop_lvl <- -999
        # max_trop_lvl <- -999
        
        mean_omn <- ifelse(is.na(as.numeric(PreyAveragedTrophicLevel(community)))[1] == FALSE, FractionOmnivorous(community), NA)
        clus_coeff <- ClustF(best.web2, type = "directed")$GlobaltotalCC
        sd_gen <- sd(InDegree(community))
        sd_vulner <- sd(OutDegree(community))
        ACC <- 1-dist_ACC(ss_sim = best.web2, ss_real = all.web.info$predation.matrix)
        TSS <- TSS_func(ss_sim = best.web2, ss_real = all.web.info$predation.matrix)
        
        # community_without_diag <- TrophicSimilarity(community)
        # diag(community_without_diag) <- -999
        diet_sim <- MeanMaximumTrophicSimilarity(community)
        
        mean_path_lt <- mean(ShortestPaths(community))
        nest_prop <- as.numeric(nested(web = best.web2))
        
        fw_temp <- data.frame(connectance = connectance, prop_basal = prop_basal, prop_inter = prop_inter, prop_top = prop_top,
                              prop_herb = prop_herb, mean_trop_lvl = mean_trop_lvl, max_trop_lvl = max_trop_lvl,
                              mean_omn = mean_omn, clus_coeff = clus_coeff, sd_gen = sd_gen,
                              sd_vulner = sd_vulner, diet_sim = diet_sim, mean_path_lt = mean_path_lt,
                              prop.1.corr = prop.1.corr, prop.0.corr = prop.0.corr, ACC = ACC, TSS = TSS,
                              n_singleton = n_singleton, nest_prop = nest_prop)
        
        fw_prop <- rbind(fw_prop, fw_temp)
        best.web1 <- best.web1 + best.web2
        
      }
      
      
    }
    fw_prop <- list(prop = fw_prop, web = best.web1)
  }
  return(fw_prop)
}