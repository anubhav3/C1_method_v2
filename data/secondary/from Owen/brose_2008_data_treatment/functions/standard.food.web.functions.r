
## Get structural stability
Structural.stability <- function(sc.or.web){
    if(is.vector(sc.or.web)){
        m <- sc.or.web[1]
        c <- sc.or.web[2]
        web <- rep(1, c*m^2)
        web <- c(web, rep(0, m^2-length(web)))
        web <- matrix(web[order(rnorm(length(web)))],
                        m, m)
        diag(web) <- -1
        stab <- max(Re(eigen(web)$values))
    }
    if(is.matrix(sc.or.web)){
        web <- ifelse(sc.or.web==0, 0, 1)
        diag(web) <- -1
        stab <- max(Re(eigen(web)$values))
    }
    stab
}

                                                                  
Random.model <- function(S, L, N=1){
    if(N==1)
        web <- matrix(c(rep(1, L),
                         rep(0, S^2-L))[order(runif(S^2))],
                      S, S)
    if(N>1){
        web <- list()
        for(i in 1:N){
            web[[i]] <- matrix(c(rep(1, L),
                            rep(0, S^2-L))[order(runif(S^2))],
                               S, S)
        }
    }
    web
}

Cascade.model <- function(S, L, N=1){
    if(N==1){
        web <- matrix(0, S, S)
        web[upper.tri(web)] <- c(rep(1, L), rep(0, (S^2-S)/2-L))[order(runif((S^2-S)/2))]
        dimnames(web) <- list(1:length(web[,1]), 1:length(web[,1]))
    }
    if(N>1){
        web <- list()
        for(i in 1:N){
            web[[i]] <- matrix(0, S, S)
            web[[i]][upper.tri(web[[i]])] <- c(rep(1, L), rep(0, (S^2-S)/2-L))[order(runif((S^2-S)/2))]
            dimnames(web[[i]]) <- list(1:length(web[[i]][,1]), 1:length(web[[i]][,1]))
        }
    }
    web
}

## Niche model of Williams and Martinez
Niche.model <- function(S, L, N=1){
  C <- L/S^2
  if(N==1){
      flag=F
      while(flag==F) {
          n <- sort(runif(S))
          beta <- (1 - 2 * C) / (2 * C)
          r <- n*(1 - (1 - runif(S))^(1/beta))
          c <- r/2 + runif(S) * (n - r/2)
          web <- matrix(0,S,S)
          min.n <- c-r/2
          max.n <- c+r/2
          for(i in 1:S){
              diet <- c(1:S)[c(which(n>min.n[i]), which(n<max.n[i]))[duplicated(c(which(n>min.n[i]), which(n<max.n[i])))]]
              web[diet,i] <- 1
          }
          if( abs(sum(web)/S^2 - L/S^2) / (L/S^2) <= 0.03) 
              flag=T
      }
      
      dimnames(web) <- list(1:length(web[,1]), 1:length(web[,1]))
  }
  if(N>1){
      web <- list()
      for(j in 1:N){
          flag=F
          while(flag==F) {
              
              n <- sort(runif(S))
              beta <- (1 - 2 * C) / (2 * C)
              r <- n*(1 - (1 - runif(S))^(1/beta))
              c <- r/2 + runif(S) * (n - r/2)
              web[[j]] <- matrix(0,S,S)
              min.n <- c-r/2
              max.n <- c+r/2
              for(i in 1:S){
                  diet <- c(1:S)[c(which(n>min.n[i]), which(n<max.n[i]))[duplicated(c(which(n>min.n[i]), which(n<max.n[i])))]]
                  web[[j]][diet,i] <- 1
          }
          if( abs(sum(web[[j]])/S^2 - L/S^2) / (L/S^2) <= 0.03) 
              flag=T
          }
          dimnames(web[[j]]) <- list(1:length(web[[j]][,1]), 1:length(web[[j]][,1]))
      }
  }
  web
}


CS.Stouffer <- function(S, C){
  L <- C*S*S
  prey <- 1:S
  pred <- 1:S
  z <- L/S
  scaled.prey <- prey/2/z
  scaled.pred <- pred/2/z
  cum.p.scaled.prey <- exp(-scaled.prey)-scaled.prey*expint_E1(scaled.prey)
  cum.p.scaled.pred <- 1 - 1/2/z*cumsum( gamma_inc_P(2*z*scaled.pred, 2*z))
  
  cbind(number=0:S/z/2,
        prey=c(1, cum.p.scaled.prey),
        pred=c(1, cum.p.scaled.pred))
}

RCN.performance <- function(real.web, NITS=10, stats=1:3){

    S <- dim(real.web)[1]
    L <- sum(real.web)
    
    real.stats <- Get.web.stats(real.web, stats)
    number.of.stats <- length(real.stats)+1
    stat.names <- c(names(real.stats), "prop.corr")
    
    R.webs <- list()
    C.webs <- list()
    N.webs <- list()

    R.trad.stats <- matrix(0, NITS, number.of.stats)
    C.trad.stats <- matrix(0, NITS, number.of.stats)
    N.trad.stats <- matrix(0, NITS, number.of.stats)
    dimnames(R.trad.stats) <- list(1:NITS, stat.names)
    dimnames(C.trad.stats) <- list(1:NITS, stat.names)
    dimnames(N.trad.stats) <- list(1:NITS, stat.names)

    #max.random.perf = 0
    #max.cascade.perf = 0
    #max.niche.perf = 0
        
    for(i in 1:NITS){
        
        R.webs[[i]] <- matrix(c(rep(1, L),
                               rep(0, S^2-L))[order(runif(S^2))],
                             S, S)
        C.webs[[i]] <- Cascade.model(S, L)
        N.webs[[i]] <- Niche.model(S, L)
        
        R.trad.stats[i,] <- c(Get.web.stats(R.webs[[i]], stats), Compare.links(real.web, R.webs[[i]]))
        C.trad.stats[i,] <- c(Get.web.stats(C.webs[[i]], stats), Compare.links(real.web, C.webs[[i]]))
        N.trad.stats[i,] <- c(Get.web.stats(N.webs[[i]], stats), Compare.links(real.web, N.webs[[i]]))

    }
    
    results <- list(webs=list(Random.webs=R.webs,
                              Cascade.webs=C.webs,
                              Niche.webs=N.webs),
                    stats=list(Random.stats=R.trad.stats,
                               Cascade.stats=C.trad.stats,
                               Niche.stats=N.trad.stats))
    results
}


## If 'what' is a fraction then the web is aggregated so that the new species richness
## is this proportion of the old
## If 'what is a list' the web is aggregated according to this
Aggregate.web <- function(web, how.much, how, linkage.amount="maximum"){

  if(class(how.much)=="numeric" & how=="random"){
    proportion=how.much
    old.s <- length(web[,1])
    old.species <- 1:old.s
    new.s <- round(old.s*proportion)
    number.per.new.s <- rep(1, new.s)
    remaining <- old.s-new.s
    these <- sample(new.s, remaining, replace=T)
    for(i in 1:length(these))
      number.per.new.s[these[i]] <- number.per.new.s[these[i]] + 1
    agg <- list(sample(old.species, number.per.new.s[1], replace=F))
    old.species <- old.species[is.na(match(old.species, agg[[1]]))]
    for(i in 2:(new.s-1)){
      agg[[i]] <- sample(old.species, number.per.new.s[i], replace=F)
      old.species <- old.species[is.na(match(old.species, agg[[i]]))]
    }
    agg[[new.s]] <- old.species
  }

  if(class(how.much)=="numeric" & how=="jaccard"){
    proportion=how.much
    old.s <- length(web[,1])
    old.species <- 1:old.s
    new.s <- round(old.s*proportion)
    number.per.new.s <- rep(1, new.s)
    ##ct <- cutree(hclust(dist(cbind(web, t(web)))), k=new.s)
    ct <- cutree(hclust(1-vegdist(cbind(web, t(web)), method="jaccard", bin=T), method="average"), k=new.s)
    agg <- list()
    for(i in 1:max(ct))
      agg[[i]] <- (1:old.s)[ct==i]
  }

  if(class(how.much)=="numeric" & how=="crap"){
    proportion=how.much
    old.s <- length(web[,1])
    old.species <- 1:old.s
    new.s <- round(old.s*proportion)
    number.per.new.s <- rep(1, new.s)
    ct <- cutree(hclust(dist(web)), k=new.s)
    ##ct <- cutree(hclust(1-vegdist(cbind(web, t(web)), method="jaccard", bin=T), method="average"), k=new.s)
    agg <- list()
    for(i in 1:max(ct))
      agg[[i]] <- (1:old.s)[ct==i]
  }
  
  if(class(how.much)=="list")
    agg=how.much

  new.web <- matrix(0, length(agg), length(agg))
  for(i in 1:length(agg))
    for(j in 1:length(agg))
      new.web[i,j] <- sum(web[agg[[i]],agg[[j]]])/length(web[agg[[i]],agg[[j]]])
  if(linkage.amount=="maximum")
    new.web <- matrix(as.numeric(new.web>0), length(new.web[,1]), length(new.web[,1]))
  if(linkage.amount=="minimum")
    new.web <- matrix(as.numeric(new.web==1), length(new.web[,1]), length(new.web[,1]))
  if(is.numeric(linkage.amount))
    new.web <- matrix(as.numeric(new.web>linkage.amount), length(new.web[,1]), length(new.web[,1]))
  list(new.web, agg)
}

Sort.BIT <- function(web){
  species.names=dimnames(web)[[1]]
  dimnames(web)=list(1:length(web[,1]), 1:length(web[,1]))
  BIT=Bottom.Intermediate.Top(web)
  web=web[as.numeric(c(BIT$Bottom, BIT$Intermediate, BIT$Top)),as.numeric(c(BIT$Bottom, BIT$Intermediate, BIT$Top))]
  dimnames(web)=list(species.names[as.numeric(c(BIT$Bottom, BIT$Intermediate, BIT$Top))], species.names[as.numeric(c(BIT$Bottom, BIT$Intermediate, BIT$Top))])
  web
}

Plot.matrix <- function(web, title=" ", point.cex=0.5, trait.cex=1,
                        diag.line=T, traits=F, by.consumer=T, axes.labels=F, sp.pt.ch=NA, pt.col="black"){
    
  S <- length(web[,1])

  ##point.cex <- 30/30
  ##trait.cex <- 30/30
  
  dimnames(web) <- list(1:S, 1:S)
  consumer <- rep(1:S, each=S)
  resource <- rep(1:S, S)
  web.list <- Matrix.to.list(web)
  par(xpd=T)
  plot(consumer, resource, pch=19, type="n", cex=0.1,
       ann=F, axes=F,
       xlim=c(1, S), ylim=c(1, S))
  if(length(traits)==1)
    points(web.list[,2], S+1-as.numeric(web.list[,1]),
           type="p", pch=19, cex=point.cex, col=pt.col)
  if(length(traits)==length(web)){

    colours.to.use <- rev(heat.colors(30)[c(-1:-5, -26:-30)])
    ##colours.to.use <- rev(gray(1:30/30)[c(-1:-5, -26:-30)])
    
    if(by.consumer){
      integer.traits <- matrix(0, S, S)
      for(i in 1:S){
        traits.01 <- traits[,i]-min(traits[,i])
        traits.01 <- traits.01/max(traits.01)
        integer.traits[,i] <- round(traits.01*19)+1
        integer.traits[traits[,i]==0,i] = NaN
        
      }
    }
    
    if(!by.consumer){
      colours.to.use <- heat.colors(20)
      traits.01 <- traits-min(traits)
      traits.01 <- traits.01/max(traits.01)
      integer.traits <- round(traits.01*19)+1
    }

    if(point.cex>trait.cex){
        points(web.list[,2], S+1-as.numeric(web.list[,1]),
               type="p", pch=19, cex=point.cex, col="black")##colours.to.use[integer.traits])
        points(rep(1:S, each=S), rep(S:1, times=S), 
               pch=19, cex=trait.cex, col=colours.to.use[integer.traits])
    }
    if(point.cex<trait.cex){
        points(rep(1:S, each=S), rep(S:1, times=S), 
               pch=19, cex=trait.cex, col=colours.to.use[integer.traits])
        points(web.list[,2], S+1-as.numeric(web.list[,1]),
               type="p", pch=19, cex=point.cex, col="black")##colours.to.use[integer.traits])
    }

    if(!is.na(sp.pt.ch))
        points(web.list[,2], S+1-as.numeric(web.list[,1]),
               type="p", pch=sp.pt.ch, cex=point.cex, col="black")##colours.to.use[integer.traits])
        
    
  }
  par(xpd=F)
  mtext(side=3, text=title, font=2, line=2, cex=0.5)
  if(axes.labels){
      mtext(side=2, "Resource", line=0.5, cex=1)
      mtext(side=3, "Consumer", line=0.5, cex=1)
  }
  if(diag.line==T)
    lines(1:S, S:1, lty="dashed")
}
  
Stouffer.plots <- function(web, scaled=F){
  library(Hmisc)
  number.of.prey <- colSums(web)
  number.of.predators <- rowSums(web)
  number.of.links <- number.of.prey + number.of.predators
  if(!scaled){
    layout(matrix(c(1:3), 1, 3))
    par(cex=1.2)
    x <- ecdf(number.of.prey, pl=F)
    plot(x$x[-1], (1-x$y)[-length(x$y)],
         xlab="Number of prey", ylab="Cumulative distribution",
         xlim=c(0,max(number.of.prey)))
    x <- ecdf(number.of.predators, pl=F)
    plot(x$x[-1], (1-x$y)[-length(x$y)],
         xlab="Number of predators", ylab="Cumulative distribution",
         xlim=c(0,max(number.of.predators)))
    x <- ecdf(number.of.links, pl=F)
    plot(x$x[-1], (1-x$y)[-length(x$y)],
         xlab="Number of links", ylab="Cumulative distribution",
         xlim=c(0,max(number.of.links)))
  }
  if(scaled){
    z <- sum(web)/s
    scaled.number.of.prey <- number.of.prey/(2*z)
    scaled.number.of.predators <- number.of.predators/(2*z)
    scaled.number.of.links <- number.of.links/(2*z)
    layout(matrix(c(1:3), 1, 3))
    par(cex=1.2)
    x <- ecdf(scaled.number.of.prey, pl=F)
    plot(x$x[-1], (1-x$y)[-length(x$y)],
         xlab="Scaled number of prey", ylab="Cumulative distribution",
         xlim=c(0,max(scaled.number.of.prey)))
    x <- ecdf(scaled.number.of.predators, pl=F)
    plot(x$x[-1], (1-x$y)[-length(x$y)],
         xlab="Scaled number of predators", ylab="Cumulative distribution",
         xlim=c(0,max(scaled.number.of.predators)))
    x <- ecdf(scaled.number.of.links, pl=F)
    plot(x$x[-1], (1-x$y)[-length(x$y)],
         xlab="Scaled number of links", ylab="Cumulative distribution",
         xlim=c(0,max(scaled.number.of.links)))
  }
}

Ecdf.niche.model <- function(S, L, nits=1){
  
  ecdf.prey <- matrix(NA, S+1, nits)
  ecdf.predators <- matrix(NA, S+1, nits)
  ecdf.links <- matrix(NA, S*2+1, nits)
  
  realised.C <- 0

  C <- L/S^2
  
  for(j in 1:nits){

    web <- Niche.model(S, L)

    ##Plot.matrix(web)
    realised.C <- realised.C + sum(web)/S^2/nits
    x <- ecdf(colSums(web), what="1-F", pl=F)
    ecdf.prey[c(x$x+1)[-1], j] <- x$y[-1]
    x <- ecdf(rowSums(web), what="1-F", pl=F)
    ecdf.predators[c(x$x+1)[-1], j] <- x$y[-1]
    x <- ecdf(colSums(web)+rowSums(web), what="1-F", pl=F)
    ecdf.links[c(x$x+1)[-1], j] <- x$y[-1]
  }
  
  if(nits==1){
    ecdf.prey <- ecdf.prey[1:max(which(!is.na(ecdf.prey))),]
    ecdf.predators <- ecdf.predators[1:max(which(!is.na(ecdf.predators))),]
    ecdf.links <- ecdf.links[1:max(which(!is.na(ecdf.links))),]
    value.to.return <- list(prey=ecdf.prey,
                            predators=ecdf.predators,
                            links=ecdf.links)                        
  }
  
  if(nits>1){
    
    clean.ecdf.prey <- ecdf.prey[apply(ecdf.prey, 1, function(x) sum(x[!is.na(x)]))>0,]
    mean.ecdf.prey <- apply(clean.ecdf.prey, 1, function(x) mean(x[!is.na(x)]))
    CI.ecdf.prey <-  t(apply(clean.ecdf.prey, 1, function(x) Empirical.CI(x[!is.na(x)], 95)))

    
    clean.ecdf.predators <- ecdf.predators[apply(ecdf.predators, 1, function(x) sum(x[!is.na(x)]))>0,]
    mean.ecdf.predators <- apply(clean.ecdf.predators, 1, function(x) mean(x[!is.na(x)]))
    CI.ecdf.predators <-  t(apply(clean.ecdf.predators, 1, function(x) Empirical.CI(x[!is.na(x)], 95)))
    
    clean.ecdf.links <- ecdf.links[apply(ecdf.links, 1, function(x) sum(x[!is.na(x)]))>0,]
    mean.ecdf.links <- apply(clean.ecdf.links, 1, function(x) mean(x[!is.na(x)]))
    CI.ecdf.links <-  t(apply(clean.ecdf.links, 1, function(x) Empirical.CI(x[!is.na(x)], 95)))

    value.to.return <- list(prey=cbind(mean.ecdf.prey, CI.ecdf.prey),
                            predators=cbind(mean.ecdf.predators, CI.ecdf.predators),
                            links=cbind(mean.ecdf.links, CI.ecdf.links))
  }
  
  value.to.return
}


## ## writes a file that gviz can use to plot a graph
## Makegvizdat <- function(mat, filename="gviz.web.dot", list.of.colours="No"){

##   ## matrix.to.list <- function(web.matrix, predator.first=TRUE){
##   dat <- Matrix.to.list(mat, predator.first=FALSE)

##   ## Make output file in graphviz format
##   cat("digraph web \{ \n", append=F, file=filename)
##   cat("size=", paste("\"100,100\";"), " node [style=filled];\n", append=T, file=filename)
##   for(i in 1:length(dat[,1]))
##       cat(paste(dat[i,1], "->", dat[i,2], ";\n"), append=T, file=filename)

##   if(length(list.of.colours)>1){
##     list.of.species <- dimnames(mat)[[1]]
##     for(i in 1:length(list.of.species))
##       cat(paste(list.of.species[i], " [fillcolor=", list.of.colours[i], "];\n", sep=""), append=T, file=filename)
##   }
##   cat("}", append=T, file=filename)
  
## }


## this can handle multiple prey columns, but blanks must be zeros
## the range argument is whether the list information is such that the second and third column contain the ranges
## in this case names must be numeric
List.to.matrix <- function(web.list, predator.first=TRUE){

 
  if(length(web.list[1,])==2){
    
    ## get the species names
    species <- c(as.character(web.list[,1]), as.character(web.list[,2]))
    species.names <- sort(unique(species[!is.na(species)]))
    
    number.of.species <- length(species.names)
    web.matrix <- matrix(0, number.of.species, number.of.species)
    dimnames(web.matrix) <- list(species.names, species.names)

    
    if(predator.first){
      for(i in 1:length(web.list[,1])){
          web.matrix[!is.na(match(dimnames(web.matrix)[[1]], web.list[i,2])),
                     !is.na(match(dimnames(web.matrix)[[2]], web.list[i,1]))] <- 1
        }


    }
      
      
    if(!predator.first)
      stop("Not completed the prey first non-range function")
  }
  
  if(length(web.list[1,])==3){


    ## get the species names
    species <- c(web.list[,1])
    for(i in 1:length(web.list[,1])){
      if(is.na(web.list[i,3]))
        species <- c(species, web.list[i,2])
      if(!is.na(web.list[i,3]))
        species <- c(species, web.list[i,2]:web.list[i,3])
    }
    species.names <- sort(unique(species[!is.na(species)]))
    
    number.of.species <- length(species.names)
    web.matrix <- matrix(0, number.of.species, number.of.species)
    dimnames(web.matrix) <- list(species.names, species.names)
    
    if(!is.numeric(species.names))
      stop("Species names must be numeric in range type web lists")

    if(predator.first)
      for(i in 1:length(web.list[,1])){
        if(is.na(web.list[i,3]))
          web.matrix[!is.na(match(dimnames(web.matrix)[[1]], web.list[i,2])),
                     !is.na(match(dimnames(web.matrix)[[2]], web.list[i,1]))] <- 1
        if(!is.na(web.list[i,3])){
          prey.links <- web.list[i,2]:web.list[i,3]
          for(p in prey.links)
            web.matrix[!is.na(match(dimnames(web.matrix)[[1]], p)),
                       !is.na(match(dimnames(web.matrix)[[2]], web.list[i,1]))] <- 1
        }
      }

    if(!predator.first)
      stop("Not completed the prey first function")
  }
  web.matrix
}
  

## takes a food web in matrix format and coverts it to list format
Matrix.to.list <- function(web.matrix, predator.first=TRUE){
  if(length(dimnames(web.matrix)[[1]])==length(web.matrix[1,]))
    species.names <- dimnames(web.matrix)[[1]]
  else
    species.names <- 1:length(web.matrix[,1])
  web.list <- matrix(0, sum(web.matrix), 2)
  counter <- 1
  for(i in 1:length(web.matrix[,1]))
    for(j in 1:length(web.matrix[,1]))
      if(web.matrix[i,j]==1){
        web.list[counter,] <- c(species.names[i],species.names[j])
        counter <- counter + 1
      }
  if(!predator.first)
    web.list <- cbind(web.list[,2], web.list[,1])
  web.list
}

  
Draw.web <- function(web,
                       check.web=TRUE,
                       web.name=NULL,
                       web.structure=NULL,
                       species.trophic.heights=NULL,
                       traits=NULL,
                       link.strengths=FALSE){
  
  ## general routine for checking the food web matrix is valid
  if(check.web){
  ## check square
    if(length(web[1,])!=length(web[,1]))
      stop("Not a square matrix")
    ## check for dimnames
    if(length(dimnames(web)[[1]])==0 || length(dimnames(web)[[1]])==0)
      stop("No species names given (dimnames(web) are NULL)")
    ## check that the dimnames are the same
    if(length(web[1,])!=sum(dimnames(web)[[1]]==dimnames(web)[[2]]))
      stop("Species names do not match between rows and columns")
  }
  
  s <- length(web[,1])
  species.names <- dimnames(web)[[1]]
  dimnames(web)[[1]] <- 1:s

  ## save link.strengths
  web.strengths <- web

  
  ## only keep positives
  ##for(i in 1:number.of.species)
  ##  for(j in 1:number.of.species)
  ##    web[i,j] <- ifelse(web[i,j]>0, 1, 0)

  species.positions <- Bottom.Intermediate.Top(web)

  ## remove unconnected species
  ##if(length(species.positions$Unconnected)!=0){
  ##  web <- web[-as.numeric(species.positions$Unconnected),-as.numeric(species.positions$Unconnected)]
  ##  web.strengths <- web.strengths[-as.numeric(species.positions$Unconnected),-as.numeric(species.positions$Unconnected)]
  ##  species.names <- species.names[-as.numeric(species.positions$Unconnected)]
  ##}
  s <- length(web[,1])
  dimnames(web)[[1]] <- 1:s
  
  if(length(web.structure)!=0){
  ## identify the basal, intermediate, and top species
    basal<-1:web.structure[1,2]
    intermediate <- (max(basal)+1):(max(basal)+web.structure[2,2])
    top <- (max(intermediate)+1):(max(intermediate)+web.structure[3,2])
  }

  if(length(web.structure)==0 && length(species.trophic.heights)==0){
    ## identify the basal, intermediate, and top species
    species.positions <- Bottom.Intermediate.Top(web)
    basal <-  species.positions$Bottom
    top <-   species.positions$Top
    intermediate <- species.positions$Intermediate
    
    ## order the web by basal, intermediate, and top species
    reorder.web <-  as.numeric(c(basal, intermediate, top))
    web <- web[reorder.web,reorder.web]
    species.names <- species.names[reorder.web]
    
    ## again identify the basal, intermediate, and top species
    basal <- which(apply(web, 1, sum)==0)
    top <-  which(apply(web, 2, sum)==0)
    if(length(basal)>0 && length(top)>0)
      intermediate <- c(1:s)[c(-basal, -top)]
    if(length(basal)>0 && length(top)==0)
      intermediate <- c(1:s)[-basal]
    if(length(basal)==0 && length(top)>0)
      intermediate <- c(1:s)[-top]
    if(length(basal)==0 && length(top)==0)
      intermediate <- 1:s
    
    ## order the intermediate according to how many species they eat
    ## and how many they are eaten by
    if(is.matrix(web[intermediate,])){
      num.intermediate.eat <- apply(web[intermediate,], 1, sum)
      num.intermediate.eaten.by <- apply(web[,intermediate], 2, sum)
      int.order <- intermediate[order(num.intermediate.eat-num.intermediate.eaten.by)]
    }
    else
      int.order <- intermediate
    reorder.web <-  c(basal, int.order, top)
    web <- web[reorder.web,reorder.web]
    species.names <- species.names[reorder.web]
  }
    
  ## set some graphical parameters
  x.range <- 100 ## horizontal extent of plotting area
  y.range <- 100 ## vertical extent of plotting area
  points.cex <- 3 ## size of circles denoting species
  margin <- 5 ## margin in which circles are not plotted
  arrow.adj <- 6 ## distance from centre of circle for arrows to terminate
  points.width <- 1
  species.names.cex <- 0.2
  arrow.head.length <- 0.1

  
  ## define the coordinates of basal, intermediate, and top species
  basal.coords <- cbind(cumsum(rep(x.range/(length(basal)+1), length(basal))),
                        rep(0,length(basal)))
  top.coords <-  cbind(cumsum(rep(x.range/(length(top)+1), length(top))),
                       rep(x.range,length(top)))
  int.0 <- rep(x.range/(length(intermediate)+1), length(intermediate))
  int.y <- cumsum(int.0)[order(runif(length(intermediate)))]
  intermediate.coords <- cbind(cumsum(rep(x.range/(length(intermediate)+1), length(intermediate))), rep(50, length(intermediate)))
  all.coords <- rbind(basal.coords, intermediate.coords, top.coords)

  ## colours by trait
  if(length(traits)!=0){
    integer.traits <- traits-min(traits)
    integer.traits <- integer.traits/max(integer.traits)
    integer.traits <- 101-ceiling(100*integer.traits)
    ##print(cbind(trait.colours, integer.traits))
    colours.to.use <- rainbow(max(integer.traits)+max(integer.traits))
  
    ## plot the species circles and numbers
    plot(c(-margin,x.range+margin), c(-margin,x.range+margin), type="n", xlab="", ylab="", axes=F)
    points(basal.coords[,1], basal.coords[,2], cex=points.cex,
           col=colours.to.use[integer.traits[basal]], lwd=points.width)
    points(top.coords[,1], top.coords[,2], cex=points.cex,
           col=colours.to.use[integer.traits[top]], lwd=points.width)
    points(intermediate.coords[,1], intermediate.coords[,2], cex=points.cex,
           col=colours.to.use[integer.traits[intermediate]], lwd=points.width)
    text(all.coords[,1], all.coords[,2], labels=species.names, species.names.cex) 
    title(main=web.name)
  }
  
  ## or don't colour by trait
  if(length(traits)==0){
    
    ## plot the species circles and numbers
    plot(c(-margin,x.range+margin), c(-margin,x.range+margin), type="n", xlab="", ylab="", axes=F)
    points(basal.coords[,1], basal.coords[,2], cex=points.cex, lwd=points.width)
    points(top.coords[,1], top.coords[,2], cex=points.cex, lwd=points.width)
    points(intermediate.coords[,1], intermediate.coords[,2], cex=points.cex, lwd=points.width)
    text(all.coords[,1], all.coords[,2], labels=species.names) 
    title(main=web.name)
  }

  if(!link.strengths){
    web <- -web*upper.tri(web)
    ## draw the arrows between species
    for(i in 1:s)
      for(j in 1:s){
        if(i!=j)
          if(web[i,j]!=0){
            x.len <- (all.coords[i,1]-all.coords[j,1])
            y.len <- (all.coords[i,2]-all.coords[j,2])
            angle <- atan(y.len/x.len)
            ## print(paste(i,j,angle))
            if(x.len>=0){
              y.adj <- sin(angle)*arrow.adj
              x.adj <- cos(angle)*arrow.adj
            }
            if(x.len<0){
              y.adj <- -sin(angle)*arrow.adj
              x.adj <- -cos(angle)*arrow.adj
            }
            suppressWarnings(arrows(all.coords[i,1]-x.adj, all.coords[i,2]-y.adj, 
                   all.coords[j,1]+x.adj, all.coords[j,2]+y.adj,
                   length=arrow.head.length, angle=30, code=2,))
          }
      }
  }


  
  if(link.strengths){
    web <- -web*upper.tri(web)

    ##scale line widths
    arrow.widths <- web-min(web[web!=0])
    arrow.widths <- arrow.widths/max(web)*4+1

    
    ## draw the arrows between species
    for(i in 1:s)
      for(j in 1:s){
        if(i!=j)
          if(web[i,j]!=0){
            x.len <- (all.coords[i,1]-all.coords[j,1])
            y.len <- (all.coords[i,2]-all.coords[j,2])
            angle <- atan(y.len/x.len)
            ## print(paste(i,j,angle))
            if(x.len>=0){
              y.adj <- sin(angle)*arrow.adj
              x.adj <- cos(angle)*arrow.adj
            }
            if(x.len<0){
              y.adj <- -sin(angle)*arrow.adj
              x.adj <- -cos(angle)*arrow.adj
            }
            suppressWarnings(arrows(all.coords[i,1]-x.adj, all.coords[i,2]-y.adj, 
                   all.coords[j,1]+x.adj, all.coords[j,2]+y.adj,
                   length=arrow.head.length, angle=30, code=2,lwd=arrow.widths[i,j]))
          }
      }
  }

}

Make.summary.web <- function(web.list){
  web <- web.list[[1]]
  for(i in 2:length(web.list))
    web <- web+web.list[[i]]
  web <- matrix(1, length(web[,1]), length(web[,1]))*(web>0)
  web
}

Get.web.stats <- function(web, which.stats=1:4, real.web=NA){
    if(!is.list(web)){
        result <- c(S = length(web[,1]),
                    L = sum(web),
                    C = sum(web)/length(web[,1])^2)
        
        if(sum(which.stats==1)==1){
            BITUC <- Bottom.Intermediate.Top(web, proportion=T)$Proportions.of.each
            result <- c(result,
                        B = BITUC[1],
                        I = BITUC[2],
                        T = BITUC[3],
                        U = BITUC[4],
                        CB = Cannibals(web),
                        PCB = BITUC[5])
        }
        
        if(sum(which.stats==2)==1){
            result <- c(result,
                        Gensd = Gen.sd(web),
                        Vulsd = Vul.sd(web))
        }
        
        if(sum(which.stats==3)==1)
            result <- c(result, Maxsim=Maxsim(web))
        
        if(sum(which.stats==4)==1){

            TLs <- GetTL(web)
            if(is.na(TLs[1])){
                mean.TL <- NA
                max.TL <- NA
                sd.TL <- NA
            }
            if(!is.na(TLs[1])){
                mean.TL <- mean(TLs, na.rm=T)
                max.TL <- max(TLs, na.rm=T)
                sd.TL <- sd(TLs, na.rm=T)
            }
            
            result <- c(result,
                        mean.TL = mean.TL,
                        max.TL=max.TL,
                        sd.TL=sd.TL)                        

            
        }

        if(sum(which.stats==5)==1){

            ## move this to which.stats==1 set
            Num.H <- Num.herbivores(web) / length(web[,1])

            ## Move this to which.stats==4
            frac.omniv <- Fraction.omnivores(web)

            lev.omniv <- Level.omnivory(web)
            
            ## igraph package required
            library(igraph)
            iweb <- graph.adjacency(web, mode="directed")
            clust.coef <- transitivity(iweb)
            char.path.length <- mean(shortest.paths(iweb))
            
            
            result <- c(result,
                        Num.H = Num.H,
                        Prop.om = frac.omniv,
                        Lev.om = lev.omniv,
                        Clust = clust.coef,
                        Cpl = char.path.length)
            
        }
        
        
        
    }
    
    if(is.list(web)){
        for(i in 1:length(web)){
            
            t.result <- c(S = length(web[[i]][,1]),
                        L = sum(web[[i]]),
                        C = sum(web[[i]])/length(web[[i]][,1])^2)
            
            if(sum(which.stats==1)==1){
                BITUC <- Bottom.Intermediate.Top(web[[i]], proportion=T)$Proportions.of.each
                t.result <- c(t.result,
                            B = BITUC[1],
                            I = BITUC[2],
                            T = BITUC[3],
                            U = BITUC[4],
                            CB = Cannibals(web[[i]]),
                            PCB = BITUC[5])
            }
            
            if(sum(which.stats==2)==1){
                t.result <- c(t.result,
                            Gensd = Gen.sd(web[[i]]),
                            Vulsd = Vul.sd(web[[i]]))
            }
            
            if(sum(which.stats==3)==1)
                t.result <- c(t.result, Maxsim=Maxsim(web[[i]]))
            
            if(sum(which.stats==4)==1){

                
                TLs <- GetTL(web[[i]])
                if(is.na(TLs[1])){
                    mean.TL <- NA
                    max.TL <- NA
                    sd.TL <- NA
                }
                if(!is.na(TLs[1])){
                    mean.TL <- mean(TLs, na.rm=T)
                    max.TL <- max(TLs, na.rm=T)
                    sd.TL <- sd(TLs, na.rm=T)
                }
                
                t.result = c(t.result,
                    mean.TL = mean.TL,
                    max.TL=max.TL,
                    sd.TL=sd.TL)            
            }

            
        if(sum(which.stats==5)==1){

            ## move this to which.stats==1 set
            Num.H <- Num.herbivores(web[[i]])  / length(web[[i]][,1])

            ## Move this to which.stats==4            
            frac.omniv <- Fraction.omnivores(web[[i]])
            lev.omniv <- Level.omnivory(web[[i]])
            
            ## igraph package required
            library(igraph)
            iweb <- graph.adjacency(web[[i]], mode="directed")
            clust.coef <- transitivity(iweb)
            char.path.length <- mean(shortest.paths(iweb))
            
            
            t.result <- c(t.result,
                          Num.H = Num.H,
                          Prop.om = frac.omniv,
                          Lev.om = lev.omniv,                          
                          Clust = clust.coef,
                          Cpl = char.path.length)
            
        }



            if(i==1)
                result <- t.result
            if(i>1)
                result <- rbind(result, t.result)
        }
    }


##     ## relative position
##     if(is.matrix(real.web)){
##         if(!is.list(web)){
##             temp <- G.test.webs(real.web, web)
##             result <- c(result, yy=temp[1], ass.dir=temp[2], ass.sig=temp[3])
##         }
##         if(is.list(web)){
##             temp.result1 <- numeric(length=length(web))
##             temp.result2 <- numeric(length=length(web))
##             temp.result3 <- numeric(length=length(web))
##             for(i in 1:length(web)){
##                 temp <- G.test.webs(real.web, web[[i]])
##                 temp.result1[i] <- temp[1]
##                 temp.result2[i] <- temp[2]
##                 temp.result3[i] <- temp[3]
##             }
##             result <- cbind(result, yy=temp.result1, ass.dir=temp.result2, ass.sig=temp.result3)          
##         }
##     }


##     ## neighbour distances
##     if(!is.list(web)){
##         temp.d <- Naybor.dist(web)
##         result <- c(result, mean.dist=temp.d[1], cv.dist=temp.d[2])
##     }
##     if(is.list(web)){
##         temp.result1 <- numeric(length=length(web))
##         temp.result2 <- numeric(length=length(web))
##         for(i in 1:length(web)){
##             temp <- Naybor.dist(web[[i]])
##             temp.result1[i] <- temp[1]
##             temp.result2[i] <- temp[2]
##         }
##         result <- cbind(result, mean.dist=temp.result1, mean.dist=temp.result2)          
##     }

##     ## variogram stats
##     if(is.matrix(real.web)){
##         if(!is.list(web)){
##             temp <- Variogram.comparison(real.web, web)
##             result <- c(result, cor.vg=cor.vg, cor.vg.0=cor.vg.0, cor.vg.45=cor.vg.45, cor.vg.90=cor.vg.90,
##                         mean.cvg.0=mean.cvg.0, mean.cvg.45=mean.cvg.45, mean.cvg.90=mean.cvg.90)
##         }
##         if(is.list(web)){
##             temp <- matrix(0, length(web), 7)
##             dimnames(temp)[[2]] <-     c("cor.vg", "cor.vg.0", "cor.vg.45", "cor.vg.90",
##                                          "mean.cvg.0", "mean.cvg.45", "mean.cvg.90")
##             for(i in 1:length(web)){
##                 temp[i,] <- Variogram.comparison(real.web, web[[i]])
##             }
##             result <- cbind(result, temp)          
##         }
##     }
    
    result
}

Fraction.omnivores <- function(web) {
    TLs <- GetTL(web)
    non.int.TL <- web[,TLs %% 1 != 0]
    if(is.matrix(non.int.TL))
        frac.omniv <- sum(apply(non.int.TL, 2, sum) > 1)  / length(web[,1])
    if(is.vector(non.int.TL)) 
        frac.omniv <- (sum(non.int.TL) > 1)  / length(web[,1])
    frac.omniv
}

Level.omnivory <- function(web) {

    TLs <- GetTL(web)

    if( sum(is.na(TLs)) == length(TLs) )
       rr <- NA

    if( sum(is.na(TLs)) != length(TLs) ) {

        
        web.TLs <- matrix(rep(TLs, length(web[,1])), length(web[,1]), length(web[,1]))
        lo.pc <- numeric(length=length(web[,1]))
        for(i in 1:length(web[,1])) {
            tt <- web.TLs[web[,i]==1,i]
            if(length(tt)==0 | sum(!is.na(tt))==0 )
                lo.pc[i] = NA
            if(length(tt)>0 & sum(!is.na(tt))!=0)
                lo.pc[i] <- sd(tt)
        }
        rr <- mean(lo.pc, na.rm=T)
    }
    rr
}



Number.of.species <- function(web, check.web=FALSE)
    length(web[,1])

Number.of.links <- function(web, check.web=FALSE)
    sum(web)

Link.density <- function(web, check.web=FALSE)
    sum(web)/length(web[,1])

Connectance <- function(web, undirected=FALSE, check.web=FALSE){
  if(undirected)
    C <- Number.of.links(web)/(length(web[1,])*(length(web[1,])-1)/2)
  if(!undirected)
    C <- sum(web)/length(web[1,])^2
  C
}

## return the proportion of bottom, intermediate, top,
## unconnected and purely cannibalistic specie
Bottom.Intermediate.Top <- function(web, proportion=TRUE, check.web=FALSE){
    
  ## find the names and numbers of BIT, unconnected and pure cannibals
  names.all <- 1:length(web[1,])
  dimnames(web) <- list(names.all, names.all)
  names.Bottom <-  names.all[apply(web, 2, sum)==0 & apply(web, 1, sum)!=0]
  number.Bottom <- length(names.Bottom)
  names.Top <-   names.all[apply(web, 2, sum)!=0 & apply(web, 1, sum)==0]
  number.Top <- length(names.Top)
  names.Unconnected <- names.all[apply(web, 2, sum)==0 & apply(web, 1, sum)==0]
  number.Unconnected <- length(names.Unconnected)
  number.Pure.cannibals <- 0
  names.Pure.cannibals <- character(0)
  for(i in 1:length(web[,1]))
    if(web[i,i]!=0 && sum(web[-i,i]+web[i,-i])==0){
      if(number.Pure.cannibals==0){
        names.Pure.cannibals <- dimnames(web)[[1]][i]
        number.Pure.cannibals <- 1
      }
      else{
        names.Pure.cannibals <- c(names.Pure.cannibals, dimnames(web)[[1]][i])
        number.Pure.cannibals <- number.Pure.cannibals + 1
      }
    }
  names.Intermediate <- dimnames(web)[[1]][is.na(match(dimnames(web)[[1]],
                                                      c(names.Bottom, names.Top, names.Unconnected, names.Pure.cannibals)))]
  number.Intermediate <- length(web[1,])-number.Bottom-number.Top-number.Unconnected-number.Pure.cannibals

  if(proportion)
    result <- list(Bottom=names.Bottom,
                   Intermediate=names.Intermediate,
                   Top=names.Top,
                   Unconnected=names.Unconnected,
                   Pure.cannibals=names.Pure.cannibals,
                   Proportions.of.each=c(number.Bottom, number.Intermediate, number.Top, number.Unconnected, number.Pure.cannibals)/length(web[1,]))
  if(!proportion)
    result <- list(Bottom=names.Bottom,
                   Intermediate=names.Intermediate,
                   Top=names.Top,
                   Unconnected=names.Unconnected,
                   Pure.cannibals=names.Pure.cannibals,
                   Proportions.of.each=c(number.Bottom, number.Intermediate, number.Top, number.Unconnected, number.Pure.cannibals))
  result
}

Num.herbivores <- function(web) {
    
    S <- length(web[,1])
    ## Find the rows/columns with basal species
    B.rows <-  c(1:S)[apply(web, 2, sum)==0 & apply(web, 1, sum)!=0]
    ## Find r/c wth species that only eat basal
    if(length(B.rows)==0)
        Nh <- NA

    if(length(B.rows)>0) {
        if( length(B.rows)>1 & (S-length(B.rows))>1 )
            Nh <- sum( apply(web[B.rows,], 2, function(x) sum(x!=0)>0) &
                      apply(web[-B.rows,], 2, function(x) sum(x!=0)==0) )
        
        if( length(B.rows)==1 & (S-length(B.rows))>1 )
            Nh <- sum( web[B.rows,]!=0  &
                      apply(web[-B.rows,], 2, function(x) sum(x!=0)==0) )
        
        if( length(B.rows)>1 & (S-length(B.rows))==1 )
            Nh <- sum( apply(web[B.rows,], 2, function(x) sum(x!=0)>0) &
                      web[-B.rows,]==0) 
    }
    Nh

}
    
    
Cannibals <- function(web)
  length(dimnames(web)[[1]][diag(web)==1]) / length(web[1,])

Gen.sd <- function(web)
  sd(colSums(web)/sum(web))

Vul.sd <- function(web)
  sd(rowSums(web)/sum(web))

Maxsim <- function(web){
  sims <- matrix(0, length(web[,1]), length(web[,1]))
  for(i in 1:length(web[,1]))
    for(j in 1:length(web[,1]))
      sims[i,j] <- T.sim.ij(web, i, j)
  diag(sims) <- NA
  mean(apply(sims, 1, function(x) max(x[!is.na(x)])))
}

## used by Maxsim
T.sim.ij <- function(web, i, j){
  same <- sum(web[i,] & web[j,]) + sum(web[,i] & web[,j])
  total <- sum(web[i,] | web[j,]) + sum(web[,i] | web[,j])
  same / total
}

GetTL <- function(web){
    
    ## takes predation matrix with consumers in columns
    ## identify the columns with basal species
    tweb <- t(web)

    ## make the rows add to one
    rs <- rowSums(tweb)
    for(i in 1:length(tweb[,1]))
        tweb[i,tweb[i,]==1] = 1/rs[i]

    nb.TL <- try(solve(diag(length(tweb[,1])) - tweb), T)

    if(class(nb.TL)=="try-error")
        nbTL <- rep(NA, length(tweb[,1]))

    if(class(nb.TL)!="try-error")
        nbTL <- rowSums(nb.TL)

    nbTL
    
}
    
## Returns the species that take part in loops
Species.in.loops <- function(web){

  species.names <- character(0)
  for(i in 2:length(web[,1])){
    temp <- web
    for(j in 1:(i-1))
      temp <- temp %*% web
    if(i==2)
      species.names <- dimnames(web)[[1]][diag(temp)>0]
    else
      species.names <- c(species.names, dimnames(web)[[1]][diag(temp)>0])
  }
  sort(unique(species.names))
}

## Returns a list of the separate webs in the matrix
Return.connected.webs <- function(web){
  cum.done <- numeric(0)
  sub.web <- 1
  while(length(cum.done)<length(web[1,])){
    if(length(cum.done)==0)
      i <- 1
    if(length(cum.done)>0)
      i <- c(1:length(web[,1]))[-cum.done][1]
    species.list <- i
    done <- i
    species.list <- unique(c(species.list, which(web[species.list,]>0), which(web[,species.list]>0)))
    while(sum(!is.na(match(species.list, done)))!=length(species.list)){
      species.list <- unique(c(species.list,
                               which(web[species.list[is.na(match(species.list, done))][1], ]>0),
                               which(web[, species.list[is.na(match(species.list, done))][1]]>0)))
      done <- c(done, species.list[is.na(match(species.list, done))][1])
    }

    species.list <- sort(species.list)
    
    if(length(cum.done)>0)
      cum.done <- c(cum.done, done)
    
    if(length(cum.done)==0)
      cum.done <- sort(done)
    
    if(sub.web>1 && length(species.list)>1){
      all.webs[[sub.web]] <- web[species.list,species.list]
      dimnames(all.webs[[sub.web]]) <- list(species.list, species.list)
      sub.web <- sub.web+1
    }
   
    if(sub.web==1 && length(species.list)>1){
      all.webs <- list(web[species.list,species.list])
      dimnames(all.webs[[sub.web]]) <- list(species.list, species.list)
      sub.web <- sub.web+1
    }
  }
  all.webs
}


HOP.l.by.s <- function(web){
  op.columns <- colSums(web)!=0
  web <- web[,op.columns]
  l.by.s <- mean(colSums(web))
  l.by.s
}
  

Web.ecdf <- function(web, scaled=TRUE){
  if(scaled){
    z <- sum(web)/dim(web)[[1]]
    u.knots <- 0:S/2/z
    prey <- ecdf(colSums(web)/2/z)
    prey <- cbind(u.knots, 1-prey(u.knots))
    pred <- ecdf(rowSums(web)/2/z)
    pred <- cbind(u.knots, 1-pred(u.knots))
    links <- ecdf((rowSums(web)+colSums(web))/2/z)
    links <- cbind(u.knots, 1-links(u.knots))
  }
  if(!scaled){
    u.knots <- 0:S
    prey <- ecdf(colSums(web))
    prey <- cbind(u.knots, 1-prey(u.knots))
    pred <- ecdf(rowSums(web))
    pred <- cbind(u.knots, 1-pred(u.knots))
    links <- ecdf((rowSums(web)+colSums(web)))
    links <- cbind(u.knots, 1-links(u.knots))
  }
  result <- list(prey=prey, pred=pred, links=links)
  result
}             

Shortest.paths <- function(web, cannibalism=1, undirected=T){
  undirected.web <- matrix(1, length(web[,1]), length(web[1,]))*((web+t(web))>0)
  shortest.paths <- geodist(undirected.web)$gdist
  shortest.paths <- shortest.paths[upper.tri(shortest.paths)]
  if(cannibalism==1){
    self.self <- diag(undirected.web)
    self.self[self.self==0] <- 2
    shortest.paths <- c(shortest.paths, as.vector(self.self))
  }
  shortest.paths
}

G.test.webs <- function(real.web, model.web){
    yy <- sum(real.web==1 & model.web==1)
    nn <- sum(real.web==0 & model.web==0)
    yn <- sum(real.web==1 & model.web==0)
    ny <- sum(real.web==0 & model.web==1)
    m1 <- glm(c(yy, nn, yn, ny)~c(1,0,1,0) + c(1,0,0,1), family=poisson)
    m1$aic
}

Chi2.test.webs <- function(real.web, model.web){
    yy <- sum(real.web==1 & model.web==1)
    nn <- sum(real.web==0 & model.web==0)
    yn <- sum(real.web==1 & model.web==0)
    ny <- sum(real.web==0 & model.web==1)
    tab.chi<-matrix(c(yy,yn,ny,nn),ncol=2)
    m1 <- chisq.test(tab.chi)$statistic
    m1
}
    
Compare.links <- function(real.web, model.web){
  result <- sum(real.web==1 & model.web==1)/sum(model.web)
  result
}


