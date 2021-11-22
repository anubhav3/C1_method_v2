rm(list=ls())


source("~/core.R.functions/core_R_functions.r")

setwd("~/work/research/4.in.review/allometric.web/data/Jens/smallreef")

web.name = "Small Reef"

species.sizes <- read.csv("sizes.csv", header=F)[-51,2]

web <- read.csv("predation.list.csv")
predation.matrix <- List.to.matrix(web)

## sort everything by species sizes
new.order <- order(species.sizes)
species.sizes <- species.sizes[new.order]
predation.matrix <- predation.matrix[new.order, new.order]


all.web.info <- list(web.name=web.name,
                         species.names=dimnames(predation.matrix[[1]]),
                         size.units <- c("mass"),
                         species.sizes=species.sizes,
                         species.abundance=NA,
                         predation.matrix=predation.matrix)

setwd("~/work/research/4.in.review/allometric.web/data/by.webs/all.species/Small Reef")
save(all.web.info, file=paste(web.name, ".web.Rdata", sep=""))    
