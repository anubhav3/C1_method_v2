rm(list=ls())

## LOCATION:
if(.Platform$OS.type=="windows")
  source("C:\\Documents and Settings\\bo1op\\My Documents\\work\\programming\\core R code for Rprofile\\core_R_functions.r")
if(.Platform$OS.type=="unix")
  source("~/core.R.functions/core_R_functions.r")

setwd("C:\\Documents and Settings\\bo1op\\My Documents\\work\\research\\4.in.review\\allometric.web\\data\\Ythan")





pm <- as.matrix(read.csv("Ythan1.pm.csv", header=F))
sizes <- as.matrix(read.csv("Ythan1.sizes.csv", header=F))

pm1 <- ifelse(pm==-1, 1, 0)

pm2 <- pm1[order(sizes), order(sizes)]

sizes2 <- sizes[order(sizes)]

Plot.matrix(pm1)

dimnames(pm2) <- list(1:88, 1:88)
species.names <- 1:88

all.web.info <- list(web.name="Ythan1",
                           species.names=1:88,
                           size.type="mass",
                           species.sizes=sizes2,
                           species.met.cat=NA,
                           feeding.interaction=NA,
                           species.abundance=NA,
                           predation.matrix=pm2)

setwd("C:\\Documents and Settings\\bo1op\\My Documents\\work\\research\\4.in.review\\allometric.web\\data\\by.webs\\all.species\\Ythan1")

save(all.web.info, file="Ythan1.web.Rdata")

Bottom.Intermediate.Top(all.web.info$predation.matrix)

all.web.info$predation.matrix
pm2
