# 04.10.2021
# Check the script from Owen

# source("~/core.R.functions/core_R_functions.r")
source("data/from Owen/brose_2008_data_treatment/functions/standard.food.web.functions.r")

# setwd("~/work/research/4.in.review/allometric.web/data/Jens/Capinteria")

web.name = "Capinteria"

species.sizes <- read.csv("data/three webs/Capinteria/masses.csv", header=F) ##[,1]
##species.names <- read.csv("sizes.csv", header=T)[,5]

web <- read.csv("data/secondary/three webs/Capinteria/predation.list.csv")
predation.matrix <- List.to.matrix(web)

## sort everything by species sizes
new.order <- order(species.sizes[,2])
species.sizes <- species.sizes[new.order,]
predation.matrix <- predation.matrix[new.order, new.order]
