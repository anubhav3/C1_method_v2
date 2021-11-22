# 12.10.2021
# We compare the food web data from 2008 paper and from the openly accessible

##### For Benguela Pelagic

bp_2008 <- readRDS("../C1_method/data_new/Benguela Pelagic.web.Rdata")
bp_2021 <- readRDS("../C1_method_v2/data/Benguela Pelagic.web.RDS")

sum(bp_2008$predation.matrix != bp_2021$predation.matrix)
