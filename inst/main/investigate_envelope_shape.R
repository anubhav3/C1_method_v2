# 22.11.2021
# We investigate the evelope shape of the points in Fig. S37-38 of SI Ver. 1

library(dplyr)
library(ggplot2)
library(tidyverse)

####### Mill Stream
fw_prop <- readRDS("results/rejection/Mill Stream/rN=1000_tol=0.75_TSS_lower_a/Mill Stream_prop.RDS")$prop

fw_prop_longer <- fw_prop %>%
  pivot_longer(!TSS, names_to = "Properties", values_to = "values")

ggplot(fw_prop_longer) +
  geom_point(aes(x = TSS, y = values)) +
  facet_wrap(~Properties, scales = "free")
