#Here, we find the lower prior range of parameter 'a'
library(dplyr)
library(ggplot2)

fw_name <- "Benguela Pelagic"

fw_bs <- readRDS(paste0("results/rejection/", fw_name, "/rN=1e+05_tol=2_TSS_min_a/", fw_name, ".RDS"))
dd_bs <- data.frame(a = fw_bs$post_dists$a,
                    ai = fw_bs$post_dists$ai,
                    aj = fw_bs$post_dists$aj,
                    b = fw_bs$post_dists$r.b,
                    conn = fw_bs$acc_ss,
                    TSS = 1-fw_bs$dist)
dd_ai <- dd_bs
dd_ai$ai <- as.factor(round(dd_bs$ai, digits = 1))
dd_ai$b <- as.factor(round(dd_bs$b, digits = 1))
dd_ai$aj <- as.factor(round(dd_bs$aj, digits = 1))
# dd_ai$a <- as.factor(round(dd_bs$a, digits = 1))


dd_ai %>%
  filter(b %in% seq(as.integer(range(dd_bs$b)[1]), as.integer(range(dd_bs$b)[2]), by = 0.5)) %>%
  # filter(b %in% c(0.0)) %>%
  ggplot() +
  geom_point(aes(x = a, y = TSS, color = b)) +
  geom_vline(xintercept = -10, color = "red") +
  geom_vline(xintercept = 10, color = "red") +
  ylim(c(-1,1)) +
  xlim(c(-20,10)) +
  xlab(TeX("log_{10}(a)")) +
  ylab("True skill statistics") +
  theme_classic() +
  scale_color_discrete(name = TeX("log_{10}(b)"))

# ggsave(filename = paste0("results/rejection/", fw_name, "/rN=1e+05_tol=2_TSS_min_a/prior_a_selection_", fw_name, ".png"))
