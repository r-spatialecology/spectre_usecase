### Plot results of min_conf_ algorithm tests...

setwd("~/spectre/BCI_tidy")

library(ggplot2)
library(tidyverse)

df<- list.files(path = paste0(getwd(), "/table_res"), pattern = "*.rds", full.names = TRUE) %>%
  map(readRDS) %>%
  bind_rows()

df$replicate <- as.factor(df$replicate)

df$Commonness <- as.numeric(as.character(df$Commonness))

names(df)

df$delta_commonness <- df$mean_real_commonness_error / df$mean_commonness * 100

# https://www.datanovia.com/en/blog/how-to-change-ggplot-facet-labels/

# New facet label names for sites
sites.labs <- c("30 sites", "50 sites", "100 sites", "150 sites")
names(sites.labs) <- c("30", "50", "100", "150")

# New facet label names for species
spec.labs <- c("25 spec.", "50 spec.", "100 spec.", "150 spec.", "200 spec.", "250 spec.")
names(spec.labs) <- c("25", "50", "100", "150", "200", "250")


# Mean_commonness
ggplot(data = df, aes(x = iterations, y = mean_commonness, col = replicate))+ # , col = replicate))+
  facet_grid(scales="fixed", vars(n_species),vars(n_sites),labeller = labeller(n_sites = sites.labs, n_species = spec.labs)  )+
  geom_point()+
  scale_y_continuous(name = "Mean commonness" ) + # , breaks = seq(0, 100, 2), limits = c(0, 10) ) + 
  scale_x_continuous(name ="Max iterations", breaks = c(10000, 40000), limits = c(0, 50000), labels = c("10000" = "10k", "40000" = "40k"))+
  theme_bw(base_size = 14)

# MAE_commonness
ggplot(data = df, aes(x = iterations, y = mean_real_commonness_error, col = replicate))+ # , col = replicate))+
  facet_grid(scales="fixed", vars(n_species),vars(n_sites),labeller = labeller(n_sites = sites.labs, n_species = spec.labs)  )+
  geom_point()+
  scale_y_continuous(name = expression(MAE[commonness]) )+ # , breaks = seq(0, 100, 2), limits = c(0, 10) ) + 
  scale_x_continuous(name ="Max iterations", breaks = c(10000, 40000), limits = c(0, 50000), labels = c("10000" = "10k", "40000" = "40k"))+
  theme_bw(base_size = 14)

ggsave("BCI_MAE_commonness.png", path = "figures", width = 8, height = 6, units = "in")

# Relative commonness error = MEA_commonness / mean absolute commonness 
ggplot(data = df, aes(x = iterations, y = delta_commonness, col = replicate))+ # , col = replicate))+
  facet_grid(scales="fixed", vars(n_species),vars(n_sites),labeller = labeller(n_sites = sites.labs, n_species = spec.labs)  )+
  geom_point()+
  scale_y_continuous(name = "Relative commonness error [%]") + # , breaks = seq(0, 100, 2), limits = c(0, 10) ) + 
  scale_x_continuous(name ="Max iterations", breaks = c(10000, 40000), limits = c(0, 50000), labels = c("10000" = "10k", "40000" = "40k"))+
  theme_bw(base_size = 14)

ggsave("BCI_RE_commonness.png", path = "figures", width = 8, height = 6, units = "in")

# distance D
ggplot(data = df, aes(x = iterations, y = final_distance_D, col = replicate))+ # , col = replicate))+
  facet_grid(scales="fixed", vars(n_species),vars(n_sites),labeller = labeller(n_sites = sites.labs, n_species = spec.labs)  )+
  geom_point()+
  scale_y_continuous(name = expression(Distance ~ D)) + # , breaks = seq(0, 100, 2), limits = c(0, 10) ) + 
  scale_x_continuous(name ="Max iterations", breaks = c(10000, 40000), limits = c(0, 50000), labels = c("10000" = "10k", "40000" = "40k"))+
  theme_bw(base_size = 14)

ggsave("BCI_distance.png", path = "figures", width = 8, height = 6, units = "in")

# distance D ~ mean_richness
ggplot(data = df, aes(x = mean_richness, y = final_distance_D, col = replicate))+ # , col = replicate))+
  facet_grid(scales="fixed", vars(iterations),labeller = label_both)+
  geom_point()+
  scale_y_continuous(name = "Distance D") + # , breaks = seq(0, 100, 2), limits = c(0, 10) ) + 
  scale_x_continuous(name ="Mean richness" ) + # , breaks = seq(0, 20000, 10000), limits = c(0, 20000))+
  theme_bw()

# MAE_c ~ mean_richness
ggplot(data = df, aes(x = mean_richness, y = mean_real_commonness_error, col = replicate))+ # , col = replicate))+
  facet_grid(scales="fixed", vars(iterations),labeller = label_both)+
  geom_point()+
  scale_y_continuous(name = "MAE_c") + # , breaks = seq(0, 100, 2), limits = c(0, 10) ) + 
  scale_x_continuous(name ="Mean richness" ) + # , breaks = seq(0, 20000, 10000), limits = c(0, 20000))+
  theme_bw()

# distance D ~ mean_commonness
ggplot(data = df, aes(x = mean_commonness, y = final_distance_D, col = replicate))+ # , col = replicate))+
  facet_grid(scales="fixed", vars(iterations),labeller = label_both)+
  geom_point()+
  scale_y_continuous(name = "Distance D") + # , breaks = seq(0, 100, 2), limits = c(0, 10) ) + 
  scale_x_continuous(name ="Mean commonness" ) + # , breaks = seq(0, 20000, 10000), limits = c(0, 20000))+
  theme_bw()

# MAE_c ~ mean_richness
ggplot(data = df, aes(x = mean_commonness, y = mean_real_commonness_error, col = replicate))+ # , col = replicate))+
  facet_grid(scales="fixed", vars(iterations),labeller = label_both)+
  geom_point()+
  scale_y_continuous(name = "MAE_c") + # , breaks = seq(0, 100, 2), limits = c(0, 10) ) + 
  scale_x_continuous(name ="Mean commonness" ) + # , breaks = seq(0, 20000, 10000), limits = c(0, 20000))+
  theme_bw()
