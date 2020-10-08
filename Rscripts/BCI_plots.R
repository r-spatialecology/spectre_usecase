### Plot results 

library(ggplot2)
library(tidyverse)

df<- list.files(path =("./BCI_res"), pattern = "*.rds", full.names = TRUE) %>%
  map(readRDS) %>%
  bind_rows()

df$replicate <- as.factor(df$replicate)
df$species <- as.factor(df$n_species)
df$sites <- as.factor(df$n_sites)
df$iterations <- as.factor(df$iterations)
names(df)

# MAE_c
ggplot(data = df, aes(x = iterations, y = MAE_c))+ # , col = replicate))+
  facet_grid(scales="fixed", vars(species),vars(sites),labeller = label_both)+
  geom_boxplot()+
  scale_y_continuous(name = expression(MAE[c]), limits = c(0, max(df$MAE_c)) ) + # , breaks = seq(0, 100, 2), limits = c(0, 10) ) + 
  scale_x_discrete(name ="Max iterations") + #, breaks = c(10000, 40000), limits = c(0, 50000), labels = c("10000" = "10k", "40000" = "40k"))+
  theme_bw(base_size = 14)
ggsave("BCI_MAE_c.png", path = "figures/", width = 8, height = 6, units = "in")

# RCE
ggplot(data = df, aes(x = iterations, y = RCE))+ # , col = replicate))+
  geom_boxplot()+
  scale_y_continuous(name = "RCE [%]", limits = c(0, max(df$RCE)) )+ # , breaks = seq(0, 100, 2), limits = c(0, 10) ) + 
  scale_x_discrete(name ="Max iterations") + #, breaks = c(10000, 40000), limits = c(0, 50000), labels = c("10000" = "10k", "40000" = "40k"))+
  theme_bw(base_size = 14)

ggsave("BCI_RCE.png", path = "figures/", width = 8, height = 6, units = "in")


