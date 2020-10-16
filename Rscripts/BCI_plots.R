### Plot BCI results

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

# mean absolute error in commonness (MAE_c)
ggplot(data = df, aes(x = iterations, y = MAE_c))+ # , col = replicate))+
  facet_grid(scales="fixed", vars(species),vars(sites),labeller = label_both)+
#  geom_jitter(width = 0.2)+
 geom_boxplot()+
    scale_y_continuous(name = expression(MAE[c]), limits = c(0, max(df$MAE_c)) ) + 
  scale_x_discrete(name ="Max iterations") + 
  theme_bw(base_size = 14)
ggsave("BCI_MAE_c.png", path = "figures/", width = 8, height = 6, units = "in")

# relative commonness error (RCE)
ggplot(data = df, aes(x = iterations, y = RCE))+ # , col = replicate))+
  facet_grid(scales="fixed", vars(species),vars(sites),labeller = label_both)+
 # geom_jitter(width = 0.1, col = "gray")+
  geom_boxplot()+
  scale_y_continuous(name = "RCE [%]", limits = c(0, 8) )+ 
  scale_x_discrete(name ="Max iterations") +
  theme_bw(base_size = 14)

ggsave("BCI_RCE.png", path = "figures/", width = 8, height = 6, units = "in")


