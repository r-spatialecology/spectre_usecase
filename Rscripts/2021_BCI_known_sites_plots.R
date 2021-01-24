### Plot BCI "known sites" results 

in_dir <- "./data/BCI_known_sites"

library(ggplot2)
library(tidyverse)

df<- list.files(path =in_dir, pattern = "*.rds", full.names = TRUE) %>%
  map(readRDS) %>%
  bind_rows()

df$replicate <- as.factor(df$replicate)
df$known_sites <- as.factor(df$known_sites)

# Correctly predicted species
# Correctly predicted species
ggplot(data = df, aes(x = known_sites, y = correctly_predicted_species))+ # , col = replicate))+
  scale_y_continuous(name = "Correctly predicted species [%]", breaks = seq(0, 100, 20), limits = c(0, 100) ) + 
  scale_x_discrete(name ="n known sites", breaks = seq(0, 40, 10))+
  #  geom_jitter(width = 0.2) + 
  geom_boxplot()+ 
  ggsci::scale_color_jco() +
  ggthemes::theme_tufte(base_size = 12)
ggsave("BCI_known_sites_correctly_boxplot.png", path = "figures/", width = 8, height = 6, units = "in")

