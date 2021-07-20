# Create plot for error ~ iteration 

in.dir <- "./data/figure2"
out.dir <- "figures/"

library("ggplot2")
library("ggthemes")
library("ggsci")

df <- readRDS(paste0(in.dir,"/BCI100Sites100SpeciesForPlotting.rds"))

new_data <- data.frame(error = df$res_min_conf$error$error,
                       iteration = df$res_min_conf$error$i)


(plot1 <- ggplot(data = new_data, aes(x = iteration, y = error))+ 
    geom_point(size = 0.4)+
    labs(title = "",
         x = "Iterations",
         y = "Error")+
    xlim(0, 50000)+ # I cut the iterations at 50k since nothing interesting happened thereafter (60k were run... )
    ggsci::scale_color_jco() +
    ggthemes::theme_tufte(base_size = 12))+
    theme(axis.line = element_line()) 

ggsave(paste0(out.dir, "BCI_figure2.png"),  width = 6, height = 6, dpi = 300)
# ggsave(filename="min_energy.png", path=plotdir, width=6, height=6, dpi=300) # copy from Jan's Figure Code
  