# Script to produce Fig. 2 in the spectre manuscript 
# Simpkins et al. () intended to be published in MEE
# ==================================================

# We are thankful for the permission to use the Barro Colorado data set.
# Condit R., Perez, R., Aguilar, S., Lao, S., Foster, R., Hubbell, S.P. 2019. 
# Complete data from the Barro Colorado 50-ha plot: 423617 trees, 35 years, 2019 version.   
# https://doi.org/10.15146/5xcp-0d46.

library("spectre")
library("foreach")

# set parameters 
n_sites <- c(100) # how large is the BCI subset used
n_species <- c(100) # how many species are sampled from subset of the BCI data 
max_iterations <- c(50000) # how many iterations of the algorithm
seed = 123
set.seed(seed)

# load BCI data
data <- readRDS("data/BCI_tree8_MSP.rds") 
print(paste0(dim(data)[1], " species, ",  dim(data)[2], " sites"))

# create random BCI subsample
sampled_data <- matrix(nrow = n_species, ncol = n_sites, data = 0)

if(n_sites > dim(data)[2]) { # sites in columns 
  n_sites <- dim(data)[2] 
  print("Could not sample all requested sites: you reached maximum number of sites... ")
}
sampled_sites <-  sample(dim(data)[2], n_sites, replace = FALSE) 

if (n_species > dim(data)[1]) { # species in rows
  n_species <- dim(data)[1]
  print("Could not sample all requested species: You reached maximum number of species ... ")
}

temp_data <- data[, sampled_sites]

# check number of species in subsample 
available_species <- which(rowSums(temp_data) > 0)
n_available_species <- length(available_species)

if (n_available_species >= n_species){
  
  # check that sampled sites have a commonness > 1 (very small samples could have only sites that share no species)
  sum_commonness <- 0
  while (sum_commonness < 1) {
    sampled_species <-  sample(available_species, n_species, replace = FALSE) 
    sampled_data[1:n_species, ] <- temp_data[sampled_species, ]
    
    # check commonness of sampled sites
    (objective_commonness_matrix <- spectre:::calculate_solution_commonness_rcpp( sampled_data ) )
    sum_commonness <- sum(objective_commonness_matrix, na.rm = TRUE)
    print(paste0("Sum commonness is = ", sum_commonness))
  }
  
  print(paste0(dim(sampled_data)[1], " species, ",  dim(sampled_data)[2], " sites"))
  
  # get crucial information from sampled data
  (alpha_list <- colSums(sampled_data))
  (total_gamma <- sum(rowSums(sampled_data) > 0))
  
  res_min_conf <- spectre::run_optimization_min_conf(alpha_list = alpha_list, 
                                                     total_gamma = total_gamma, 
                                                     target = objective_commonness_matrix, 
                                                     max_iterations = max_iterations,
                                                     verbose = TRUE,
                                                     seed = seed) 
  
}

# Produce Fig.2 for the manuscript
# ================================
png("figures/Fig2.png")
spectre::plot_error(res_min_conf)
dev.off()
