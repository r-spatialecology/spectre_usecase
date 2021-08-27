# Script 1 to produce Fig. 4 in the spectre manuscript 
# Simpkins et al. () intended to be published in MEE
# ==================================================

# Predicting specific species for specific sites, using 100 random sites from the BCI data set.
# Tree species compositions from n sites are copied directly into the siteXspecies solution matrix.
# These "known species" at "known sites" are fixed, only species at "unknown sites" will be optimized.
# We evaluate how the inclusion of "known species" at "known sites" increases the number of correctly 
# predicted species at specific sites (script to reproduce Figure 4 of the manuscript)

# We are thankful for the permission to use the Barro Colorado data set.
# Condit R., Perez, R., Aguilar, S., Lao, S., Foster, R., Hubbell, S.P. 2019. 
# Complete data from the Barro Colorado 50-ha plot: 423617 trees, 35 years, 2019 version.   
# https://doi.org/10.15146/5xcp-0d46.


# install.packages("spectre")
library("spectre")
library("foreach")

out_dir <- "data/Fig4_data"
dir.create(file.path(out_dir))

doParallel::registerDoParallel(24)

# set parameters 
replicate <- 1:25 # replicates per parameter combination
n_sites <- 100 # how large is the BCI subset used
n_species <- 100  # how many species are sampled subset of the BCI data 
known_sites <- c(seq(0, 40, 5)) # all species from how many sites are "fixed"?
max_iterations <- 50000 # how many iterations of the algorithm
verbose = TRUE

parameters <- tidyr::crossing(replicate, 
                              n_sites,
                              known_sites,
                              n_species,
                              max_iterations,
                              verbose
)
parameters$siminputrow <- 1:dim(parameters)[1]

# load BCI data set
data <- readRDS("data/BCI_tree8_MSP.rds") 
print(paste0(dim(data)[1], " species, ",  dim(data)[2], " sites")) # check orientation of the data

sim_fun <- function(siminputrow, parameters, writeRDS, verbose) 
{
  # Read and set parameters 
  p <- parameters[siminputrow, ]
  if(!( dim(p)[1] == 1)) {
    p <- parameters[1, ] # only used for quick testing
  }
  (n_sites <- p$n_sites)
  (n_species <- p$n_species) 
  (known_sites <- p$known_sites)
  replicate <- p$replicate
  max_iterations <- p$max_iterations
  #energy_threshold <- p$energy_threshold 
  verbose <- p$verbose
  siminputrow <- p$siminputrow
  seed <- siminputrow
  set.seed(seed)
  
  # create BCI subsample
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
      (target_commonness <- spectre:::calculate_solution_commonness_rcpp( sampled_data ) )
      sum_commonness <- sum(target_commonness, na.rm = TRUE)
      print(paste0("Sum commonness is = ", sum_commonness))
    }
    
    print(paste0(dim(sampled_data)[1], " species, ",  dim(sampled_data)[2], " sites"))
    
    # get crucial information from sampled data
    (alpha_list <- colSums(sampled_data))
    (total_gamma <- sum(rowSums(sampled_data) > 0))
    mean_richness <- mean(alpha_list)
    mean_commonness <- mean(abs(target_commonness), na.rm = TRUE)
    
    if (known_sites > 0){
      
      # fixed species at random sites
      fixed_sites <- sample(1:n_sites, known_sites, replace = FALSE)
      fixed_species <- matrix(data = 0, nrow = total_gamma, ncol = n_sites)
      fixed_species[, fixed_sites] <- sampled_data[, fixed_sites]
      
      # observed richness at fixed sites may be > predicted richness at that site,
      # this would cause spectre to be caught in a loop
      if(any(alpha_list < colSums(fixed_species))){
        # fix richness in alpha list to match number of fixed species
        alpha_list[alpha_list < colSums(fixed_species)] <- colSums(fixed_species)[alpha_list < colSums(fixed_species)]
        print("More fixed species than richness at site(s), thus richness was corrected to be the number of fixed species there")
      }else{
        print("Everything is OK :-)  ")
      }
      
      time_before <- Sys.time()
      res_min_conf <- spectre::run_optimization_min_conf(alpha_list = alpha_list, 
                                                         total_gamma = total_gamma, 
                                                         target = target_commonness, 
                                                         fixed_species = fixed_species,
                                                         partial_solution = fixed_species,
                                                         max_iterations = max_iterations,
                                                         # energy_threshold = energy_threshold,
                                                         verbose = verbose,
                                                         seed = seed,
                                                         interruptible = TRUE) # 
      time_after <- Sys.time()
      (calc_time <- time_after - time_before)
      
      
      # only keep sites that have no fixed species
      sampled_data <- sampled_data[, - fixed_sites]
      
      res_min_conf$optimized_grid <- res_min_conf$optimized_grid[, -fixed_sites]
      
      # re-calculate target commonness 
      (target_commonness <- spectre:::calculate_solution_commonness_rcpp( sampled_data ) )
      
      
    } else {
      
      # no fixed species at random sites
      time_before <- Sys.time()
      res_min_conf <- spectre::run_optimization_min_conf(alpha_list = alpha_list, # check wheter both algorithms are the same here!
                                                         total_gamma = total_gamma, 
                                                         target = target_commonness, 
                                                         max_iterations = max_iterations,
                                                         #energy_threshold = energy_threshold,
                                                         verbose = verbose,
                                                         seed = seed,
                                                         interruptible = TRUE) # 
      time_after <- Sys.time()
      (calc_time <- time_after - time_before)
    }
    
    # how many species were predicted correctly?
    n_correctly_predicted <- sum(  which(res_min_conf$optimized_grid == 1) %in%  which(sampled_data == 1))
    n_observed_species <- length(which(sampled_data == 1))
    correctly_predicted_species <- n_correctly_predicted / n_observed_species * 100 
    
    print(paste0(correctly_predicted_species, " % correctly predicted"))
    
    # MAE_c
    solution_commonness <- spectre:::calculate_solution_commonness_rcpp(res_min_conf$optimized_grid)
    # bla <- res_min_conf$optimized_grid
    # class(bla)
    # spectre::calc_commonness_error(bla, target_commonness)
    # spectre::calc_commonness_error(res_min_conf$optimized_grid)
    MAE_c <- mean(abs(solution_commonness - target_commonness), na.rm = TRUE) 
    RCE <- MAE_c / mean(abs(target_commonness), na.rm = TRUE) * 100 
    
    r_1 <- tibble::tibble(replicate = replicate,
                          n_sites = n_sites,
                          known_sites = known_sites,
                          n_species = n_species,
                          correctly_predicted_species = correctly_predicted_species,
                          MAE_c = MAE_c,
                          RCE = RCE,
                          interations = max_iterations,
                          time = calc_time)
    
    saveRDS(r_1, file = paste0(out_dir, "/res", siminputrow, ".rds")) 
  }
}

foreach(REPLICATE = 1:dim(parameters)[1], .export = c("p"), .packages = c("spectre")) %dopar% {
  print(paste0("Replicate: ", REPLICATE))
  
  sim_fun(REPLICATE,
          parameters,
          TRUE,
          TRUE)
  return(1)
} 
