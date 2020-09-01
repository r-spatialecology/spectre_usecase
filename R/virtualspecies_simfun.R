#' @title virtualspecies_simfun
#' 
#' @description Simulation function to run several virtualspecies grids in an experiment (e.g. through the Q function)
#' 
#' @param siminputrow define which row of the parameters tibble should be computed
#' @param parameters a parameter matrix containing details on the virtualspecies parameterization (see Details below)
#' @param max_runs maximum number of runs in spectre::run_optimization_min_conf()
#' @param energy_threshold energy threshold in spectre::run_optimization_min_conf()
#' @param writeRDS TRUE/FALSE, if TRUE, each run weill be stored as rds (can be useful when performing calculations on the HPC)
#' @param outdir directory for storing rds files (only used when writeRDS = TRUE)
#' 
#' @details 
#' 
#' This function is basically a complex wrapper around `spectre::run_optimization_min_conf()`.
#' However, this function allows to provide a parameter matrix to generate virtualspecies grids.
#' 
#' The parameters tibble can contain multiple experiments (parameterization) and the siminputrow value defines which one is executed.
#' The parameter tibble need to contain the following columns (parameters):
#' landscape_size ~ size of the generated virtualspecies landscape
#' corr_within  ~ correlation within virtual species (clustering of individuals of the same species)
#' gamma ~ total gamma of the generated landscape
#' beta ~ width of the distribution kernel of the virtualspecies
#' corr_among ~ correlation among species (clustering of different species at the same location)
#' random_seeds ~ random seeds to control stochasticity
#' 
#' 
#' @return spectre results object
#' @examples 
#' \dontrun{
#' 
#' ## Here is an example of using the simfun locally:
#' 
#' library(future)
#' plan(multisession)
#' 
#' results <- furrr::future_map_dfr(seq(nrow(parameters)), function(x) {
#'   virtualspecies_simfun(siminputrow = x, 
#'                         parameters = parameters,
#'                         max_runs = max_runs,
#'                         energy_threshold = energy_threshold,
#'                         writeRDS = FALSE,
#'                         outdir = dir.cloud)
#'                         })
#' 
#' }
#' @export

virtualspecies_simfun <- function(siminputrow, parameters, max_runs, energy_threshold, writeRDS, outdir)
{
  # Read and set parameters
  p <- parameters[siminputrow,]
  set.seed(p$random_seed)
  
  # Construct virtual species:
  spp <- spectre::generate_data_virtualspecies(ncol=p$landscape_size, nrow=p$landscape_size,
                                               corr_within = p$corr_within, 
                                               corr_among = p$corr_among, 
                                               gamma = p$gamma, 
                                               beta = p$beta)
  
  # Calculate alpha:
  alpha <- raster::getValues(sum(spp))
  
  # Construct solution and target:
  solution <- spectre::generate_data_virtualspecies_to_solution(spp)
  target <- spectre:::calculate_solution_commonness_rcpp(solution)
  
  #print("start")
  start_time <- Sys.time()
  res_min_conf <- spectre::run_optimization_min_conf(alpha, p$gamma, target, max_runs, energy_threshold)
  end_time <- Sys.time()
  time_minconf <- as.numeric(end_time - start_time)
  
  # Create tibble
  result <- tibble::tibble(spp.virtual = list(spp),
                           spp.spectre = list(res_min_conf),
                           benchmark = time_minconf)
  
  # Combine with input
  result <- cbind(p, result)
  
  # Write output
  if(isTRUE(writeRDS)) {
    saveRDS(result, file.path(outdir, paste0("spectre_", siminputrow, ".rds")))
  }
  
  return(result)
}
