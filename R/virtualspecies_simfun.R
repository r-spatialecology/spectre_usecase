#' @title virtualspecies_simfun
#' 
#' @description Simulation function to run several virtualspecies grids in an experiment (e.g. through the Q function)
#' 
#' @param siminputrow define which row of the parameters tibble should be computed
#' @param parameters a parameter matrix containing details on the virtualspecies parameterization (see Details below)
#' @param max_runs maximum number of runs in spectre::run_optimization_min_conf()
#' @param autostop optimizer stops after this number of iterations with no improvement
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
#'                         writeRDS = FALSE,
#'                         outdir = dir.cloud)
#'                         })
#' 
#' }
#' @export

virtualspecies_simfun <- function(siminputrow, parameters, max_runs, autostop, writeRDS, outdir)
{
  t0_start_simfun <- Sys.time()
  
  # Read and set parameters
  p <- parameters[siminputrow,]
  set.seed(p$random_seed)
  
  # Construct virtual species:
  spp <- generate_data_virtualspecies(ncol=p$landscape_size, nrow=p$landscape_size,
                                      corr_within = p$corr_within, 
                                      corr_among = p$corr_among, 
                                      gamma = p$gamma, 
                                      beta = p$beta)
  
  # Calculate alpha:
  alpha <- raster::getValues(sum(spp))
  
  # Construct solution and target:
  solution <- generate_data_virtualspecies_to_solution(spp)
  target <- spectre:::calculate_solution_commonness_rcpp(solution)
  
  t1_end_data_preparation <- Sys.time()
  
  # Run spectre:
  res_min_conf <- spectre::run_optimization_min_conf(alpha_list = alpha, 
                                                     total_gamma = p$gamma, 
                                                     target = target, 
                                                     max_iterations = max_runs,
                                                     autostop = autostop,
                                                     verbose = FALSE,
                                                     interruptible = FALSE)
  t2_end_spectre <- Sys.time()
  
  # Create tibble
  result <- tibble::tibble(spp.virtual = list(spp),
                           spp.spectre = list(res_min_conf),
                           bench_total = t2_end_spectre - t0_start_simfun,
                           bench_prep = t1_end_data_preparation - t0_start_simfun,
                           bench_spectre = t2_end_spectre - t1_end_data_preparation)
  
  # Combine with input
  result <- cbind(p, result)
  
  # Write output
  if(isTRUE(writeRDS)) {
    saveRDS(result, file.path(outdir, paste0("spectre_", siminputrow, ".rds")))
  }
  
  return(result)
}
