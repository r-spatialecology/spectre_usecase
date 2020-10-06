#' @title calculate_rce
#' 
#' @description The function calculates the relative commonness error
#' 
#' @param target The target (commonness matrix) that was provided for the spectre optimization algorithm
#' @param optimized_grid The final grid (species X site) that was reported by the spectre algoithm
#' 
#' @details 
#' 
#' 
#' @return rce relative commonness error
#' @examples 
#' \dontrun{
#' 
#' 
#' 
#' }
#' @export

calculate_rce <- function(target, optimized_grid)
{
  
  solution_commonness <- spectre:::calculate_solution_commonness_rcpp(optimized_grid)
  target_commonness <- target 
  
  # Calculate mean:
  target_mean_commonness <- calculate_mc(target)
  
  # Calculate mean difference:
  rce <- mean(abs(solution_commonness - target_commonness), na.rm = TRUE)
  
  # Weight rleative to target mean:
  rce <- rce / target_mean_commonness * 100
  
  return(rce)
}
