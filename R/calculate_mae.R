#' @title calculate_mae
#' 
#' @description The function calculates the real commonness error
#' 
#' @param target The target (commonness matrix) that was provided for the spectre optimization algorithm
#' @param optimized_grid The final grid (species X site) that was reported by the spectre algoithm
#' 
#' @details 
#' 
#' @return mae real commonness error
#' @examples 
#' \dontrun{
#' 
#' 
#' 
#' }
#' @export

calculate_mae <- function(target, optimized_grid)
{
  solution_commonness <- spectre:::calculate_solution_commonness_rcpp(optimized_grid)
  target_commonness <- target 
  
  # Calculate mean difference:
  mae <- mean(abs(solution_commonness - target_commonness), na.rm = TRUE)
  
  return(mae)
}
