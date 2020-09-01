#' @title calculate_rec
#' 
#' @description The function calculates the relative commonness error
#' 
#' @param target The target (commonness matrix) that was provided for the spectre optimization algorithm
#' @param optimized_grid The final grid (species X site) that was reported by the spectre algoithm
#' 
#' @details 
#' 
#' The function first calculates the mean commonness of the target and the commonness of the solution:
#' target_commonness <- mean(abs(target))
#' solution_commonness <- spectre:::calculate_solution_commonness_rcpp(ptimized_grid)
#' 
#' The relative error in commonness (REC) is than calculated as:
#' rec <- mean(abs(solution_commonness - target_commonness), na.rm = TRUE)
#' 
#' 
#' @return rec relative commonness error
#' @examples 
#' \dontrun{
#' 
#' 
#' 
#' }
#' @export

calculate_rec <- function(target, optimized_grid)
{
  target_commonness <- mean(abs(target), na.rm = TRUE)
  solution_commonness <- spectre:::calculate_solution_commonness_rcpp(optimized_grid)
  rec <- mean(abs(solution_commonness - target_commonness), na.rm = TRUE)
  
  return(rec)
}
