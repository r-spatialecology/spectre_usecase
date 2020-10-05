#' @title calculate_mc
#' 
#' @description The function calculates the mean commonness of the target
#' 
#' @param target The target (commonness matrix) that was provided for the spectre optimization algorithm
#' 
#' @details 
#' 
#' @return mc mean commonness
#' @examples 
#' \dontrun{
#' 
#' 
#' 
#' }
#' @export

calculate_mc <- function(target)
{
  # Calculate mean:
  target_mean_commonness <- mean(abs(target), na.rm = TRUE)
  return(target_mean_commonness)
}
