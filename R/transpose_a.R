#' Transpose A
#' 
#' This function transposes the universe parameter, one_hot
#' encoding any factor variables.
#' 
#' @param universe_small is the set of stocks available in the 
#' universe (with redundant assets removed), with target weights
#' @param match is the vector of column names which contain the
#' characteristics we'd like to match
#' 
#' @return A, n characteristics in height, for walkr computation 
#' 
#' @examples
#' \dontrun{
#' A <- transpose_a(universe_small, match)
#' }


transpose_a <- function(universe_small, match) {
  
  A <- universe_small %>%
    
    # Select only the variables on which we want to match
    select(match) %>%
    
    # Establish as data.table for one_hot encoding factors
    data.table::data.table() %>% 
    mltools::one_hot() %>%
    
    # Transpose for use in walkr
    t()
  print(A)
  
  return(A)
}