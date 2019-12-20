#' Transpose b 
#' 
#' This function transposes creates parameter b for 
#' use with walkr. The function calculates the target
#' portfolio's exposures and returns a transposed frame
#' for matching
#' 
#' @param universe_small is the set of stocks available in the 
#' universe (with redundant assets removed), with target weights
#' @param match is the vector of column names which contain the
#' characteristics we'd like to match
#' @param portfolio.weight is the name of the column which contains
#' the target portfolio weights (for calculating exposures)
#' 
#' @return b, a frame of target portfolio exposures
#' 
#' @examples
#' \dontrun{
#' b <- transpose_b(universe_small, match, portfolio.weight)
#' }


transpose_b <- function(universe_small, match, portfolio.weight) {
  
  b <- universe_small %>% 
    
    # Select matching variables
    select(match, portfolio.weight) %>% 
    
    # Establish data.table for one_hot encoding
    data.table::data.table() %>% 
    mltools::one_hot() %>%
    
    # Portfolio exposures are equal to sum of asset's % weights 
    # multiplied by their exposures
    mutate_at(vars(-portfolio.weight), ~ .x * !!as.name(portfolio.weight)) %>%
    select(-portfolio.weight) %>%
    summarise_all(list(sum)) %>%
    
    # Transpose for walkr calculations
    t()
  print(b)
  
  return(b)
  
}