#' Transpose b 
#' 
#' This function transposes parameter b.
#' Further, it is able to accept a frame
#' with factor variables and automatically
#' calculate exposures for walkr computation.
#' 
#' @param universe TODO
#' 
#' @return TODO 
#' 
#' @examples
#' \dontrun{
#' TODO
#' }


transpose_b <- function(universe, match, portfolio.weight) {
  
  # Comment TODO
  universe[[portfolio.weight]] <- universe[[portfolio.weight]] / sum(universe[[portfolio.weight]])
  
  b <- universe %>% 
    select(match, portfolio.weight) %>% 
    data.table::data.table() %>% 
    mltools::one_hot() %>%
    mutate_at(vars(-portfolio.weight), ~ .x * !!as.name(portfolio.weight)) %>%
    select(-portfolio.weight) %>%
    summarise_all(list(sum)) %>%
    t()
  
  return(b)
  
}