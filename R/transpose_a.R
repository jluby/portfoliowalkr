#' Transpose A
#' 
#' This function transposes the universe parameter,
#' allowing for greater flexibility of walkr usage 
#' and the use of factor variables.
#' 
#' @param universe TODO
#' 
#' @return A, n characteristics in height, for walkr computation 
#' 
#' @examples
#' \dontrun{
#' TODO
#' }


transpose_a <- function(universe, match) {
  
  # Comment TODO
  A <- universe %>%
    select(match) %>%
    data.table::data.table() %>% 
    mltools::one_hot() %>%
    t()
  
  return(A)
}