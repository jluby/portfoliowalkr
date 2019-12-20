#' Check Exposures
#' 
#' This function checks a random number of matched columns
#' from the universe to demonstrate that exposures are the same
#' 
#' @param weighted_universe is the $frame output of portfoliowalkr
#' @param match is the vector of column names which contain the
#' characteristics whose exposures we'd like to compare
#' @param portfolio.weight is the name of the column which contains
#' the target portfolio weights (for calculating exposures)
#' @param n is the number of weighted portfolios we'd like to compare
#' 
#' @return exposures, a frame of portfolio exposures
#' 
#' @examples
#' \dontrun{
#' exposures <- check_exposures(full_list$frame, match = c('growth', 'size', 'sector'), portfolio.weight = "portfolio", n = 3)
#' }


check_exposures <- function(weighted_universe, match, portfolio.weight, n) {
  
  exposures <- weighted_universe %>% 
    mutate_if(is.factor, ~factor(.x, ordered = FALSE)) %>% 
    select(match, portfolio.weight) %>%
    data.table::data.table() %>% 
    mltools::one_hot() %>%
    mutate_at(vars(-portfolio.weight), ~ (.x * !!as.name(portfolio.weight))/sum(!!as.name(portfolio.weight))) %>%
    select(-portfolio.weight) %>%
    summarise_all(list(sum)) %>% 
    t() %>% 
    as.data.frame() %>% 
    set_names(portfolio.weight)
  
  points <- length(grep(x = colnames(weighted_universe), pattern = "weight[0-9]"))
  
  names <- row.names(exposures)

  sample_points <- sample(points, n) %>% 
    sort()
  
  for (i in sample_points) {
    col <- paste0('weight', i)
    
    random_exposure <- weighted_universe %>% 
      mutate_if(is.factor, ~factor(.x, ordered = FALSE)) %>% 
      select(match, col) %>%
      data.table::data.table() %>% 
      mltools::one_hot() %>%
      mutate_at(vars(-col), ~ (.x * !!as.name(col))/sum(!!as.name(col))) %>%
      select(-col) %>%
      summarise_all(list(sum)) %>% 
      t() %>% 
      as.data.frame() %>% 
      set_names(col)
    
    exposures <- bind_cols(exposures, random_exposure)
  }
  
  row.names(exposures) <- names
  
  return(exposures)
  
}