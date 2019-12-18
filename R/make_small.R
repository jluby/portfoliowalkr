#' Make Small Universe
#' 
#' This function creates a simplified universe for cases where
#' not all factor variables are represented or only numeric values
#' at the edge of the distribution are represented in the portfolio.
#' These distributions flatten the polytope and make it impossible for
#' walkr to properly sample, so they must be removed before sampling
#' then later reattached.
#' 
#' @param universe is the set of stocks available in the universe,
#' with weights
#' @param match TODO
#' 
#' @return TODO
#' 
#' @examples
#' \dontrun{
#' TODO
#' }


make_small <- function(universe, match, portfolio.weight) {
  
  universe_small <- universe
  
  universe_small[[portfolio.weight]] <- universe_small[[portfolio.weight]] / sum(universe_small[[portfolio.weight]])
  
  for (i in match) {
    if (class(universe_small[[i]]) == "factor") {
      # Any categorical which is completely unrepresented must go
       keep_list <- universe %>% 
         select(i, portfolio.weight) %>% 
         filter(!!as.name(portfolio.weight) != 0) %>% 
         select(1) %>% 
         unique() %>% 
         pull() %>% 
         as.character()
       
       universe_small <- universe_small %>% 
         filter(!!as.name(i) %in% keep_list)
    } else {
      # Any variable whose exposure sits on the edge of its own distribution
      # can only be considered in terms of those edge cases, and we will not
      # need to match later
      max_min <- universe_small %>% 
        select(i, portfolio.weight) %>% 
        summarize(max = max(!!as.name(i)),
                  min = min(!!as.name(i)))
      
      weighted_mean <- universe_small %>%
        select(i, portfolio.weight) %>%
        mutate_at(vars(i), ~ .x * !!as.name(portfolio.weight)) %>% 
        summarize(weighted_mean = sum(!!as.name(i)))
      
      if (weighted_mean %in% max_min) {
        universe_small <- universe_small %>% 
          filter(!!as.name(i) == pull(weighted_mean)) 
        
        match <- match[match != i]
      }
    }
  }
  
  small_set <- list("universe_small" = universe_small, "match" = match)
  
  return(small_set)
}