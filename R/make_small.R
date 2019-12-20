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
#' with target weights
#' @param match is the vector of column names which contain the
#' characteristics we'd like to match
#' @param portfolio.weight is the name of the column which contains
#' the target portfolio weights (for calculating exposures)
#' 
#' @return list small_set which contains (1) the universe_small with
#' only relevant assets for matching and (2) a revised match vector
#' if we no longer want to match on a column
#' 
#' @examples
#' \dontrun{
#' small_set <- make_small(universe, match, portfolio.weight)
#' }


make_small <- function(universe, match, portfolio.weight) {
  
  universe_small <- universe
  
  # Establish weights as a percentage of the total
  universe_small[[portfolio.weight]] <- universe_small[[portfolio.weight]] / sum(universe_small[[portfolio.weight]])
  
  # Iterate through matching variables
  for (i in match) {
    # If the variable is categorical, we'll need to remove it before one_hot encoding
    if (class(universe_small[[i]]) == "factor") {
      # Establish the list of categoricals which are represented in the frame
       keep_list <- universe %>% 
         
         # Select categorical and weights
         select(i, portfolio.weight) %>% 
         
         # Filter down to assets which have non-zero weight
         filter(!!as.name(portfolio.weight) != 0) %>% 
         
         # Select the first column and take unique values
         # Saving that output as a character list to check against
         select(1) %>% 
         unique() %>% 
         pull() %>% 
         as.character()

       # Only keep assets whose category (ie. sector) is represented in target
       # Otherwise that asset's weight must be zero
       universe_small <- universe_small %>% 
         filter(!!as.name(i) %in% keep_list)

    } else {
      # Any numeric variable whose exposure sits on the edge of its own distribution
      # can only be considered in terms of those edge cases. All others can be
      # filtered out and set to zero later.
      
      # Select numerical matching variable and summarize its max and min values
      max_min <- universe_small %>% 
        select(i, portfolio.weight) %>% 
        summarize(max = max(!!as.name(i)),
                  min = min(!!as.name(i)))
      
      # Establish the portfolio's exposure to this characteristic
      weighted_mean <- universe_small %>%
        select(i, portfolio.weight) %>%
        mutate_at(vars(i), ~ .x * !!as.name(portfolio.weight)) %>% 
        summarize(weighted_mean = sum(!!as.name(i)))
      
      # If exposure is equal to the max, min, or both, then we filter
      # out any assets which do not have that value and we'll set to
      # zero when we reattach
      if (weighted_mean %in% max_min) {
        universe_small <- universe_small %>% 
          filter(!!as.name(i) == pull(weighted_mean)) 
        
        # Remove this variable from matching, because we now only have
        # a set of assets with this exact value on which to match
        match <- match[match != i]
      }
    }
  }
  
  # Make ordered factors unordered for one_hot encoding later
  # and to prevent some strange behavior along the way
  universe_small <- universe_small %>%
    mutate_if(is.factor, ~factor(.,ordered = FALSE))
  
  # Establish new universe_small and list of matching variables for later
  small_set <- list("universe_small" = universe_small, "match" = match)
  
  return(small_set)
}