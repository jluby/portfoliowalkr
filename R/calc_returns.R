#' Calculate Portfolio Returns
#' 
#' This function calculates the returns of all of the portfolios
#' considered, adding in a portfolio with even weight across the
#' universe for comparison
#' 
#' @param weighted_universe is the universe parameter with characteristic-matching random weights joined
#' @param universe is the universe of assets and their characteristics
#' @param portfolio.weight is the name of the column which contains the target portfolio weights (for calculating exposures)
#' @param ret.var is the name of the column which contains the returns of each asset
#' @param points is the number of randomly sampled portfolios
#' 
#' @return returns, a data.frame of each portfolio's return
#' 
#' @examples
#' \dontrun{
#' returns <- calc_returns(weighted_universe, universe, portfolio.weight, ret.var, points)
#' }


calc_returns <- function(weighted_universe, universe, portfolio.weight, ret.var, points) {
  
  returns <- weighted_universe %>%
    mutate(even_weighting = sum(!!as.name(portfolio.weight))/n()) %>%
    select(ret.var, even_weighting, portfolio.weight, (length(universe)):(length(universe)+points)) %>% 
    mutate_at(vars(-ret.var), ~.x / sum(universe[portfolio.weight])) %>%
    mutate_at(vars(-ret.var), ~.x * !!as.name(ret.var)) %>%
    summarise_at(vars(-ret.var), sum) %>%
    t() %>% 
    as.data.frame()
  
  colnames(returns) <- "portfolio_return"
  
  return(returns)
  
}