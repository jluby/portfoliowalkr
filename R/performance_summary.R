#' Performance Summary
#' 
#' This function creates a summary table of the target 
#' portfolio's performance
#' 
#' @param returns is a dataframe of portfolio returns
#' @param points is the number of randomly sampled portfolios
#' 
#' @return summary, a list with figures describing portfolio performance
#' 
#' @examples
#' \dontrun{
#' summary <- performance_summary(returns, points)
#' }


performance_summary <- function(returns, points) {
  
  # Since we created the returns frame in order, we can select params by position
  random_returns <- returns[3:(points+2),1]
  percentile <- ecdf(random_returns)
  
  # Save summary weights and percentiles for list
  even_weight <- c(returns[1,1], percentile(returns[1,1]))
  names(even_weight) <- c("Return", "Percentile Rank")
  target_weight <- c(returns[2,1], percentile(returns[2,1]))
  names(target_weight) <- c("Return", "Percentile Rank")

  # Save list
  summary <- list("Target" = even_weight,
       "Universe" = target_weight,
       "Quantiles" = quantile(random_returns))
    
  
  return(summary)
  
}