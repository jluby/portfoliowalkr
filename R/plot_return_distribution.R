#' Plot Return Distribution
#' 
#' This function creates a ggplot histogram of the sampled 
#' portfolio distributions against an evenly weighted portfolio
#' and the target portfolio
#' 
#' @param returns is a dataframe of portfolio returns
#' @param points is the number of randomly sampled portfolios
#' 
#' @return plot, a histogram of the returns distribution
#' 
#' @examples
#' \dontrun{
#' plot <- plot_return_distribution(returns, portfolio.weight)
#' }


plot_return_distribution <- function(returns, points) {
  
  # Since we created the returns frame in order, we can select params by position
  even_weight_return <- returns[1,1]
  target_return <- returns[2,1]
  
  plot <- returns %>% 
    
    # Take random portfolio returns
    slice(3:n()) %>% 
    
    # Then plot a histogram of returns
    ggplot(aes(x=portfolio_return)) + 
    geom_histogram(bins = 20) + 
    labs(x = "Portfolio Return", y = "Portfolio Count") +
    
    # Add in vertical lines for each of the portfolio comparisons declared above
    geom_vline(xintercept = even_weight_return, linetype = "dotted") +
    geom_vline(xintercept = target_return, linetype = "twodash") +
    
    # Add in labels for those lines, making sure that they are always visible on the plot by adjusting positions relative to location
    annotate(geom = 'text', label = paste("Evenly Weighted Portfolio Return:", round(even_weight_return, 4)), x = even_weight_return, y = Inf, hjust = ifelse(even_weight_return < (.75*max(returns[['portfolio_return']])) + (.25*min(returns[['portfolio_return']])), -.02, 1.02), vjust = 1.5, size = 3) +
    annotate(geom = 'text', label = paste("Target Portfolio Return:", round(target_return, 4)), x = target_return, y = Inf, hjust = ifelse(target_return < (.75*max(returns[['portfolio_return']])) + (.25*min(returns[['portfolio_return']])), -.02, 1.03), vjust = 3, size = 3)
  
  return(plot)
  
}