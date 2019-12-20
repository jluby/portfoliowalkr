#' The Portfoliowalkr function
#' 
#' Given an asset universe \eqn{A} and a vector of baseline weights with 
#' which to calculate target portfolio characteristics \eqn{b}, 
#' \code{walkrportfolio} samples points from the intersection of 
#' \eqn{Ax = b} with the n-simplex (\eqn{\sum x = 1}, \eqn{x_i \ge 0}).
#' 
#' \code{walkrportfolio}, per its namesake, makes use of the \code{walkr}
#' package to sample randomly from the space of characteristic-matched
#' portfolios. \code{walkrportfolio} returns \code{points} vectors of
#' weights, which when applied to the universe A yields a portfolio with
#' the same characteristics as the target. 
#' 
#' 1) Hit-and-run is computationally less expensive and also 
#' guarantees uniformity asympotically with complexity of O(n^3)
#' points with respect to dimension n. However, in real practice, 
#' as dimensions ramp up, the mixing of hit-and-run is poor compared 
#' to Dikin. Thus, a lot of thinning would be needed as dimension
#' ramps up.
#' 
#' 2) Dikin Walk is a nearly uniform method known for its very strong 
#' mixing properties. However, each Dikin step is much more 
#' computationally expensive than hit-and-run, so it takes more time 
#' to sample every point. Thus, the "dikin" method
#' uses RcppEigen to speed up the core computationally 
#' expensive operations in the algorithm.
#' 
#' @param universe is the universe of stocks available for sampling, with dates, returns, weights, and any desired characteristics
#' @param match is the list of columns containing characteristics to be matched
#' @param portfolio.weight is the name of the column containing the weights of the target portfolio
#' @param ret.var is the name of the column containing percentage return over the time period
#' @param points is the number of points we want to sample
#' @param method is the MCMC sampling method. Please enter "hit-and-run" or "dikin"
#' @param chains is the number of chains run
#' @param thin every thin-th point is stored
#' @param burn the first burn points are deleted
#'        
#' @return A list of objects for assessing portfolio performance. $plot returns a histogram of 
#' random portfolio performances with target portfolio and universe performances demarcated for 
#' comparison. $summary returns a numeric description of these performances. $frame returns the universe
#' frame with matched weights attached. $returns is a frame containing each portfolios' 
#' return, including one evenly weighted within the universe for comparison. $explore is a list
#' of chains from the walkr output which can be input into the explore_walkr function to better
#' understand the convergence of the model.
#'   
#' @examples
#' match_list <- portfoliowalkr(universe = jan, match = c('sector', 'growth', 'size'), points = 100, method = "dikin")       
#' match_list$frame
#'                                                                                              
#' @export 
#' 

portfoliowalkr <- function(universe,
                           match,
                           portfolio.weight = "portfolio",
                           ret.var = "return",
                           points = 1000, 
                           method = "dikin",
                           chains = 1,
                           thin = 1,
                           burn = 0.5) {
  
  # Make sure weights and return variables are numeric
  for (i in c(portfolio.weight, ret.var)) {
    if (class(universe[[i]]) != "numeric") {
      stop("portfolio.weight and return variables must be numeric")
    }
  }
  
  # Make sure match variables are contained in the dataset
  for (i in match) {
    if (is.null(universe[i])) {
      stop("All matching variables must be contained in the dataset")
    }
  }
  
  # Make sure weights are greater than 0, not a problem if they don't sum to 1
  if(sum(universe[portfolio.weight] < 0) > 0) {
    stop("Assets may only have zero or positive weight")
  }
  
  ## portfolio.weight must have length 1.
  if(length(portfolio.weight) != 1) {
    stop("portfolio.weight needs to be provided")
  }
  
  # portfolio.weight must be a column
  if(is.null(universe[portfolio.weight])) {
    stop("portfolio.weight must be a column in the frame")
  }
  
  # If exposure is the min of any or max of any then remove from sample and figure out how to reattach
  # Create id for reattachment
  universe <- universe %>% 
    mutate(rowid = row_number())
  
  # For categorical, only keep if one of them has weight
  # For numeric, if mean value is at the edge of the distribution then 
  # filter down to only max or min values and remove variable from `match`
  # Else this function does nothing
  small_set <- make_small(universe, match, portfolio.weight)
  
  # Pull values from set for transposition (not really a necessary step)
  universe_small <- small_set$universe_small
  match <- small_set$match
  
  # Make sure all matched variables are either numeric or a factor
  #TODO fix this so you can move it up
  for (i in match) {
    if (class(universe_small[[i]]) != "factor" & class(universe_small[[i]]) != "numeric") {
      stop("All match variables must be either factor or numeric")
    }
  }

  # Transpose the `match` columns, for matching against b
  A <- transpose_a(universe_small, match)

  # Create characteristic stats to which we match our benchmarks
  b <- transpose_b(universe_small, match, portfolio.weight)

  # Run walkr to get random weights from the space of characteristic matches
  chain_list <- walkr(A = A, b = b, points = points, method = method, chains = chains, thin = thin, burn = burn, ret.format = "list")
  
  weights <- as.data.frame(chain_list)
  
  # Make sure the colnames are going to be easily interpretable later
  for (i in 1:ncol(weights)) {
    colnames(weights)[i] <- paste0("V", i)
  }
  
  # Bind weights to the small universe, will always be in the same order
  # Select from id's to last weight
  weighted_small <- bind_cols(universe_small, as.data.frame(weights)) %>% 
    select((length(universe)):(length(universe)+points))
  
  # Attach weights, by id, to the universe of stocks.
  weighted_universe <- left_join(universe, weighted_small, by = "rowid") %>%
    
    # Fill any NAs left over with 0's because if unfilled that means their weight should be 0
    # Return weights in same relative size as that provided by user
    mutate_at(vars((length(universe)+1):(length(universe)+points)), 
              ~replace_na(.,0) * sum(universe[portfolio.weight])) %>% 
    
    # Rename weight columns for cleanliness
    rename_at(vars((length(universe)+1):(length(universe)+points)), 
              ~str_replace(., "V", "weight"))  %>% 
    
    # Select out rowid
    select(-rowid)
  
  # Calculate returns of each portfolio for plotting and summary
  returns <- calc_returns(weighted_universe, universe, portfolio.weight, ret.var, points)
  
  # Make histogram of returns
  plot <- plot_return_distribution(returns, points)
  
  # Create list of summary statistics
  summary <- performance_summary(returns, points)
  
  # Save list of outputs
  full_list <- list('plot' = plot, 'summary' = summary, 'frame' = weighted_universe, 'returns' = returns, 'explore' = chain_list)
  
  return(full_list)
}

# Tests?
