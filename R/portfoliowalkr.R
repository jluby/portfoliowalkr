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
#' @param thin every thin-th point is stored
#' @param burn the first burn points are deleted
#'        
#' @return A matrix containing all of the points. Each column is a point sampled. 
#'   
#' @examples
#' ## 4D constraint
#' A <- matrix(c(2,0,1,3), ncol = 4)
#' b <- 0.5
#' sampled_points <- walkr(A = A, b = b, points = 100, method = "dikin")       
#' 
#' @importFrom walkr walkr         
#'                                                
#' @export 
#' 

portfoliowalkr <- function(universe,
                           match,
                           portfolio.weight = "portfolio",
                           ret.var = "return",
                           points = 1000, 
                           method = "dikin",
                           thin = 1,
                           burn = 0.5) {
  
  # Make sure all matched variables are either numeric or a factor
  for (i in match) {
    if (class(pull(universe[i])) != "factor" & class(pull(universe[i])) != "numeric") {
      stop("All match variables must be either factor or numeric")
    }
  }
  
  # Make sure weights and return variables are numeric
  for (i in c(portfolio.weight, ret.var)) {
    if (class(pull(universe[i])) != "numeric") {
      stop("portfolio.weight and return variables must be numeric")
    }
  }
  
  # Make a check for things having values - If every one is the same then ignore and throw warning
  
  # Make sure factors are unordered so that they can be one hot encoded
  universe <- universe %>% 
    mutate_if(is.factor, ~factor(.,ordered = FALSE))
  
  # Make sure weights are greater than 0
  if(sum(universe[portfolio.weight] < 0) > 0) {
    stop("Assets may only have zero or positive weight")
  }
  
  ## portfolio.weight must have length 1.
  if(length(portfolio.weight) != 1) {
    stop("portfolio.weight needs to be provided")
  }
  
  # portfolio.weight must be a column
  if(is.null(universe[portfolio.weight])) {
    stop("portfolio.weight must be a provided column")
  }
  
  # If exposure is the min of any or max of any then remove from sample and figure out how to reattach
  # Create id for reattachment
  universe <- universe %>% 
    mutate(id = row_number())
  
  # For categorical, only keep if one of them has weight
  # For numeric, if mean value is at the edge of the distribution then 
  # filter down to only max or min values and remove variable from `match`
  # Else this function does nothing
  small_set <- make_small(universe, match, portfolio.weight)
  
  universe_small <- small_set$universe_small
  match <- small_set$match

  # Transpose the `match` columns, for matching against b
  A <- transpose_a(universe_small, match)
  
  # Create characteristic stats to which we match our benchmarks
  b <- transpose_b(universe_small, match, portfolio.weight)
  
  weights <- walkr(A = A, b = b, points = points, method = method, thin = thin, burn = burn)
  
  weighted_small <- bind_cols(universe_small, as.data.frame(weights)) %>% 
    select((length(universe)):(length(universe)+points))
  
  weighted_universe <- left_join(universe, weighted_small, by = "id") %>%
    mutate_at(vars((length(universe)+1):(length(universe)+points)), ~replace_na(.,0)) %>% 
    select(-id)
  
  return(weighted_universe)
}

# Add a check that a given sample gets the same exposures