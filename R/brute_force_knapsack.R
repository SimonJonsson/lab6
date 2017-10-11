
#' An implementation of the brute force solution to the knapsack problem
#' Where we list all possible combinations of a vector with size \{0,1\}\*n and then make it into
#' a matrix and check each row for best possible value. The matrix has the size 2^n rows and n columns.
#'
#' @param x data.frame with two vectors of the same size, v with values and w with weights
#' @param W an integer as the total weight of the knapsack
#' @param parallel boolean deciding if computation should be done in parallel or concurrent
#'
#' @export
brute_force_knapsack <- function(x, W, parallel = FALSE) {
  stopifnot(W > 0 &
              is.data.frame(x) &
              is.vector(x$v) &
              is.vector(x$w) &
              length(x$w) == length(x$v))
  n <- length(x[[1]])
  v <- x$v
  w <- x$w
  best <- replicate(n, 0)

  if (parallel) {
    # Checks each row of M if the weight is allowed
    res_vec <- parallel::mclapply(1:(2^n-1), function(x, w, v, W) {
      m <- intToBits(x)
      if (sum(w[m == 1]) <= W) {
        return(m)
      }
    }, w, v, W, mc.cores = parallel::detectCores())

    # Takes all the allowed rows and calculates which one has the best value
    lapply(Filter(Negate(is.null), res_vec), function(m) {
      if (sum(v[m == 1]) > sum(v[best == 1])) {
        best <<- m
      }
    })
  } else {
    # Goes through each row of M to find the optimal value
    lapply(1:(2^n-1), function(x) {
      m <- intToBits(x)
      if (sum(v[m == 1]) > sum(v[best == 1]) & sum(w[m == 1]) <= W) {
        best <<- m
      }
    })
  }

  res <- list(value = sum(v[best == 1]), elements = which(best == 1))
  return(res)
}
#system.time(brute_force_knapsack(x = knapsack_objects[1:20,], W = 3500, parallel=TRUE))
